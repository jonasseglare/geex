(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.dsl :as dsl]
            [bluebell.utils.debug :as dbg]
            [geex.java :as java]
            [geex.core.exprmap :as exprmap]
            [clojure.reflect :as r]
            [geex.core :as core]
            [clojure.java.io :as io]
            [geex.core.jvm :as gjvm]
            [clojure.string :as cljstr]
            [bluebell.utils.specutils :as specutils]))


(declare public)


(spec/def ::classes (spec/coll-of class?))
(spec/def ::extends ::classes)
(spec/def ::implements ::classes)
(spec/def ::method any?)
(spec/def ::methods (spec/map-of ::name ::method))
(spec/def ::variables (spec/map-of ::name ::variable))
(spec/def ::name string?)
(spec/def ::type any?) ;; <-- any valid type signature
(spec/def ::variable (spec/keys :req-un [::name ::type ::context]))

(def visibilities #{:public :private :protected})
(spec/def ::visibility visibilities)
(spec/def ::static? boolean?)
(spec/def ::current-variable ::name)
(spec/def ::package ::name)
(spec/def ::output-prefix string?)
(spec/def ::context (spec/keys :req-un [::visibility ::static?]
                               :opt-un [::current-variable]))
(spec/def ::accumulator (spec/keys :req-un [::extends
                                            ::implements
                                            ::methods
                                            ::variables]
                                   :opt-un [::name
                                            ::package]))
(spec/def ::settings (spec/keys :req-un [::output-prefix]))

(def eval-dsl (dsl/dsl-evaluator {:accumulator-spec ::accumulator
                                  :context-spec ::context}))

(def context? (specutils/pred ::context))
(def accumulator? (specutils/pred ::accumulator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-context {:static? false
                    :visibility :public})
(def empty-accumulator
  {:context empty-context
   :extends []
   :implements []
   :methods {}
   :variables {}})

(defn class-spec-sub [name-str body]
  (fn [context acc]
    (eval-dsl context
              (assoc acc :name name-str)
              body)))

(defn with-visibility [v & body]
  {:pre [(spec/valid? ::visibility v)]}
  (fn [c a]
    (eval-dsl
     (assoc c :visibility v)
     a
     body)))

(defn assoc-new [dst key value]
  (if (contains? dst key)
    (throw (ex-info "Already exists"
                    {:key key}))
    (assoc dst key value)))

(defn variable-sub [var-type name body]
  (fn [ctx acc]
    (eval-dsl
     (assoc ctx :current-variable name)
     (update acc :variables assoc-new
             name
             {:name name
              :type (java/import-type-signature var-type)
              :context ctx})
     body)))

(defn static-str [context]
  {:pre [(spec/valid? ::context context)]}
  (if (:static? context) "static" ""))

(defn visibility-str [context]
  {:pre [(spec/valid? ::context context)]}
  (-> context :visibility name))

(defn render-classes [label k acc]
  {:pre [(keyword? k)
         (string? label)]}
  (let [e (k acc)]
    (if (empty? e)
      []
      [label (mapv r/typename e)])))

(def render-extends
  (partial render-classes "extends" :extends))

(def render-implements
  (partial render-classes "implements" :implements))

(defn expand-member-variable [variable]
  {:pre [(spec/valid? ::variable variable)]}
  (let [flat (core/flatten-expr (:type variable))
        prefix (:name variable)]
    (mapv
     (fn [i f]
       {:name (str prefix "_" i)
        :type f})
     (range (count flat))
     flat)))

(defn gen-setter [setter-name var-spec]
  (let [expansion (expand-member-variable var-spec)
        context (:context var-spec)
        input-var-name "input_value"
        tp (:type var-spec)
        input-var-type (gjvm/get-type-signature tp)
        fg (core/full-generate
            [{:platform :java}]
            (let [unpacked (java/unpack
                            tp
                            (core/bind-name input-var-type
                                            input-var-name))
                  flat-unpacked (core/flatten-expr unpacked)]
              (assert (= (count flat-unpacked)
                         (count expansion)))
              (doseq [[l r] (map vector expansion flat-unpacked)]
                (java/assign
                 (java/str-to-java-identifier (:name l))
                 r)))
            (java/make-void))]
    [(static-str context) "public void "
     (java/to-java-identifier setter-name) "("
     (r/typename input-var-type)
     (java/str-to-java-identifier input-var-name)
     ") {" (:result fg) "}"]))

(defn gen-getter [getter-name var-spec]
  (let [expansion (expand-member-variable var-spec)
        tp (:type var-spec)
        output-var-type (gjvm/get-type-signature tp)
        fg (core/full-generate
            [{:platform :java}]
            (core/return-value
             (core/populate-seeds
              tp
              (mapv (fn [e] (core/bind-name (:type e)
                                            (:name e)))
                    expansion))))]
    [(static-str (:context var-spec))
     "public " (r/typename output-var-type)
     (java/str-to-java-identifier getter-name) "() {"
     (:result fg)
     "}"]))

(defn render-variable [var-spec]
  {:pre [(spec/valid? ::variable var-spec)]}
  (let [context (:context var-spec)
        ctx-spec [(static-str context)
                  (visibility-str context)]
        expansion (expand-member-variable var-spec)]
    [(mapv
       (fn [x]
         [ctx-spec
          (java/seed-typename (:type x))
          (java/str-to-java-identifier (:name x))
          ";"])
       expansion)
     (if-let [setter-name (:setter var-spec)]
       (gen-setter setter-name var-spec)
       [])
     (if-let [getter-name (:getter var-spec)]
       (gen-getter getter-name var-spec)
       [])]))

(defn render-variables [acc]
  (mapv render-variable (-> acc :variables vals)))

(defn ensure-new-value [new-value]
  (fn [old-value]
    (assert (nil? old-value))
    new-value))

(defn setter-sub [setter-name]
  (fn [ctx acc]
    (if-let [v (:current-variable ctx)]
      (update-in acc [:variables v :setter]
                 (ensure-new-value setter-name))
      (throw (ex-info
              "setter can only be used inside a variable binding"
              {:setter-name setter-name})))))

(defn getter-sub [getter-name]
  (fn [ctx acc]
    (if-let [v (:current-variable ctx)]
      (update-in acc [:variables v :getter]
                 (ensure-new-value getter-name))
      (throw (ex-info "getter can only be used inside a variable binding")))))

(defn method-sub [method-name-str arglist body-fn]
  (fn [ctx acc]
    (update-in acc [:methods method-name-str]
               (ensure-new-value {:name method-name-str
                                  :args arglist
                                  :body-fn body-fn
                                  :context ctx}))))

(defn render-method [method-spec]
  (let [ctx (:context method-spec)
        fg (core/full-generate
            [{:platform :java}]
            (core/return-value
             (apply
              (:body-fn method-spec)
              (map java/to-binding (:args method-spec)))))]
    [(exprmap/get-static-code (:comp-state fg))
     (static-str ctx)
     (visibility-str ctx)
     (java/return-type-signature fg)
     (java/str-to-java-identifier (:name method-spec))
     "("
     (java/make-arg-list (:args method-spec))
     ") {"
     (:result fg)
     "}"]))

(defn render-methods [acc]
  (mapv render-method (-> acc :methods vals)))

(defn package-sub [package-name body]
  (fn [ctx acc]
    (eval-dsl ctx
              (assoc-new acc :package package-name)
              body)))

(defn get-output-filename [settings acc]
  {:pre [(spec/valid? ::settings settings)
         (spec/valid? ::accumulator acc)]}
  (let [prefix (:output-prefix settings)
        package-str (or (:package acc) "")
        class-name (:name acc)
        package-parts (cljstr/split package-str #"\.")]
    (assert class-name)
    (apply io/file (reduce into [prefix]
                           [package-parts
                            [(str class-name ".java")]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic settings {:output-prefix "src/java"})

;;;------- The DSL -------

(defmacro class-spec [name-symbol & body]
  {:pre [(symbol? name-symbol)]}
  `(class-spec-sub ~(str name-symbol) ~(vec body)))

(def evaluate (partial eval-dsl empty-context empty-accumulator))

(defn extends [& classes]
  (fn [ctx acc]
    (update acc :extends into classes)))

(defn implements [& classes]
  (fn [ctx acc]
    (update acc :implements into classes)))

(def private (partial with-visibility :private))
(def public (partial with-visibility :public))
(def protected (partial with-visibility :protected))

(defn static [& body]
  (fn [ctx acc]
    (eval-dsl (assoc ctx :static? true) acc body)))


(defmacro variable [var-type var-name & body]
  {:pre [(symbol? var-name)]}
  `(variable-sub ~var-type ~(str var-name) ~(vec body)))

(defmacro setter [setter-name]
  {:pre [(symbol? setter-name)]}
  `(setter-sub ~(str setter-name)))

(defmacro getter [getter-name]
  {:pre [(symbol? getter-name)]}
  `(getter-sub ~(str getter-name)))

(defmacro method [& args0]
  (let [args (java/parse-typed-defn-args args0)
        arglist (:arglist args)]
    `(method-sub ~(-> args :name str)
                 ~(-> arglist java/quote-args)
                 (fn [~@(map :name arglist)]
                   ~@(java/append-void-if-empty (:body args))))))

(defmacro package [package-sym & body]
  {:pre [(symbol? package-sym)]}
  `(package-sub ~(str package-sym) ~(vec body)))

;;;------- More -------

(defn disp [x]
  (println "x=" x)
  x)

(defn render-class-code [acc]
  {:pre [(accumulator? acc)]}
  (java/format-nested [(-> acc :context visibility-str)
                       "class " (:name acc)
                       (render-extends acc)
                       (render-implements acc) "{"
                       (render-variables acc)
                       (render-methods acc)
                       "}"]))

(defn instantiate-object [body]
  (let [cs (evaluate body)]
    (->> cs
         render-class-code
         (java/janino-cook-and-load-object (:name cs)))))

(defn instantiate-class [body]
  (let [cs (evaluate body)]
    (->> cs
         render-class-code
         (java/janino-cook-and-load-class (:name cs)))))

(defn save-class
  ([body]
   (save-class body settings))
  ([body settings]
   (let [acc (evaluate body)
         output-filename (get-output-filename settings acc)]
     (io/make-parents output-filename)
     (spit output-filename (render-class-code acc)))))


(defn test-it []
  (let [cs (evaluate
            (class-spec
             Mummi 
                                        ;(extends java.lang.Integer)
                                        ;(implements java.lang.Double)


             (public
              (method mummi [Double/TYPE x] [x x])
              
              (method katt2 [Double/TYPE x]
                      {:a x})
              )
             
             (private
              (variable [java.lang.Double/TYPE
                         java.lang.Integer] a
                        (setter setA)
                        (getter getA)))
             
             ))
        src (render-class-code cs)]
    (println (str  "The source is\n" src))
    (java/janino-cook-and-load-object (:name cs) src)))

(comment
  



  )
