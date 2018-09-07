(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.dsl :as dsl]
            [bluebell.utils.debug :as dbg]
            [geex.java :as java]
            [clojure.reflect :as r]
            [geex.core :as core]
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
(spec/def ::context (spec/keys :req-un [::visibility ::static?]))
(spec/def ::accumulator (spec/keys :req-un [::extends
                                            ::implements
                                            ::methods
                                            ::variables]
                                   :opt-un [::name]))

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
    (update acc :variables assoc-new
            name
            {:name name
             :type (java/import-type-signature var-type)
             :context ctx})))

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
        prefix (java/str-to-java-identifier (:name variable))]
    (mapv
     (fn [i f]
       {:name (str prefix "_" i)
        :type f})
     (range (count flat))
     flat)))

(defn render-variable [var-spec]
  {:pre [(spec/valid? ::variable var-spec)]}
  (let [context (:context var-spec)
        ctx-spec [(static-str context)
                  (visibility-str context)]]
    (mapv
     (fn [x]
       [ctx-spec
        (java/seed-typename (:type x))
        (:name x)
        ";"])
     (expand-member-variable var-spec))))

(defn render-variables [acc]
  (mapv render-variable (-> acc :variables vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defn render-class-code [acc]
  {:pre [(accumulator? acc)]}
  (java/format-nested [(-> acc :context visibility-str)
                       "class " (:name acc)
                       (render-extends acc)
                       (render-implements acc) "{"
                       (render-variables acc)
                       "}"]))

(defn test-it []
  (let [cs (evaluate
            (class-spec
             Mummi 
                                        ;(extends java.lang.Integer)
                                        ;(implements java.lang.Double)

             (private
              (variable [java.lang.Double/TYPE
                         java.lang.Integer] a))
             
             ))
        src (render-class-code cs)]
    (println (str  "The source is\n" src))
    (java/janino-cook-and-load-object (:name cs) src)))

(comment
  



  )
