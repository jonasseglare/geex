(ns geex.java

  "Generation of Java backed code and utilities for embed it."

  (:import [geex SeedParameters Mode
            StateSettings
            CodeMap CodeItem ISeed State]
           [java.io File])
  (:require [geex.java.defs :as jdefs]
            [geex.java.reflect :as jreflect]
            [clojure.java.io :as io]
            [geex.java.class :as gclass]
            [bluebell.utils.wip.java :refer [set-field]]
            [bluebell.utils.wip.debug :as debug]
            [geex.core.defs :as defs]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [clojure.pprint :as pp]
            [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.type :as etype]
            [geex.ebmd.type :as getype]
            [geex.core :as core]
            [bluebell.utils.wip.specutils :as specutils]
            [bluebell.utils.wip.core :as utils]
            [geex.core.seed :as sd]
            [bluebell.utils.wip.defmultiple :refer [defmultiple-extra]]
            [geex.core.jvm :as gjvm]
            [geex.core.stringutils :as su :refer [wrap-in-parens
                                                  nested-to-string]]
            [bluebell.utils.wip.tag.core :as tg]
            [geex.core.xplatform :as xp]
            [clojure.reflect :as r]
            [geex.core.datatypes :as dt]
            [clojure.string :as cljstr]
            [bluebell.utils.render-text :as render-text]
            [geex.core.seedtype :as seedtype]
            [bluebell.utils.wip.party.coll :as partycoll]
            [bluebell.utils.wip.timelog :as timelog]
            [geex.java.try-block :as try-block]
            [geex.core.utils :refer [partial-wrapping-args
                                     arity-partial
                                     environment
                                     merge-onto]]
            )
  (:refer-clojure :exclude [eval new])
  
  (:import [org.codehaus.janino SimpleCompiler]
           [com.google.googlejavaformat.java Formatter FormatterException]
           [com.google.googlejavaformat FormatterDiagnostic
            ]))




;; Lot's of interesting stuff going on here.
;; https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html

(def platform-tag [:platform :java])

(def ^:dynamic build-callbacks nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::method-directive #{:pure :static})
(spec/def ::method-directives (spec/* ::method-directive))

(spec/def ::call-method-args (spec/cat :directives ::method-directives
                                       :name string?
                                       :dst any?
                                       :args (spec/* any?)))


(def file? (partial instance? File))

(spec/def ::output-path (spec/or :file file?
                                      :string string?))

(spec/def ::settings (spec/keys :req-un [::output-path]))

(def settings? (partial spec/valid? ::settings))

(def default-settings {:output-path "src/java"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Declarations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare unpack)
(declare new)
(declare import-type-signature)
(declare make-void)
(declare visible-class?)
(declare stub-class?)
(declare ensure-anonymous-class-is-this)
(declare ensure-anonymous-object-is-this)
(declare ensure-visible)
(declare anonymous-stub-class?)
(declare seed-typename)
(declare unbox)
(declare typename)
(declare return-type-signature)
(declare box)
(declare j-nth)
(declare j-first)
(declare j-next)
(declare j-count)
(declare j-val-at)
(declare call-operator)
(declare str-to-java-identifier)
(declare to-java-identifier)
(declare call-method-sub)
(declare cast-seed)
(declare call-static-method-sub)
(declare call-operator-with-ret-type)
(declare append-void-if-empty)
(declare render-arg-list)
(declare call-method)
(declare cast-any-to-seed)
(declare call-static-pure-method)
(declare this-class)
(declare this-object)
(declare janino-cook-and-load-class)
(declare get-array-element)
(declare set-array-element)
(declare get-instance-var)
(declare set-instance-var)

(def ^:dynamic -this-class nil)
(def ^:dynamic -this-object nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- compile-cast [comp-state expr]
  (wrap-in-parens
   ["(" (typename (sd/datatype expr)) ")"
    (-> expr
        seed/access-compiled-deps
        :value)]))

(defn void? [cl]
  {:pre [(class? cl)]}
  (= Void/TYPE cl))

(defn void-like? [tp]
  {:pre [(or (nil? tp)
             (class? tp))]}
  (or (nil? tp)
      (void? tp)))

(defn conditionally [f condition arg]
  {:pre [(fn? f)]}
  (if condition
    (f arg)
    arg))

(def compile-void (constantly "/*void*/"))

;; The difference is that if src-seed is already a subtype of dst-seed, then no cast will take place.
(defn- unpack-to-seed [dst-seed src-seed]
  (assert (sd/seed? src-seed))
  (assert (sd/seed? dst-seed))
  (let [dst-type (seed/datatype dst-seed)]
    (if (isa? (seed/datatype src-seed) dst-type) src-seed
      (cast-seed dst-type src-seed))))

(defn- unpack-to-vector [dst-type src-seed]
  (mapv (fn [index dst-element-type]
          (unpack dst-element-type (j-nth src-seed (int index))))
        (range (count dst-type))
        dst-type))

(defn- unpack-to-seq [dst-type src-seed]
  (second
   (reduce
    (fn [[src-seq dst] element-type]
      [(unpack-to-seed (sd/typed-seed clojure.lang.ISeq)
                       (j-next src-seq))
       (conj dst (unpack element-type (j-first src-seq)))])
    [src-seed '()]
    dst-type)))

(defn- unpack-to-map [dst-type src-seed]
  (into {} (map (fn [[k v]]
                  [k (unpack v (j-val-at src-seed (cast-seed
                                                   java.lang.Object
                                                   (core/to-seed k))))])
                dst-type)))



(defn- make-marker [col]
  (str (apply str (take col (repeat " ")))
       "^ ERROR HERE!"))

(defn- point-at-location [source-code line-number column-number]
  (cljstr/join
   "\n"
   (utils/insert-at (cljstr/split-lines source-code)
                    line-number
                    [(make-marker
                      (dec column-number))])))

(defn- point-at-error [source-code location]
  {:pre [(string? source-code)
         (instance? org.codehaus.commons.compiler.Location
                    location)]}
  (if (nil? location)
    source-code
    (point-at-location source-code
                       (.getLineNumber location)
                       (.getColumnNumber location))))

(defn- point-at-diagnostic [source-code diagnostic]
  (point-at-location source-code
                     (.line diagnostic)
                     (.column diagnostic)))

;; Either we load it dynamically, or we load it from disk.


(defn- nil-is-not-supported [& args]
  (throw
   (ex-info
    "An dynamically typed nil is not supported on the java platform"
    {:args args})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Identifiers on Java
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- special-char-to-escaped [x]
  (case x
    \: "_c"
    \- "_d"
    \_ "__"
    \/ "_s"
    \. "_p"
    \? "_q"
    (str x)))


(defn- seed-or-class? [x]
  (or (sd/seed? x)
      (class? x)))

(defn- class-to-typed-seed [x]
  (if (class? x)
    (sd/typed-seed x)
    x))

(defn- eval-arg-type [arg]
  (update arg :type clojure.core/eval))

(defn- make-arg-decl [parsed-arg]
  (let [tp (:type parsed-arg)
        type-sig (gjvm/get-type-signature tp)
        java-typename (typename type-sig)]
    ["final"
     java-typename
     (to-java-identifier (:name parsed-arg))
     ]))

(defn- join-args2
  ([]
   nil)
  ([c0 c1]
   (if (nil? c0)
     c1
     (into [] [c0 [", "] c1]))))

(defn- join-args [args]
  (or (reduce join-args2 args) []))

(defn- find-member-info [cl member-name0]
  (assert (class? cl))
  (let [member-name (symbol member-name0)]
    (->> cl
         clojure.reflect/reflect
         :members
         (filter #(= (:name %) member-name)))))

(defn has-return-value? [x]
  {:pre [(seed/seed? x)]}
  (not (void-like? (seed/datatype x))))

(defn- compile-call-method [comp-state expr]
  (conditionally
   wrap-in-parens
   (has-return-value? expr)
   [(:obj (sd/access-compiled-deps expr))
    "."
    (.getData expr)
    (let [dp (sd/access-compiled-indexed-deps expr)]
      (wrap-in-parens (join-args dp)))]))

(defn- class-name-prefix [cl]
  (if (anonymous-stub-class? cl)
    []
    [(typename cl)
     "."]))

(defn- compile-call-static-method [comp-state expr]
  (let [data (.getData expr)
        cl (:class data)]
    (conditionally
     wrap-in-parens
     (has-return-value? expr)
     [(class-name-prefix cl)
      (:method-name data)
      (let [dp (sd/access-compiled-indexed-deps expr)]
        (wrap-in-parens (join-args dp)))])))

(defn- format-source [src]
  (try
    (.formatSource (Formatter.) src)
    (catch FormatterException e
      (println "Failed to format this:")
      (println (point-at-diagnostic src (-> e
                                            .diagnostics
                                            (.get 0))))
      (throw e))))

(defn format-nested [s]
  (try
    (format-source s)
    (catch Throwable e
      (println
       (format "The flattened string: \n\n%s\n\n" s))
      (throw e))))

(defn- preprocess-method-args [args0]
  (let [args (mapv core/to-seed args0)
        arg-types (into-array java.lang.Class (mapv sd/datatype args))]
    (utils/map-of args arg-types)))

(defn- compile-operator-call [comp-state expr]
  (let [args (sd/access-compiled-indexed-deps expr)
        op (.getData expr)]
    (wrap-in-parens
     (if (= 1 (count args))

       ;; Prefix
       [op
        (first args)]

       ;; Infix
       (reduce into
               [(first args)]
               [(map (fn [arg]
                       [op arg])
                     (rest args))])))))

;;;;;;;;;;;;;;;;;;;; keywords

(defn- render-var-init [tp name val]
  [tp " " name " = " val ";"])

(defn- bind-statically [key comp-state binding-type binding-name binding-value]
  (core/add-top-code
   comp-state
   key
   ["static "
    (render-var-init
     binding-type
     binding-name
     binding-value)])
  binding-name)

(defn- escape-char [x]
  (or (char-escape-string x) x))

(defn- java-string-literal [s]
  (str "\"" (apply str (map escape-char s)) "\""))

(defn- java-char-literal [c]
  (str "'" (escape-char c) "'"))

(defn- compile-interned [comp-state expr]
  (let [data (sd/access-seed-data expr)
        kwd (:value data)
        tp (:type data)]
    (bind-statically
     [::interned kwd]
     comp-state
     (seed-typename expr)
     (str-to-java-identifier
      (str "INTERNED_" (str tp "_" kwd)))
     [(str "clojure.lang." tp ".intern(")
      (let [kwdns (namespace kwd)]
        (if (nil? kwdns)
          []
          [(java-string-literal kwdns)
           ", "]))
      (java-string-literal (name kwd)) ")"])))

(defn- compile-string [comp-state expr]
  (java-string-literal (sd/access-seed-data expr)))

(defn- compile-char [comp-state expr]
  (java-char-literal (sd/access-seed-data expr)))

(defn- make-seq-expr [args]
  ["clojure.lang.PersistentList.EMPTY"
   (mapv (fn [arg]
           [".cons((java.lang.Object)(" arg "))"])
         (reverse args))])

(defn- object-args [args]
  (or (join-args
       (map (fn [arg]
              ["(java.lang.Object)(" arg ")"]) args))
      []))

(defn- make-vec-expr [args]
  ["clojure.lang.PersistentVector.create(new java.lang.Object[]{"
   (object-args args)
   "})"])

(defn- make-map-expr [args]
  ["clojure.lang.PersistentHashMap.create("
   (object-args args)
   ")"])

(defn- make-set-expr [args]
  ["clojure.lang.PersistentHashSet.create("
   (object-args args)
   ")"])

(defn- compile-seq [comp-state args]
  (make-seq-expr args))

(defn- compile-vec [comp-state args]
  (make-vec-expr args))

(defn- compile-map [comp-state args]
  (make-map-expr args))

(defn- compile-set [comp-state args]
  (make-set-expr args))

(defn- compile-array-from-size [comp-state expr]
  (wrap-in-parens
   ["new " (-> expr
               seed/access-seed-data
               :component-class
               typename) "["
    (-> expr seed/access-compiled-deps :size) "]"]))

(defn- compile-set-array [state expr]
  (let [deps (seed/access-compiled-deps expr)]
    [(:dst deps) "[" (:index deps) "] = " (:value deps)]))

(defn- compile-get-array [state expr]
  (let [deps (seed/access-compiled-deps expr)]
    (wrap-in-parens [(:src deps) "[" (:index deps) "]"])))

(defn compile-array-length [state expr]
  (let [deps (seed/access-compiled-deps expr)]
    (wrap-in-parens [(:src deps) ".length"])))

(defn- render-if [condition true-branch false-branch]
  ["if (" condition ") {"
   true-branch
   "} else {"
   false-branch
   "}"])

(def ^:private var-name-java-sym (comp to-java-identifier
                             :name
                             :var))

(defn- bind-java-identifier [sd]
  {:pre [(core/seed? sd)]}
  (-> sd
      .getData
      to-java-identifier))

(defn- compile-assign [comp-state expr]
  (let [v (-> expr seed/access-compiled-deps
              :value)]
    [(.getData expr) " = " v ";"]))

(defn- compile-recur [state expr]
  "continue")

(defn- compile-loop2 [state expr]
  (let [deps (.getMap (.deps expr))
        body  (-> deps :body)]
    ["while (true) {"
     (seed/compilation-result body)
     "break;}"]))

#_(defn compile-local-var-section [state ^ISeed sd]
  (let [bindings (map local-var-binding (.getData sd))
        compiled-deps (seed/access-compiled-deps sd)]
    `(let ~(reduce into [] bindings)
       ~(:result compiled-deps))))

(defn- default-expr-for-type [x]
  (when (not (class? x))
    (throw (ex-info "Not a class"
                    {:x x})))
  (cond
    (= Float/TYPE x) "0.0f"
    (= Double/TYPE x) "0.0"
    (or (= Integer/TYPE x)
        (= Long/TYPE x)
        (= Short/TYPE x)
        (= Character/TYPE x)) "0"
    (= Boolean/TYPE x) "false"
    :default "null"))

(defn local-var-binding [lvar]
  (let [sym (xp/call :local-var-sym (.getIndex lvar))
        _ (assert (string? sym))
        java-type (-> lvar .getType .get)
        init-value (default-expr-for-type java-type)]
    (if (class? java-type)
      [(typename java-type) sym " = "
       init-value ";"]
      (throw (ex-info "Not a Java class"
                      {:java-type java-type
                       :lvar lvar})))))

(defn- compile-local-var-section [^State state
                                  ^ISeed expr]
  (let [bindings (mapv local-var-binding (.getData expr))
        deps (seed/access-compiled-deps expr)]
    [bindings
     (:result deps)]))

(defn- to-string [x]
  (if (seed/seed? x)
    (let [t (seed/datatype x)]
      (if (dt/boxed-type? t)
        (x 'toString)
        (call-method :static "valueOf" String x)))
    (str x)))

(defn- strcat2 [a b]
  (call-method "concat" a b))

(defn- concatenate-strings [strings]
  (reduce strcat2 (new String) strings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Basic platform operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cmp-operator [op]
  (arity-partial
   call-operator-with-ret-type
   Boolean/TYPE
   op
   [:left :right]))

(defn- get-method-with-hint [cl method-name arg-types]
  (try
                                        ;(.getMethod cl method-name arg-types)
    (jreflect/get-matching-method cl method-name (vec arg-types))
    (catch NoSuchMethodException e
      (when (stub-class? cl)
        (println
         (format
          "HINT: Did you forget to declare the return type for the method '%s', and is it visible?" method-name)))
      (throw e))))

(defn- call-static-method-sub [info cl args0]
  (ensure-visible cl)
  (ensure-anonymous-class-is-this cl)
  (let [method-name (:name info)
        {:keys [args arg-types]} (preprocess-method-args args0)
        method (get-method-with-hint cl method-name arg-types)
        rettype (.getReturnType method)]
    (core/make-dynamic-seed
     description (str "call static method "
                      method-name)
     type rettype
     data {:class cl
           :method-name method-name}
     mode (if (:dirty? info)
            Mode/SideEffectful
            Mode/Pure)
     hasValue (not= Void/TYPE rettype)
     rawDeps (core/to-indexed-map args)
     compiler compile-call-static-method)))

(defn- make-method-info [parsed-method-args]
  (let [dirs (:directives parsed-method-args)]
    (merge
     {:dirty? (not (contains? dirs :pure))
      :name (:name parsed-method-args)})))

(defn- compile-call-constructor [state sd]
  (let [deps (seed/access-compiled-indexed-deps sd)]
    (wrap-in-parens
     ["new"
      (.getData sd)
      (let [dp (sd/access-compiled-indexed-deps sd)]
        (wrap-in-parens (join-args dp)))])))

(defn- call-constructor-seed [cl args]
  (let [class-name (typename cl)]
    (core/make-dynamic-seed
     compiler compile-call-constructor
     description "call constructor"
     data class-name
     rawDeps (core/to-indexed-map args)
     mode Mode/SideEffectful
     type cl)))

(defn- call-method-sub [info obj0 args0]
  (let [method-name (:name info)
        obj (core/to-seed obj0)
        {:keys [args arg-types]} (preprocess-method-args args0)
        cl (ensure-visible (sd/datatype obj))
        _ (ensure-anonymous-object-is-this obj)
        method (get-method-with-hint cl method-name arg-types)
        rettype (.getReturnType method)]
    (core/make-dynamic-seed
     compiler compile-call-method
     description "call method"
     type rettype
     hasValue (not= Void/TYPE rettype)
     rawDeps (merge {:obj obj}
                    (core/to-indexed-map args))
     mode (cond
            ;;;(void? rettype) Mode/Statement
            (:dirty? info) Mode/SideEffectful
            :default Mode/Pure)
     data method-name)))

(defn- call-break []
  (core/make-dynamic-seed
   description "Break"
   mode Mode/SideEffectful
   hasValue false
   compiler (constantly "break;")))

(defn- this-seed [cl]
  (core/make-dynamic-seed
   type cl
   description "this"
   mode Mode/Pure
   bind false
   compiler (constantly "this")))

(defn- throw-error [msg]
  (core/make-dynamic-seed
   description "Crash"
   hasValue false
   mode Mode/SideEffectful
   compiler (constantly
             (str "throw new RuntimeException("
                  (java-string-literal msg)
                  ");"))))

(defn- nothing-seed [state]
  (core/make-dynamic-seed
   description "Nothing"
   mode Mode/Pure
   hasValue false
   type Void/TYPE
   compiler (constantly [])))


(defn- format-nested-show-error [code]
  (try
    (format-nested code)
    (catch Throwable e
      (println "The input code")
      (pp/pprint code)
      (throw e))))

(defn- make-call-operator-seed
  [ret-type operator args]
  (core/make-dynamic-seed
   description (str "call operator " operator)
   type ret-type
   rawDeps (core/to-indexed-map args)
   data operator
   mode Mode/Pure
   compiler compile-operator-call))

(defn- parse-method-args
  [method-args]
  (update (specutils/force-conform
           ::call-method-args method-args)
          :directives set))

(defn- collection-op
  [name]
  (fn [src]
    (call-method
     :static :pure
     name
     clojure.lang.RT
     (cast-any-to-seed java.lang.Object src))))

(defn- pure-static-methods [cl names]
    (into {}
          (map
           (fn [sp]
             (let [[key name arg-count] sp]
               [(keyword name)
                (partial call-static-pure-method name cl)]))
           names)))
  
(defn- java-math-fns [names]
  (pure-static-methods java.lang.Math names))

(defn- numeric-class-method [method-name]
  (fn [x0]
    (let [x (core/wrap x0)
          primitive-cl (seed/datatype x)
          cl (or (get dt/unboxed-to-boxed-map primitive-cl)
                 primitive-cl)]
      (call-static-pure-method method-name cl x))))



(def stub-tag "GEEX_CLASS_STUB")

(defn- decorate-class-stub-name [index class-def]
  (update class-def :name
          (fn [x] (str (or x "")
                       stub-tag
                       "_"
                       index))))

(defn- stub-visibility [x all-public?]
  (if all-public?
    "public"
    (-> x
        gclass/visibility
        gclass/visibility-str)))

(defn- make-method-arg-list [m]
  (let [arg-types (:arg-types m)
        arg-count (count arg-types)
        arg-names (mapv (fn [x]
                          (format "arg%02d" x))
                        (range arg-count))
        arg-list (mapv (fn [arg-name arg-type]
                         {:name arg-name
                          :type (import-type-signature
                                 arg-type)})
                       arg-names
                       arg-types)]
    arg-list))

(defn- make-stub-variable [all-public? v]
  [(stub-visibility v all-public?)
   (if (gclass/static? v) "static" "")
   (typename (gjvm/get-type-signature (:type v)))
   (:name v)
   ";"])

(defn- make-stub-method [all-public? v]
  (if (contains? v :ret)
    (let [ret (gjvm/get-type-signature (:ret v))]
      [(stub-visibility v all-public?)
       (if (gclass/static? v) "static" "")
       (typename ret)
       (:name v)
       "("
       (-> v make-method-arg-list render-arg-list)
       ") {"
       "return " (default-expr-for-type ret) ";"
       "}"])
    []))

(defn- make-stub-constructor [all-public? class-name c]
  [(stub-visibility c all-public?)
   class-name
   "("
   (-> c make-method-arg-list render-arg-list)
   ") {}"])

(defn- make-stub-class-code [class-def all-public?]
  [(if (contains? class-def :package)
     ["package " (:package class-def) ";"]
     [])
   ["public class " (:name class-def)
    (gclass/extends-code class-def)
    (gclass/implements-code class-def)
    " {"
    (mapv (partial make-stub-variable all-public?)
          (:variables class-def))
    (mapv (partial make-stub-method all-public?)
          (:methods class-def))
    (mapv (partial make-stub-constructor
                   all-public? (:name class-def))
          (:constructors class-def))
    "}"]])

(defn make-stub-class [class-def unique-index all-public?]
  (let [class-def (decorate-class-stub-name
                   unique-index
                   (gclass/validate-class-def class-def))
        class-name (gclass/full-java-class-name class-def)
        code (make-stub-class-code class-def all-public?)
        flat-code (nested-to-string code)]
    (janino-cook-and-load-class class-name flat-code)))

(defn- typename-stub-class-name [raw-name]
  (if-let [i (cljstr/index-of raw-name stub-tag)]
    (subs raw-name 0 i)))

(defn stub-class? [x]
  (and (class? x)
       (not (nil? (typename-stub-class-name
                   (r/typename x))))))

(defn- with-register-class
  [class-def body-fn]
  {:pre [(gclass/valid? class-def)
         (fn? body-fn)]}
  (let [private-stub (make-stub-class
                      class-def
                      (.generateSymbolIndex (core/get-state))
                      true)
        public-stub (make-stub-class
                     class-def
                     (.generateSymbolIndex (core/get-state))
                     false)]
    (core/with-modified-state-var
      "visible-classes"
      (fn [m] (into
               (or m #{})
               (mapv r/typename [private-stub
                                 public-stub])))
      (body-fn (merge
                class-def
                {:public-stub public-stub
                 :private-stub private-stub})))))


(defn visible-class? [x]
  (and (class? x)
       (or (not (stub-class? x))
           (contains? (set (core/get-state-var "visible-classes"))
                      (r/typename x)))))

(defn- ensure-visible [x]
  (when (not (visible-class? x))
    (throw (ex-info "Trying to use class that is no longer visible in this scope"
                    {:class x})))
  x)

(defn- anonymous-stub-name? [stub-name]
  (and
   (not (nil? stub-name))
   (or (cljstr/ends-with? stub-name ".")
       (empty? stub-name))))

(defn- anonymous-stub-class? [x]
  (and (class? x)
       (anonymous-stub-name?
        (typename-stub-class-name
         (r/typename x)))))

(defn- ensure-anonymous-class-is-this [x]
  {:pre [(class? x)]}
  (when (and (anonymous-stub-class? x)
             (not= x (this-class)))
    (throw (ex-info "Trying to use anonymous class that is not this"
                    {:class x
                     :this (this-class)}))))

(defn- ensure-anonymous-object-is-this [x]
  {:pre [(seed/seed? x)]}
  (when (and (anonymous-stub-class? (seed/datatype x))
             (not= x (this-object)))
    (throw (ex-info "Trying to use anonymous class that is not this"
                    {:object x
                     :this (this-object)}))))
(defn- config-actual-type [vdef]
  {:pre [(spec/valid? ::gclass/variable vdef)]}
  (assoc vdef :actual-type
         (gjvm/get-type-signature (:type vdef))))

(defn- static-tag-str [vmdef]
  (if (gclass/static? vmdef)
    "static"
    ""))

(defn- visibility-tag-str [vmdef]
  (-> vmdef
      gclass/visibility
      gclass/visibility-str))

(defn- compile-member-variable [state expr]
  (let [vdef (.getData expr)
        deps (seed/access-compiled-deps expr)
        tp (:actual-type vdef)]
    [(static-tag-str vdef)
     (visibility-tag-str vdef)
     (typename tp)
     (:name vdef)
     (if (contains? deps :init)
       [" = " (:init deps)]
       [])
     ";"]))

(defn- make-variable-seed [class-def v]
  (let [v (config-actual-type v)]
    (core/make-dynamic-seed
     (core/get-state)
     description "member variable"
     data v
     hasValue false
     mode Mode/Code
     rawDeps (if (contains? v :init)
               {:init (cast-any-to-seed
                       (:actual-type v)
                       (core/wrap (:init v)))}
               {})
     compiler compile-member-variable
     )))

(defn- compile-method [state expr]
  (let [deps (seed/access-compiled-deps expr)
        data (.getData expr)
        method (:method data)
        class-def (:class-def data)
        ret-type (:return-type data)
        arg-list (render-arg-list (:arg-list data))
        ret-type-sig (-> ret-type
                         gjvm/get-type-signature
                         typename)]
    [(static-tag-str method)
     (visibility-tag-str method)
     ret-type-sig
     (:name method)
     "(" arg-list ")"
     "{"
     (:body deps)
     "}"]))

(defn- to-binding [quoted-arg]
  "Internal function: Used when importing the arguments to a method."
  (let [tp (:type quoted-arg)
        t (gjvm/get-type-signature tp)]
    ;;; TODO: Get the type, depending on what...
    (unpack
     
     ;; The actual type used by us:
     tp 

     ;; A seed holding the raw runtime value
     (core/bind-name t (:name quoted-arg)))))

(defn- make-method-seed [class-def m]
  {:pre [(contains? m :fn)
         (gclass/has-stubs? class-def)]}
  (binding [-this-class (:private-stub class-def)
            -this-object (if (gclass/static? m)
                           -this-object
                           (this-seed
                            (:private-stub
                             class-def)))]
    (let [arg-list (make-method-arg-list m)
          f (:fn m)
          result (do
                   (core/dont-list!
                    (core/with-local-var-section
                      (let [bds
                            (into [ ;; Only provide a this-argument for named classes.
                                   (if (gclass/named? class-def)
                                     (if (gclass/static? m)
                                       -this-class
                                       -this-object))]
                                  (mapv to-binding arg-list))]
                        
                        (core/return-value (apply f bds))))))
          raw-type (seed/datatype result)
          inferred-type (gjvm/get-type-signature raw-type)
          ret (if (contains? m :ret)
                (gjvm/get-type-signature (:ret m)))]
      (when (and ret
                 (not= ret inferred-type))
        (throw (ex-info "Return type mismatch"
                        {:method m
                         :declared-return-type ret
                         :inferred-return-type inferred-type})))
      (core/make-dynamic-seed
       (core/get-state)
       description "method"
       hasValue false
       mode Mode/Code
       rawDeps {:body result}
       data {:class-def class-def
             :method m
             :return-type inferred-type
             :arg-list arg-list}
       compiler compile-method))))

(defn- compile-constructor [state expr]
  (let [deps (seed/access-compiled-deps expr)
        data (.getData expr)
        method (:method data)
        class-def (:class-def data)
        arg-list (render-arg-list (:arg-list data))
        body (:body deps)]
    [(visibility-tag-str method)
     (:name class-def)
     "(" arg-list ")"
     "{"
     body
     "}"]))

(defn- make-constructor [class-def m]
  {:pre [(contains? m :fn)
         (gclass/has-stubs? class-def)
         (gclass/named? class-def)]}
  (binding [-this-class (:private-stub class-def)
            -this-object (this-seed
                          (:private-stub
                           class-def))]
    (let [arg-list (make-method-arg-list m)
          f (:fn m)
          bds (into [-this-object]
                    (mapv to-binding arg-list))
          result (core/dont-list!
                   (core/with-local-var-section
                     (do (apply f bds)
                         (make-void))))]
      (core/make-dynamic-seed
       (core/get-state)
       description "constructor"
       mode Mode/Code
       hasValue false
       rawDeps {:body result}
       data {:class-def class-def
             :method m
             :arg-list arg-list}
       compiler compile-constructor))))

(defn- make-general-method-seed [class-def m]
  (if (gclass/abstract-method? m)
    []
    (make-method-seed class-def m)))

(defn- compile-anonymous-instance [state expr]
  (let [deps (seed/access-compiled-deps expr)
        cdef (.getData expr)]
    ["new " (-> cdef :super r/typename) "() {"
     (:scope deps)
     "}"]))

(defn- class-or-interface-str [class-def]
  (if (gclass/interface? class-def)
    "interface"
    "class"))

(defn- compile-local-class [state expr]
  (let [deps (seed/access-compiled-deps expr)
        class-def (.getData expr)]
    [(class-or-interface-str class-def) (:name class-def)
     (gclass/extends-code class-def)
     (gclass/implements-code class-def)
     "{"
     (:scope deps)
     "}"]))

(defn- anonymous-instance-seed [class-def scope]
  (core/make-dynamic-seed
   (core/get-state)
   description "anonymous object"
   rawDeps {:scope scope}
   mode Mode/SideEffectful
   hasValue true
   data class-def
   bind true
   type (:super class-def)
   compiler compile-anonymous-instance))

(defn- assign
  "Internal function:"
  [dst-var-name src]
  {:pre [(string? dst-var-name)]}
  (core/make-dynamic-seed
   description "assign"
   rawDeps {:value src}
   mode Mode/SideEffectful
   hasValue false
   data dst-var-name
   compiler compile-assign))


(defn- return-type-signature
  "Interal function: Given the output fg of full-generate, get the typename of the return value."
  [fg]
  (-> fg
      :expr
      gjvm/get-type-signature
      typename))

(defn- append-void-if-empty [x]
  "Internal function: Used when generating typed-defn"
  {:pre [(or (sequential? x)
             (nil? x))]}
  (if (empty? x)
    `((make-void))
    x))

(defn- render-arg-list
  "Internal function: Used to generate code for function arglist."
  [parsed-args]
  {:pre [(jdefs/parsed-typed-arguments? parsed-args)]}
  (or (reduce join-args2 (mapv make-arg-decl parsed-args)) []))

(defn list-class-items [f class-def k]
  (doseq [x (get class-def k)]
    (core/list! (f class-def x))))

(defn- expand-class-body [fl? class-def]
  {:pre [(gclass/valid? class-def)]}
  (core/open-scope!)
  (list-class-items make-variable-seed class-def :variables)
  (list-class-items make-general-method-seed class-def :methods)
  (list-class-items make-constructor class-def :constructors)
  (core/wrap ::defs/nothing)
  (core/close-scope!))

(defn- let-class-sub [args body]
  (if (empty? args)
    `(do ~@body)
    (let [[f & r] args]
      `(with-local-class
         ~(:class-def f)
         (fn [~(:symbol f)]
           ~(let-class-sub r body))))))

(defn- compile-class-definition [state expr]
  (let [deps (seed/access-compiled-deps expr)
        body (:body deps)
        data (.getData expr)
        class-def (:class-def data)
        top? (:top? data)]
    [(visibility-tag-str class-def)
     (static-tag-str class-def)
     (class-or-interface-str class-def)
     (:name class-def)
     (gclass/extends-code class-def)
     (gclass/implements-code class-def)
     "{"
     "/* Various definitions */"
     (core/get-top-code state)
     body
     "}"]))

(defn- defined-class-seed [top? class-def body]
  (let [state (core/get-state)]
    (core/make-dynamic-seed
     state
     description "Class definition"
     rawDeps {:body body}
     data {:class-def class-def
           :top? top?}
     mode Mode/Code
     hasValue false
     compiler compile-class-definition)))

(defn- define-class-sub [top? class-def]
  (let [class-def (gclass/validate-class-def class-def)]
    (with-register-class
      class-def
      (fn [class-def]
        {:pre [(gclass/has-stubs? class-def)]}
        (assert (gclass/named? class-def))
        (defined-class-seed
          top?
          class-def
          (expand-class-body (not top?) class-def))))))

(defn- define-top-class [class-def]
  (define-class-sub true class-def))

(defn- local-class-seed [class-def scope]
  {:pre [(gclass/valid? class-def)
         (gclass/named? class-def)]}
  (core/list!
   (core/make-dynamic-seed
    (core/get-state)
    description "local class"
    rawDeps {:scope scope}
    mode Mode/Code
    hasValue false
    data class-def
    type (:super class-def)
    compiler compile-local-class)))

(defn- class-name-to-path [class-name settings]
  {:pre [(string? class-name)
         (settings? settings)]}
  (let [parts (cljstr/split class-name #"\.")
        dirs (butlast parts)
        filename (str (last parts) ".java")
        parts (reduce into [(:output-path settings)]
                      [dirs [filename]])]
    (apply io/file parts)))

(defn- prepare-object [dst-object]
  (let [dst-object (core/wrap dst-object)
        dst-type (seed/datatype dst-object)]
    (when (not (class? dst-type))
      (throw (ex-info "Not an object"
                      {:dst-object dst-object})))
    dst-object))

(defn- nested-to-string-top [s]
  (try
    (nested-to-string s)
    (catch Exception e
      (println "Failed to compile this: " s)
      (throw e))))


;;;; case


(spec/def ::case-args (spec/cat :key any?
                                :cases (spec/* (spec/cat :value any?
                                                :code any?))
                                :default any?))

(defn- render-primitive [x]
  (cond
    (char? x) (java-char-literal x)
    (or (number? x)
        (boolean? x)) (str x)
    
    :default (throw (ex-info "Cannot render this as primitive"
                             {:x x}))))

(defn- render-case [keys deps]
  (let [codes (mapv (fn [i] (get deps i))
                    (range (count keys)))]
    ["switch (" (:input deps) ") {"
     (mapv (fn [k code]
             ["case " (render-primitive k) ": {" code " break;}"])
           keys codes)
     "default: {"
     (:default deps)
     "break;}}"]))

(defn compile-case [state expr]
  (let [deps (seed/access-compiled-deps expr)]
    (render-case (.getData expr) deps)))

(defn case-sub [input cases default]
  (let [ks (mapv first cases)
        code (mapv second cases)
        tp (seed/datatype input)]

    (cond
      (or (= tp Integer/TYPE)
          (= tp Long/TYPE)
          (= tp Short/TYPE)
          (= tp Byte/TYPE)) (assert (every? int? ks)
                                    "Every case must be an integer")

      (= tp Character/TYPE) (assert (every? char? ks)
                                    "Every case must be a character")

      (= tp Boolean/TYPE) (assert (every? boolean? ks)
                                  "Every case must be a boolean")

      :default (throw (ex-info
                       "Type not supported as dispatch value in case"
                       {:type tp})))
    
    (core/make-seed!
     (doto (SeedParameters.)
       (set-field description "Case")
       (set-field mode Mode/SideEffectful)
       (set-field hasValue false)
       (set-field compiler compile-case)
       (set-field data ks)
       (set-field rawDeps (merge {:input input
                                  :default default}
                                 (zipmap
                                  (range)
                                  code)))))))

(defn array-call-access [this args]
  (case (count args)
    1 (get-array-element this (first args))
    2 (set-array-element this (first args) (second args))
    3 (throw (ex-info "Trying to access array with bad arguments"
                      {:this this
                       :args args}))))

(defn object-call-access [this args]
  (let [op (first args)]
    (cond
      (keyword? op)
      (case (count args)
        1 (this 'valAt op)
        2 ((cast-seed clojure.lang.Associative this)
           'assoc
           (cast-any-to-seed Object op)
           (cast-any-to-seed Object (last args)))
        (throw (ex-info "Cannot access map-like interface with these args"
                        {:args args})))

      (symbol? op)
      (apply call-method (into [(name op) this] (rest args)))

      (string? op)
      (let [field-name op]
        (case (count args)
          1 (get-instance-var this field-name)
          2 (set-instance-var this field-name (second args))
          (throw (ex-info "Trying to access field with bad arguments"
                          {:this this
                           :args args}))))

      :default (throw (ex-info "Call operation not supported"
                               {:this this
                                :args args})))))

(defn- seed-call-access [this & args]
  (let [st (seed/datatype this)]
    (cond
      (dt/array-class? st)
      (array-call-access this args)

      (empty? args) (throw (ex-info "Cannot call without arguments"
                                    {:this this}))

      :default (object-call-access this args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cast-to-int [x]
  (cast-any-to-seed Integer/TYPE x))

(defn import-type-signature
  "Internal function: Used when parsing the type specification of a function."
  [x]
  (second
   (core/flat-seeds-traverse
    getype/resolve-type
    x
    (comp sd/strip-seed
          sd/typed-seed
          getype/resolve-type))))

(defn this-class
  "Get this class if inside a method"
  []
  (if (nil? -this-class)
    (throw (ex-info "No this-class" {}))
    -this-class))

(defn this-object
  "Get this object if inside a method"
  []
  (if (nil? -this-object)
    (throw (ex-info "No this-object" {}))
    -this-object))

(defn make-void []
  "Creates a seed representing void"
  (core/make-dynamic-seed
   description "void"
   mode Mode/Pure
   type Void/TYPE
   bind false
   hasValue false
   compiler compile-void))




(defn str-to-java-identifier
  "Internal function: Used in code generation to produce a string representing a valid Java identifier."
  [& args]
  (->> args
       (cljstr/join "_")
       vec
       (mapv special-char-to-escaped)
       (apply str)))

(ebmd/declare-poly to-java-identifier)

(ebmd/def-poly to-java-identifier [etype/symbol x]
  (str-to-java-identifier (name x)))

(ebmd/def-poly to-java-identifier [etype/string x]
  (str-to-java-identifier x))


(defn- parse-typed-defn-args
  "Internal function: Parses the input to typed-defn macro."
  [args0]
  {:post [(jdefs/parsed-defn-args? %)]}
  (specutils/force-conform ::jdefs/defn-args args0))

(defonce the-compiler (SimpleCompiler.))

(defn janino-cook-and-load-class
  "Given a class-name and source code of that class, compile the code and load the class dynamically."
  ([class-name source-code]
   (janino-cook-and-load-class
    the-compiler class-name source-code))
  ([sc class-name source-code]
   (try
     (.cook sc source-code)
     (.loadClass (.getClassLoader sc) class-name)
     (catch org.codehaus.commons.compiler.CompileException e
       (let [location (.getLocation e)
             marked-source-code (if (nil? location)
                                  "(no location to point at)"
                                  (point-at-error source-code location))]
         (println marked-source-code)
         (throw (ex-info "Failed to compile code"
                         {:code marked-source-code
                          :location location
                          :exception e})))))))

(defn typename [x]
  (cond
    (class? x)
    (let [tn (r/typename x)]
      (if-let [stub-name (typename-stub-class-name tn)]
        (if (anonymous-stub-name? stub-name)
          (throw (ex-info
                  "Trying to get typename of anonymous class"
                  {:class x}))
          stub-name)
        tn))
    
    ;(spec/valid? ::named-class-type x) (second x)
    :default (throw (ex-info "Cannot take typename of this value"
                             {:value x}))))

(defn janino-cook-and-load-object
  "Given a class name and source code, compile the class, load the class and instantiate an object."
  [class-name source-code]
  (.newInstance (janino-cook-and-load-class
                 class-name
                 source-code)))

(defn unpack
  "Imports incoming dynamic data to data of nested seeds when importing arguments."
  [dst-type src-seed]
  (assert (sd/seed? src-seed))
  (cond
    (class? dst-type) (unpack-to-seed (sd/typed-seed dst-type) src-seed)
    (sd/seed? dst-type) (unpack-to-seed dst-type src-seed)
    (vector? dst-type) (unpack-to-vector
                        dst-type
                        (unpack-to-seed
                         (sd/typed-seed clojure.lang.Indexed)
                         src-seed))
    (seq? dst-type) (unpack-to-seq
                     dst-type
                     (unpack-to-seed
                      (sd/typed-seed clojure.lang.ISeq)
                      src-seed))
    (map? dst-type) (unpack-to-map
                     dst-type
                     (unpack-to-seed
                      (sd/typed-seed clojure.lang.ILookup)
                      src-seed))))



(defn cast-seed
  "Casts a seed."
  [type value]
  {:pre [(sd/seed? value)]}
  (when (not (class? type))
    (throw (ex-info "Cannot cast to a non-class type"
                    {:type type
                     :value value})))
  (cond

    ;; Unboxing cast?
    (and (dt/unboxed-type? type)
         (not (dt/unboxed-type? (sd/datatype value)))) 
    (unbox (cast-seed (dt/box-class type) value))

    ;; Same type: No need to cast
    (= type (sd/datatype value)) value

    ;; Different types
    :default (core/make-dynamic-seed
              description "cast-seed"
              mode Mode/Pure
              rawDeps {:value value}
              compiler compile-cast
              type type)))

(defn cast-any-to-seed
  "Converts anything to a seed with a certain type."
  [type x]
  (cast-seed type (core/to-seed x)))

(defn seed-typename
  "Returns the typename of a seed."
  [x]
  {:pre [(sd/seed? x)]}
  (let [dt (sd/datatype x)]
    (assert (class? dt))
    (typename dt)))

(defn to-size-type
  "Converts an integer to int, as used for arrays on the JVM"
  [x]
  (cast-any-to-seed Integer/TYPE x))

(defn make-array-from-size
  "Geex function to make an array"
  [component-class size]
  {:pre [(class? component-class)]}
  (core/make-dynamic-seed
   description "array-seed"
   mode Mode/Pure
   data {:component-class component-class}
   type (class (make-array component-class 0))
   rawDeps {:size (to-size-type size)}
   compiler compile-array-from-size))

(defn set-array-element
  "Geex function to set an array element"
  [dst-array index value]
  (core/make-dynamic-seed
   description "array-set"
   mode Mode/SideEffectful
   type nil
   hasValue false
   rawDeps {:dst dst-array
            :index (to-size-type index)
            :value value}
   compiler compile-set-array))

(defn get-array-element
  "Geex function to get an array element"
  [src-array index]
  (core/make-dynamic-seed
   description "array-get"
   mode Mode/Ordered
   type (.getComponentType (sd/datatype src-array))
   rawDeps {:src src-array
            :index (to-size-type index)}
   compiler compile-get-array))

(defn array-length
  "Geex function to get array length"
  [src-array]
  (core/make-dynamic-seed
   description "array-length"
   mode Mode/Pure
   type java.lang.Integer/TYPE
   rawDeps {:src src-array}
   compiler compile-array-length))

(defn call-operator
  "Geex function to call an operator"
  [operator & args0]
  (debug/exception-hook
   (let [args (mapv core/to-seed args0)
         arg-types (mapv seed/datatype args)
         op-info (get jdefs/operator-info-map operator)
         _ (utils/data-assert (not (nil? op-info))
                              "Operator not recognized"
                              {:operator operator})

         result-fn (:result-fn op-info)
         _ (assert (fn? result-fn))
         ret-type (result-fn arg-types)
         _ (assert (class? ret-type))
         
         _ (utils/data-assert (not (nil? ret-type))
                              "Cannot infer return type for operator and types"
                              {:operator operator
                               :arg-types arg-types})]
     (make-call-operator-seed ret-type operator args))
   (render-text/disp
    (render-text/add-line "Error when calling operator '"
                          operator
                          "' with arguments:")
    (render-text/pprint args0))))

(defn call-operator-with-ret-type
  "Geex function to call an operator with a specified return type"
  [ret-type operator & args0]
  (let [args (mapv core/to-seed args0)]
    (make-call-operator-seed ret-type operator args)))

(defn call-method
  "Geex function to call method"
  [& method-args]
  (let [args (parse-method-args method-args)]
    ((if (contains? (:directives args) :static)
       call-static-method-sub
       call-method-sub)
     (make-method-info args)
     (:dst args)
     (:args args))))


(defn box
  "Geex function to turn a primitive into a boxed value."
  [x0]
  (let [x (core/to-seed x0)
        tp (seed/datatype x)]
    (if (dt/unboxed-type? tp)
      (call-method :static "valueOf" (dt/box-class tp) x)
      x)))

(defn unbox
  "Geex function to unwrap a boxed value."
  [x0]
  (let [x (core/to-seed x0)
        tp (seed/datatype x)]
    (if (dt/unboxed-type? tp)
      x
      (let [unboxed-type (dt/unbox-class tp)]
        (call-method (str (.getName unboxed-type) "Value") x)))))

(def call-static-pure-method (partial call-method :pure :static))

#_(def clj-equiv (partial call-method :pure :static "equiv" clojure.lang.Util))

(defn clj-equiv [x y]
  (let [x (core/to-seed x)
        y (core/to-seed y)]
    (if (and (dt/unboxed-type? (seed/datatype x))
             (dt/unboxed-type? (seed/datatype y)))
      (call-operator "==" x y)
      (call-method :pure :static "equiv" clojure.lang.Util x y))))


(def call-static-method (partial call-method :static))
(def call-pure-method (partial call-method :pure))

;;; Method shorts
(def j-nth (arity-partial call-method "nth" #{3}))
(def j-first (arity-partial call-method "first" #{2}))
(def j-next (arity-partial call-method "next" #{2}))
(def j-count (arity-partial call-method "count" #{2}))
(def j-val-at (arity-partial call-method "valAt" #{3}))

(defmacro disp-ns []
  (let [k# *ns*]
    k#))

(defn instantiate [class-def]
  (let [class-def (gclass/validate-class-def class-def)]
    (when (gclass/abstract? class-def)
      (throw (ex-info "Cannot instantiate an abstract class"
                      {:class-def class-def})))
    (when (gclass/interface? class-def)
      (throw (ex-info "Cannot instantiate an interface")))
    (assert (gclass/anonymous? class-def))
    (with-register-class
      class-def
      (fn [class-def]
        (anonymous-instance-seed
         class-def
         (expand-class-body true class-def))))))

(defn with-local-class [class-def body-fn]
  (let [class-def (gclass/validate-class-def class-def)]
    (with-register-class
      class-def
      (fn [class-def]
        (local-class-seed
         class-def
         (expand-class-body true class-def))
        (body-fn (:public-stub class-def))))))

(spec/def ::let-class-binding (spec/cat :symbol symbol?
                                        :class-def any?))
(spec/def ::let-class-args (spec/* ::let-class-binding))

(defmacro let-class [args & body]
  (let [parsed-args (spec/conform ::let-class-args args)]
    (when (= parsed-args ::spec/invalid)
      (throw (ex-info (str "Failed to parse let-class-args: "
                           (spec/explain-str ::let-class-args args))
                      {})))
    (let-class-sub parsed-args body)))


(defn define-class [class-def]
  (define-class-sub false class-def))

(defn render-class-data [class-def]
  (let [pkg (:package class-def)
        class-def (gclass/validate-class-def class-def)
        body-fn (fn []
                  (apply core/set-flag! (:flags class-def))
                  ;; List it, because by default there is a top
                  ;; scope, and Mode/Code-seeds will not be
                  ;; rendered there.
                  (core/list! (define-top-class class-def)))

        fg (core/full-generate
            [{:platform :java}]
            (body-fn))
        fg (update fg :result
                   (fn [code]
                     (if (nil? pkg) code ["package "
                                          (:package class-def)
                                          "; " code])))]
    fg))

(defn- cook-and-show-errors [simple-compiler code]
  (try
    (.cook simple-compiler code)
    (catch Throwable e
      (println "Compilation error in this code:\n" code)
      (throw e))))

(defn render-compile-and-load-class [pkg class-def settings]
  (binding [build-callbacks (atom [])]
    (let [class-def (assoc class-def :package pkg)
          class-def (gclass/validate-class-def class-def)
          class-data (render-class-data class-def)
          log (:timelog class-data)
          state (:state class-data)
          code (:result class-data)
          log (timelog/log log "Composed class")
          code (nested-to-string-top code)
          formatted-code (if (or (gclass/format? class-def)
                              (.hasFlag state :format))
                        (format-nested-show-error code)
                        code)
          _ (when (.hasFlag state :disp)
              (println formatted-code))
          log (timelog/log log "Formatted code")
          disp-time? (.hasFlag state :disp-time)
          seed-count (.getSeedCount state)
          class-name (gclass/full-java-class-name class-def)
          sc (SimpleCompiler.)
          log (timelog/log log "Created compiler")
          _ (cook-and-show-errors sc formatted-code)
          log (timelog/log log "Compiled it")
          cl (.loadClass (.getClassLoader sc) class-name)
          log (timelog/log log "Loaded class")
          all-data {:log log
                    :state state
                    :code code
                    :class-name class-name
                    :class cl
                    :class-def class-def
                    :formatted-code formatted-code}]
      (doseq [cb (deref build-callbacks)]
        (cb all-data))
      (when disp-time?
        (println "--- Time report ---")
        (timelog/disp log)
        (println "\nNumber of seeds:" seed-count)
        (println "Time per seed:" (/ (timelog/total-time log)
                                     seed-count))
        nil)
      all-data)))

(defn write-source-file [class-def settings]
  {:pre [(settings? settings)
         (map? settings)]}
  (let [class-def (gclass/validate-class-def class-def)
        class-data (render-class-data class-def)
        code (:result class-data)
        code (nested-to-string code)
        source-code (format-nested-show-error code)
        class-name (gclass/full-java-class-name class-def)
        dst-path (class-name-to-path class-name settings)]
    (io/make-parents dst-path)
    (spit dst-path source-code)
    (println "Wrote code to" (.getCanonicalPath dst-path))))

(defn write-source-files [class-defs settings]
  (if (sequential? class-defs)
    (doseq [cd class-defs]
      (write-source-files cd settings))
    (write-source-file class-defs settings)))

(defmacro this-file-ns []
  (let [g (gensym)]
    `(do
       (def ~g)
       (:ns (meta #'~g)))))

(defmacro package-from-ns []
  `(cljstr/replace (str (this-file-ns)) "-" "_"))

(defmacro make-class [class-def]
  `(:class
    (render-compile-and-load-class
     (str-to-java-identifier (str *ns*))
     ~class-def {})))

(defn render-class-and-save-source [pkg class-def settings]
  {:pre [(map? class-def)
         (contains? class-def :name)]}
  (let [suffix "119DynDefClass119"
        true-full-class-name (gclass/full-java-class-name pkg (:name class-def))


        ;; Here's the thing: If we have already rendered the source code to disk
        ;; in the past and compiled it with Javac, the Janino class loader will
        ;; load *that* class instead of the one compiled on-the-fly here. So when
        ;; dynamically compiling a class, we give it a unique name by appending 
        ;; a special suffix to its name. From the generated source code, we remove all
        ;; occurrences of that suffix. It is not beautiful... but it solves it for now.
        rendered (render-compile-and-load-class
                  pkg (update class-def :name (fn [old-name] (str old-name suffix)))
                  settings)
        
        output-path (:output-path settings)
        dst-path (class-name-to-path true-full-class-name {:output-path output-path})
        cleaned-up-code (cljstr/replace (:formatted-code rendered) suffix "")]
    (io/make-parents dst-path)
    (spit dst-path cleaned-up-code)
    (:class rendered)))

(defmacro def-class [class-symbol class-def & extra-class-data]
  {:pre [(symbol? class-symbol)
         (map? class-def)]}
  (let [{:keys [mode java-output-path]} (merge-onto (environment) class-def)
        package-name (or (:package class-def)
                         "geex_defclass")]
    (when (not (contains? class-def :package))
      (println
       (str "Warning: No :package specified for class-def of "
            (:name class-def)
            ", defaults to geex_defclass. Pay attention to naming collisions.")))

    (when (not (string? package-name))
      (throw (ex-info "Package name must be a string"
                      {:provided-package-name package-name})))
    
    (when (nil? mode)
      (throw (ex-info "Mode is nil. Please specify mode using jvm-opts in your Leiningen project."
                      {:example-leiningen-project.clj
                       {:profiles {:dev        {:jvm-opts ["-Dgeex_mode=development"]}
                                   :test       {:jvm-opts ["-Dgeex_mode=test"]}
                                   :production {:jvm-opts ["-Dgeex_mode=production"]} 
                                   :repl       {:jvm-opts ["-Dgeex_mode=repl"]}
                                   :uberjar    {:jvm-opts ["-Dgeex_mode=uberjar"]}}}})))

    (when (nil? java-output-path)
      (throw (ex-info "Java output path is nil. Please specify it using jvm-opts in your Leiningen project"
                      {:example-leiningen-project.clj
                       {:jvm-opts ["-Dgeex_java_output_path=/tmp/geexjava"]}})))
    
    (if (= :repl mode)
      `(def ~class-symbol (render-class-and-save-source
                           ~package-name
                           (reduce merge ~class-def ~(vec extra-class-data))
                           {:output-path ~java-output-path}))
      (let [full-name (symbol (gclass/full-java-class-name
                               package-name (:name class-def)))]
        `(def ~class-symbol ~full-name)))))

(defn add-build-callback
  "Adds a callback that receives all relevant data once a class has been built."
  [cb]
  (assert build-callbacks)
  (swap! build-callbacks conj cb))

(defmacro typed-defn [& args0]
  (let [args (parse-typed-defn-args args0)
        fn-name (:name args)
        arglist (:arglist args)
        body (append-void-if-empty (:body args))
        arg-names (mapv :name arglist)
        body-fn `(fn [this# ~@arg-names] ~@body)
        class-name (str-to-java-identifier
                    (str "TypedDefn_"
                         fn-name))]
    `(do
       (let [obj# (.newInstance
                   (make-class {:name ~class-name
                                :flags []
                                :methods
                                [{:name "apply"
                                  :arg-types ~(mapv
                                               :type
                                               arglist)
                                  :fn ~body-fn}]}))]
         (defn ~fn-name [~@arg-names]
           (.apply obj# ~@arg-names))))))

(defn set-instance-var [dst-object field-name value]
  {:pre [(string? field-name)]}
  (let [dst-object (prepare-object dst-object)
        dst-type (seed/datatype dst-object)]
    (let [field (.getField dst-type field-name)
          field-type (.getType field)
          value (unpack field-type (core/wrap value))]
      (core/make-dynamic-seed
       description "set instance var"
       rawDeps {:value value
                :dst dst-object}
       mode Mode/SideEffectful
       hasValue false
       compiler (fn [state expr]
                  (let [deps (seed/access-compiled-deps expr)]
                    [(:dst deps)
                     (str "." field-name " = ")
                     (:value deps)
                     ";"]))))))

(defn get-instance-var [src-object field-name]
  {:pre [(string? field-name)]}
  (let [src-object (prepare-object src-object)
        src-type (seed/datatype src-object)]
    (let [field (.getField src-type field-name)
          field-type (.getType field)]
      (core/make-dynamic-seed
       description "get instance var"
       rawDeps {:src src-object}
       mode Mode/Pure
       type field-type
       compiler (fn [state expr]
                  (let [deps (seed/access-compiled-deps expr)]
                    (wrap-in-parens
                     [(:src deps)
                      (str "." field-name)])))))))

(defn set-static-var [field-name dst-class value]
  {:pre [(class? dst-class)
         (string? field-name)]}
  (let [field (.getField dst-class field-name)
        field-type (.getType field)
        value (unpack field-type (core/wrap value))]
    (core/make-dynamic-seed
     description "set static var"
     rawDeps {:value value}
     mode Mode/SideEffectful
     hasValue false
     compiler (fn [state expr]
                (let [deps (seed/access-compiled-deps expr)]
                  [(class-name-prefix dst-class)
                   field-name " = "
                   (:value deps)
                   ";"]
                  )))))

(defn get-static-var [field-name src-class]
  {:pre [(class? src-class)
         (string? field-name)]}
  (let [field (.getField src-class field-name)
        field-type (.getType field)]
    (core/make-dynamic-seed
     description "get static var"
     mode Mode/Pure
     type field-type
     compiler (fn [state expr]
                (wrap-in-parens
                 [(class-name-prefix src-class)
                  field-name])))))

(defn system-out []
  (get-static-var "out" System))

(defn- java-println [s]
  ((system-out) 'println s))


(defn throw [x]
  (let [x (core/wrap x)]
    (when (not (class? (seed/datatype x)))
      (throw (ex-info "Cannot throw something whose type is not a class"
                      {:x x})))
    (core/make-dynamic-seed
     description "throw"
     mode Mode/SideEffectful
     hasValue false
     rawDeps {:exception x}
     compiler (fn [state expr]
                ["if (true) {throw "
                 (:exception (seed/access-compiled-deps expr))
                 ";}"]))))

(defn switch-fn [k cases default-fn]
  {:pre [(every? fn? (mapv second cases))
         (fn? default-fn)]}
  (core/with-branching-code
    (fn [bd]
      (let [evaled-k (core/wrap k)]
        (case-sub
         k
         (mapv (fn [[k case-body-fn]]
                 {:pre (fn? case-body-fn)}
                 [k (core/perform-branch bd case-body-fn)])
               cases)
         (core/perform-branch bd default-fn)))))  )

(defmacro switch [& args]
  (let [parsed (spec/conform ::case-args args)]
    (if (= ::spec/invalid parsed)
      (throw (ex-info (str "Failed to parse case args: "
                           (spec/explain-str ::case-args args)))))

    (let [k (:key parsed)
          cases (:cases parsed)
          default (:default parsed)
          bd (gensym)]
      `(switch-fn
        ~k
        ~(mapv (fn [c] [(:value c) `(fn [] ~(:code c))])
               cases)
        (fn [] ~default)))))

(defn format-literal [type value]
  (cond
    (= Float/TYPE type) (str value "f")
    (= Long/TYPE type) (str value "L")
    :default 
    (str value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implement common methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn seq-iterable [src]
  (xp/call :seq (core/wrap src)))

(ebmd/declare-poly iterable)

(ebmd/def-poly iterable [etype/any x]
                   x)

(ebmd/def-poly iterable
  [(getype/seed-of java.lang.Object) src]
  (seq-iterable src))

(ebmd/def-poly iterable
  [(getype/seed-of clojure.lang.IPersistentVector) src]
  (seq-iterable src))

; Not pure!!!
;  "random"

(defn check-compilation-result [seed x]
  (when (not (or (string? x)
                 (vector? x)
                 (keyword? x)))
    (throw (ex-info
            "Invalid compilation result"
            {:seed seed
             :result x}))))


(defn new [cl & args0]
  {:pre [(class? cl)]}
  (let [args (mapv core/wrap args0)
        arg-types (mapv seed/datatype args)]
    (when (not (every? class? arg-types))
      (throw (ex-info
              "Cannot call constructor. Not every argument is a class"
              {:class cl
               :raw-args args0
               :wrapped-args args
               :arg-types arg-types})))
    (let [constructor (.getConstructor
                       cl
                       (into-array java.lang.Class arg-types))]
      (call-constructor-seed cl args))))

(def collection-types
  [clojure.lang.PersistentVector
   clojure.lang.IPersistentVector
   clojure.lang.IPersistentMap
   clojure.lang.IPersistentSet
   clojure.lang.IPersistentList
   clojure.lang.IPersistentStack
   clojure.lang.ISeq
   clojure.lang.IPersistentCollection
   ])

(defn get-primary-collection-type [x]
  (first
   (filter
    (partial isa? x)
    collection-types)))

(defn conj-impl [dst x]
  {:pre [(seed/seed? dst)]}
  (let [cl (seed/datatype dst)
        stable-type (get-primary-collection-type cl)]
    (cast-seed
     stable-type
     (call-method
      :static :pure
      "conj"
      clojure.lang.RT
      dst
      (cast-any-to-seed java.lang.Object x)))))

#_(fn [x]
           )

(defn- seed-to-scope-code [^ISeed x]
  (let [state (.getState x)
        mode (.getMode x)
        tp (seed/datatype x)]
    (cond
      (.isListed state)
      [(if (.isBound state) 
         ["final "
          (typename tp)
          " "
          (.getKey state)
          " = "]
         [])
       (.getValue state)
       (if (= mode Mode/Code)
         ""
         ";")]
      :defualt [])))

(defn- close-scope-fn [^State state ^ISeed close-seed]
  (let [deps (core/ordered-indexed-deps close-seed)]
    (if (empty? deps) []
        (let [last-dep (last deps)]
          (if (.hasValue last-dep)
            (throw
             (ex-info
              "In Java, the last seed in a scope 
must not have a value"
              {:deps deps
               :last-dep last-dep})))
          (reduce into
                  []
                  (mapv seed-to-scope-code deps))))))

(defn- gen-seed-sym [^ISeed x]
  (format "s%04d" (.getId x)))

(xp/register
 :java
 (merge
  (java-math-fns jdefs/math-functions)
  
  {
  :settings-for-state
  (fn [state-params]
    (doto (StateSettings.)
      (set-field platform :java)
      (set-field checkCompilationResult check-compilation-result)
      (set-field closeScope close-scope-fn)
      (set-field generateSeedSymbol gen-seed-sym)
      (set-field forwardedFunction seed-call-access)))

   :default-expr-for-type default-expr-for-type

   :lvar-for-seed core/lvar-str-for-seed

   :counter-to-sym core/counter-to-str

   :local-var-sym core/local-var-str

   :get-compilable-type-signature
   gjvm/get-compilable-type-signature

   :compile-set-local-var
   (fn [state expr]
     (let [lvar (.getData expr)
           sym (xp/call :local-var-sym (.getIndex lvar))
           deps (seed/access-compiled-deps expr)
           v (:value deps)]
       [sym " = " v]
       ))

   :compile-get-var (fn [state expr]
                      (xp/call
                       :local-var-sym
                       (-> expr .getData)))

   :compile-coll2
   (fn [comp-state expr]
     (let [original-coll (.getData expr)
           args (vec
                 (seed/access-compiled-indexed-deps
                  expr))]
       (cond
         (seq? original-coll) (compile-seq comp-state args)
         (vector? original-coll) (compile-vec comp-state args)
         (set? original-coll) (compile-set comp-state args)
         (map? original-coll) (compile-map
                               comp-state
                               args))))

   :compile-class
   (fn [comp-state expr]
     "null"          
     )

   :compile-static-value
   (fn [state expr]
     (format-literal
      (.getType expr)
      (.getData expr)))

   :make-void make-void

   :compile-nothing (constantly [])
   
   :keyword-seed
   (fn  [state kwd]
     (core/make-dynamic-seed
      state
      description "keyword"
      data {:type "Keyword"
            :value kwd}
      mode Mode/Pure
      type clojure.lang.Keyword
      compiler compile-interned))

   :symbol-seed
   (fn  [state sym]
     (core/make-dynamic-seed
      state
      description "symbol"
      mode Mode/Pure
      data {:type "Symbol"
            :value sym}
      type clojure.lang.Symbol
      compiler compile-interned))

   :string-seed
   (fn [state x]
     (core/make-dynamic-seed
      state
      description "String seed"
      mode Mode/Pure
      data x
      type java.lang.String
      compiler compile-string))

   :char-seed
   (fn [state x]
     (core/make-dynamic-seed
      state
      description "Char seed"
      mode Mode/Pure
      data x
      type Character/TYPE
      compiler compile-char))
   
   :make-nil #(core/nil-of % java.lang.Object)

   :compile-if
   (fn [state expr]
     (let [deps (seed/access-compiled-deps expr)]
       (render-if (:cond deps)
                  (:on-true deps)
                  (:on-false deps))))

   :compile-bind-name to-java-identifier

   :compile-return-value
   (fn [^ISeed value-seed]
     (if (.hasValue value-seed)
       (let [r (seed/compilation-result value-seed)]
         ["return " r])
       "return"))

   :compile-nil?
   (fn [comp-state expr]
     (wrap-in-parens
      [(-> expr sd/access-compiled-deps :value)
       "== null"]))

   :binary-add (arity-partial call-operator "+" [:a :b])
   :unary-add (arity-partial call-operator "+" [:a])
   :binary-div (arity-partial call-operator "/" [:a :b])
   :binary-sub (arity-partial call-operator "-" [:a :b])
   :binary-mul (arity-partial call-operator "*" [:a :b])
   :negate (arity-partial call-operator "-" [:a])
   :not (arity-partial call-operator "!" [:a])

   :quot (arity-partial call-method :static "quotient" clojure.lang.Numbers
                        [:numerator :denominator])
   :rem (arity-partial call-method :static "remainder" clojure.lang.Numbers
                 [:numerator :denominator])

   :== (cmp-operator "==")
   :<= (cmp-operator "<=")
   :>= (cmp-operator ">=")
   :< (cmp-operator "<")
   :> (cmp-operator ">")
   :!= (cmp-operator "!=")

   ;;; Bitwise
   :bit-not (arity-partial call-operator "~" [:value])
   :bit-shift-left (arity-partial call-operator "<<" [:value :n])
   :unsigned-bit-shift-left (arity-partial call-operator "<<<" [:value :n])
   :bit-shift-right (arity-partial call-operator ">>" [:value :n])
   :unsigned-bit-shift-right (arity-partial call-operator ">>>" [:value :n])
   :bit-and (arity-partial call-operator "&" [:a :b])
   :bit-flip (arity-partial call-operator "^" [:a :b])
   :bit-or (arity-partial call-operator "|" [:a :b])
   

   :make-array make-array-from-size
   :aget get-array-element
   :nth-string (partial-wrapping-args call-method
                                      [:pure "charAt"]
                                      identity cast-to-int)
   :count-string (arity-partial call-method :pure "length" [:string])
   :aset set-array-element
   :alength array-length

   :conj conj-impl

   :first (collection-op "first")
   :rest (collection-op "more")
   :count (collection-op "count")
   :seq (collection-op "seq")

   := clj-equiv

   :iterable iterable

   :compile-nil (constantly "null")

   :cast cast-any-to-seed

   :unwrap unpack

   :finite? (numeric-class-method "isFinite")
   :infinite? (numeric-class-method "isInfinite")
   :nan? (numeric-class-method "isNaN")

   :basic-random (arity-partial call-method :static "random" java.lang.Math [])

   :call-method call-method


   ;; Default types for this platform
   :size-type (constantly Integer/TYPE)
   :float-type (constantly Double/TYPE)
   :int-type (constantly Integer/TYPE)

   :error throw-error

   :compile-recur compile-recur
   :compile-loop compile-loop2

   :compile-local-var-section compile-local-var-section

   :to-string to-string
   :concatenate-strings concatenate-strings
   :println java-println
   }))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  User terface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn eval-body-fn [body-fn]
  (let [tmp-name (str (gensym "Eval"))
        body-fn (fn [_] (body-fn))
        obj (.newInstance
             (make-class {:name tmp-name
                          :flags []
                          :methods
                          [{:name "perform"
                            :arg-types []
                            :fn body-fn}]}))]
    (.perform obj)))

(defmacro eval
  "Evaluate geex code"
  [& args]
  `(eval-body-fn (fn [] ~@args)))

