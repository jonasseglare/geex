(ns geex.java

  "Generation of Java backed code and utilities for embed it."

  (:import [geex SeedParameters Mode
            JavaPlatformFunctions
            StateSettings
            CodeMap CodeItem])
  (:require [geex.java.defs :as jdefs]
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
            )
  (:refer-clojure :exclude [eval])
  
  (:import [org.codehaus.janino SimpleCompiler]
           [com.google.googlejavaformat.java Formatter FormatterException]
           [com.google.googlejavaformat FormatterDiagnostic
            ]))

;; Lot's of interesting stuff going on here.
;; https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html

(def platform-tag [:platform :java])


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Declarations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare unpack)
(declare quote-args)
(declare seed-typename)
(declare unbox)
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
(declare make-arg-list)
(declare call-method)
(declare cast-any-to-seed)
(declare call-static-pure-method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- compile-cast [comp-state expr cb]
  (cb (seed/compilation-result
        comp-state
        (wrap-in-parens
         ["(" (r/typename (sd/datatype expr)) ")"
          (-> expr
              seed/access-compiled-deps
              :value)]))))

(def compile-void (core/wrap-expr-compiler (fn [_] "/*void*/")))

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

(defn java-class-name [parsed-args]
  (-> parsed-args
      :name
      name
      str-to-java-identifier))



(defn- java-package-name [parsed-args]
  (-> parsed-args
      :ns
      str-to-java-identifier))

(defn- full-java-class-name [parsed-args]
  (str (java-package-name parsed-args)
       "."
       (java-class-name parsed-args)))



(defn- quote-arg-name [arg]
  (assert (map? arg))
  (merge arg
         {:name `(quote ~(:name arg))}))

(defn- eval-arg-type [arg]
  (update arg :type clojure.core/eval))

(defn- make-arg-decl [parsed-arg]
  (let [tp (:type parsed-arg)
        type-sig (gjvm/get-type-signature tp)
        java-typename (r/typename type-sig)]
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

(defn- compile-call-method [comp-state expr cb]
  (cb
   (seed/compilation-result
     comp-state
     (wrap-in-parens
      [(:obj (sd/access-compiled-deps expr))
       "."
       (.getData expr)
       (let [dp (sd/access-compiled-indexed-deps expr)]
         (wrap-in-parens (join-args dp)))]))))

(defn- compile-call-static-method [comp-state expr cb]
  (let [data (.getData expr)]
    (cb
     (seed/compilation-result
       comp-state
       (wrap-in-parens
        [(.getName (:class data))
         "."
         (:method-name data)
         (let [dp (sd/access-compiled-indexed-deps expr)]
           (wrap-in-parens (join-args dp)))])))))

(defn- format-source [src]
  (try
    (.formatSource (Formatter.) src)
    (catch FormatterException e
      (println "Failed to format this:")
      (println (point-at-diagnostic src (-> e
                                            .diagnostics
                                            (.get 0))))
      (throw e))))

(def format-nested (comp format-source
                         nested-to-string))

(defn- preprocess-method-args [args0]
  (let [args (mapv core/to-seed args0)
        arg-types (into-array java.lang.Class (mapv sd/datatype args))]
    (utils/map-of args arg-types)))

(defn- compile-operator-call [comp-state expr cb]
  (let [args (sd/access-compiled-indexed-deps expr)
        op (.getData expr)]
    (cb (seed/compilation-result
          comp-state
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
                           (rest args))])))))))

;;;;;;;;;;;;;;;;;;;; keywords

(defn- render-var-init [tp name val]
  [tp " " name " = " val ";"])

(defn- bind-statically [key comp-state binding-type binding-name binding-value]
  (seed/compilation-result
    (core/add-top-code
     comp-state
     key
     ["static "
      (render-var-init
       binding-type
       binding-name
       binding-value)])
    binding-name))

(defn- escape-char [x]
  (or (char-escape-string x) x))

(defn- java-string-literal [s]
  (str "\"" (apply str (map escape-char s)) "\""))

(defn- compile-interned [comp-state expr cb]
  (let [data (sd/access-seed-data expr)
        kwd (:value data)
        tp (:type data)]
    (cb
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
       (java-string-literal (name kwd)) ")"]))))

(defn- compile-string [comp-state expr cb]
  (cb
   (seed/compilation-result
     comp-state
     (java-string-literal (sd/access-seed-data expr)))))

(defn- make-seq-expr [args]
  ["clojure.lang.PersistentList.EMPTY"
   (map (fn [arg]
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

(defn- compile-seq [comp-state args cb]
  (cb (seed/compilation-result comp-state (make-seq-expr args))))

(defn- compile-vec [comp-state args cb]
  (cb (seed/compilation-result comp-state (make-vec-expr args))))

(defn- compile-map [comp-state args cb]
  (cb (seed/compilation-result comp-state (make-map-expr args))))

(defn- compile-set [comp-state args cb]
  (cb (seed/compilation-result comp-state (make-set-expr args))))

(defn- compile-array-from-size [comp-state expr cb]
  (cb (seed/compilation-result
        comp-state
        (wrap-in-parens
         ["new " (-> expr
                     seed/access-seed-data
                     :component-class
                     r/typename) "["
          (-> expr seed/access-compiled-deps :size) "]"]))))

(def ^:private compile-set-array (core/wrap-expr-compiler
                        (fn [expr]
                          (let [deps (seed/access-compiled-deps expr)]
                            [(:dst deps) "[" (:index deps) "] = " (:value deps)]))))

(def ^:private compile-get-array (core/wrap-expr-compiler
                        (fn [expr]
                          (let [deps (seed/access-compiled-deps expr)]
                            (wrap-in-parens [(:src deps) "[" (:index deps) "]"])))))

(def ^:private compile-array-length (core/wrap-expr-compiler
                           (fn [expr]
                             (let [deps (seed/access-compiled-deps expr)]
                               (wrap-in-parens [(:src deps) ".length"])))))

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

(defn- compile-assign [comp-state expr cb]
  (cb
   (seed/compilation-result
     comp-state
     (let [v (-> expr seed/access-compiled-deps
                 :value)]
       [(.getData expr) " = " v]))))

(defn- compile-recur [state expr cb]
  (core/set-compilation-result
   state
   "continue"
   cb))

(defn- compile-loop2 [state expr cb]
  (let [deps (.getMap (.deps expr))
        body  (-> deps :body)]
    (core/set-compilation-result
     state
     ["while (true) {"
      (.getCompilationResult body)
      "break;}"]
     cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Basic platform operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cmp-operator [op]
  (partial
   call-operator-with-ret-type
   Boolean/TYPE
   op))

(defn- call-static-method-sub [info cl args0]
  {:pre [(class? cl)]}
  (let [method-name (:name info)
        {:keys [args arg-types]} (preprocess-method-args args0)
        method (.getMethod cl method-name arg-types)]
    (core/make-dynamic-seed
     description (str "call static method "
                      method-name)
     type (.getReturnType method)
     data {:class cl
           :method-name method-name}
     mode (if (:dirty? info)
            Mode/SideEffectful
            Mode/Pure)
     rawDeps (core/to-indexed-map args)
     compiler compile-call-static-method)))

(defn- make-method-info [parsed-method-args]
  (let [dirs (:directives parsed-method-args)]
    (merge
     {:dirty? (not (contains? dirs :pure))
      :name (:name parsed-method-args)})))


(defn- call-method-sub [info obj0 args0]
  (let [method-name (:name info)
        obj (core/to-seed obj0)
        {:keys [args arg-types]} (preprocess-method-args args0)
        cl (sd/datatype obj)
        method (.getMethod cl method-name arg-types)]
    (core/make-dynamic-seed
     compiler compile-call-method
     description "call method"
     type (.getReturnType method)
     rawDeps (merge {:obj obj}
                    (core/to-indexed-map args))
     mode (if (:dirty? info)
            Mode/SideEffectful
            Mode/Pure)
     data method-name)))

(defn- call-break []
  (core/make-dynamic-seed
   type nil
   description "Break"
   mode Mode/SideEffectful
   compiler (core/constant-code-compiler "break")))

(defn- throw-error [msg]
  (core/make-dynamic-seed
   type nil
   description "Crash"
   mode Mode/SideEffectful
   compiler (core/constant-code-compiler
             (str "throw new RuntimeException("
                  (java-string-literal msg)
                  ")"))))

(defn- nothing-seed [state]
  (core/make-dynamic-seed
   description "Nothing"
   mode Mode/Pure
   type nil
   compiler (core/constant-code-compiler [])))


(defn- format-nested-show-error [code]
  (try
    (format-nested code)
    (catch Throwable e
      (println "The input code")
      (pp/pprint code)
      (throw e))))

(defn- make-typed-defn-body-fn [arglist
                               quoted-args
                               body]
  `(fn []
     (core/return-value
      (apply
       (fn [~@(map :name arglist)]
         ~@(append-void-if-empty
            body))

       ;; Unpacking happens here
       (map to-binding ~quoted-args)))))

(defn- generate-typed-defn [package-name
                           class-name
                           body-fn
                           quoted-args]
  (let [fg (core/full-generate
            [{:platform :java}]
            (body-fn))
        code (:result fg)
        cs (:state fg)
        log (:timelog fg)
        all-code [["package " package-name ";"]
                  (str "public class "
                       class-name " {")
                   "/* Static code */"
                   (core/get-top-code cs)
                   "/* Methods */"
                   ["public " (return-type-signature fg)
                    " apply("
                    (make-arg-list quoted-args)
                    ") {"
                    code
                    "}"]
                  "}"]
        ;_ (println "log=" log)
        log (timelog/log log "Composed class")
        formatted (format-nested-show-error all-code)
        log (timelog/log log "Formatted code")
        final-state (:state fg)]
    (when (.hasFlag final-state :disp-final-source)
      (println formatted))
    [formatted log final-state]))

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


(defn default-expr-for-type [x]
  {:pre [(class? x)]}
  (cond
    (= Float/TYPE x) "0.0f"
    (= Double/TYPE x) "0.0"
    (or (= Integer/TYPE x)
        (= Long/TYPE x)
        (= Short/TYPE x)
        (= Character/TYPE x)) "0"
    (= Boolean/TYPE x) "false"
    :default "null"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Low level interface for other modules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn quote-args
   "Internal function:"
  [arglist]
  (mapv quote-arg-name arglist))

(defn assign
  "Internal function:"
  [dst-var-name src]
  {:pre [(string? dst-var-name)]}
  (core/make-dynamic-seed
   type nil
   description "assign"
   rawDeps {:value src}
   mode Mode/SideEffectful
   data dst-var-name
   compiler compile-assign))


(defn return-type-signature
  "Interal function: Given the output fg of full-generate, get the typename of the return value."
  [fg]
  (-> fg
      :expr
      gjvm/get-type-signature
      r/typename))

(defn make-void []
  "Creates a seed representing void"
  (core/make-dynamic-seed
   description "void"
   mode Mode/Pure
   type Void/TYPE
   bind false
   compiler compile-void))



(defn append-void-if-empty [x]
  "Internal function: Used when generating typed-defn"
  {:pre [(or (sequential? x)
             (nil? x))]}
  (if (empty? x)
    `((make-void))
    x))

(defn to-binding [quoted-arg]
  "Internal function: Used when importing the arguments to a method."
  (let [tp (:type quoted-arg)
        t (gjvm/get-type-signature tp)]
    ;;; TODO: Get the type, depending on what...
    (unpack
     
     ;; The actual type used by us:
     tp 

     ;; A seed holding the raw runtime value
     (core/bind-name t (:name quoted-arg)))))

(defn make-arg-list
  "Internal function: Used to generate code for function arglist."
  [parsed-args]
  {:pre [(jdefs/parsed-typed-arguments? parsed-args)]}
  (or (reduce join-args2 (map make-arg-decl parsed-args)) []))

(defn import-type-signature
  "Internal function: Used when parsing the type specification of a function."
  [x]
  (second
   (core/flat-seeds-traverse
    seed-or-class?
    x
    (comp sd/strip-seed class-to-typed-seed))))

(defn str-to-java-identifier
  "Internal function: Used in code generation to produce a string representing a valid Java identifier."
  [& args]
  (->> args
       (cljstr/join "_")
       vec
       (map special-char-to-escaped)
       (apply str)))

(ebmd/declare-poly to-java-identifier)

(ebmd/def-poly to-java-identifier [etype/symbol x]
  (str-to-java-identifier (name x)))

(ebmd/def-poly to-java-identifier [etype/string x]
  (str-to-java-identifier x))


(defn parse-typed-defn-args
  "Internal function: Parses the input to typed-defn macro."
  [args0]
  {:post [(jdefs/parsed-defn-args? %)]}
  (specutils/force-conform ::jdefs/defn-args args0))

(defn janino-cook-and-load-class
  "Given a class-name and source code of that class, compile the code and load the class dynamically."
  [class-name source-code]
  "Dynamically compile and load Java code as a class"
  [class-name source-code]
  (try
    (let [sc (SimpleCompiler.)]
      (.cook sc source-code)
      (.loadClass (.getClassLoader sc) class-name))
    (catch org.codehaus.commons.compiler.CompileException e
      (let [location (.getLocation e)
            marked-source-code (if (nil? location)
                                 "(no location to point at)"
                                 (point-at-error source-code location))]
        (println marked-source-code)
        (throw (ex-info "Failed to compile code"
                        {:code marked-source-code
                         :location location
                         :exception e}))))))

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



(defn cast-any-to-seed
  "Converts anything to a seed."
  [type x]
  (cast-seed type (core/to-seed x)))

(defn cast-seed
  "Casts a seed."
  [type value]
  {:pre [(sd/seed? value)]}
  (if (not (class? type))
    (println "-----type=" type))
  (if (and (dt/unboxed-type? type)
           (not (dt/unboxed-type? (sd/datatype value)))) 
    (unbox (cast-seed (dt/box-class type) value))
    (core/make-dynamic-seed
     description "cast-seed"
     mode Mode/Pure
     rawDeps {:value value}
     compiler compile-cast
     type type)))



(defn seed-typename
  "Returns the typename of a seed."
  [x]
  {:pre [(sd/seed? x)]}
  (let [dt (sd/datatype x)]
    (assert (class? dt))
    (r/typename dt)))

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
   (let [args (map core/to-seed args0)
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
  (let [args (map core/to-seed args0)]
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

(def clj-equiv (partial call-method :pure :static "equiv" clojure.lang.Util))


(def call-static-method (partial call-method :static))
(def call-pure-method (partial call-method :pure))

;;; Method shorts
(def j-nth (partial call-method "nth"))
(def j-first (partial call-method "first"))
(def j-next (partial call-method "next"))
(def j-count (partial call-method "count"))
(def j-val-at (partial call-method "valAt"))

(defmacro disp-ns []
  (let [k# *ns*]
    k#))

(defn config-actual-type [vdef]
  {:pre [(spec/valid? ::gclass/variable vdef)]}
  (assoc vdef :actual-type
         (gjvm/get-type-signature (:type vdef))))

(defn compile-member-variable [state expr cb]
  (let [vdef (.getData expr)
        deps (seed/access-compiled-deps expr)
        tp (:actual-type vdef)]
    (core/set-compilation-result
     state
     [(if (gclass/static? vdef)
        "static"
        "")
      (-> vdef
          gclass/visibility
          gclass/visibility-str)
      (r/typename tp)
      (:name vdef)
      (if (contains? deps :init)
        [" = " (:init deps)]
        [])]
     cb)))

(defn make-variable-seed [class-def v]
  (let [v (config-actual-type v)]
    (core/make-dynamic-seed
     (core/get-state)
     description "member variable"
     data v
     type nil
     mode Mode/SideEffectful
     rawDeps (if (contains? v :init)
               {:init (cast-any-to-seed
                       (:actual-type v)
                       (core/wrap (:init v)))}
               {})
     compiler compile-member-variable
     )))

(defn make-method-seed [class-def m]
  )

(defn compile-anonymous-instance [state expr cb]
  (let [deps (seed/access-compiled-deps expr)
        cdef (.getData expr)]
    (core/set-compilation-result
     state
     ["new " (-> cdef :super r/typename) "() {"
      (:scope deps)
      "}"]
     cb)))

(defn anonymous-instance-seed [class-def scope]
  (core/make-dynamic-seed
   (core/get-state)
   description "anonymous object"
   rawDeps {:scope scope}
   mode Mode/SideEffectful
   data class-def
   bind true
   type (:super class-def)
   compiler compile-anonymous-instance))


(defn instantiate [class-def]
  (gclass/validate-class-def class-def)
  (assert (gclass/anonymous? class-def))
  (core/flush! nil)
  (core/begin-scope! {:depending-scope? true})
  (let [vars (mapv (partial make-variable-seed
                            class-def)
                   (:variables class-def))
        methods (mapv (partial make-method-seed class-def)
                      (:methods class-def))
        es (core/dont-bind! (core/end-scope! (core/flush! ::defs/nothing)))]
    (anonymous-instance-seed
     class-def
     es)))






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

(defn check-compilation-result [x]
  (assert (or (string? x)
              (sequential? x)
              (keyword? x))
          (str "Invalid compilation result of type " (class x) ": " x)))









(xp/register
 :java
 (merge
  (java-math-fns jdefs/math-functions)
  
  {
  :settings-for-state
  (fn [state-params]
    (doto (StateSettings.)
      (set-field platformFunctions (JavaPlatformFunctions.))
      (set-field platform :java)))


   :render-bindings
   (fn [tail body-fn]
     [(mapv (fn [x]
              [(let [dt (.type x)]
                 (cond
                   (nil? dt) []
                   (class? dt) (str "final "
                                    (r/typename dt)
                                    " "
                                    (.varName x)
                                    " = ")
                   :default (throw (ex-info
                                    "Invalid type!"
                                    {:type dt}))))
               (.value x)
               ";"])
            tail)
      (body-fn)])

   :default-expr-for-type default-expr-for-type

   :lvar-for-seed core/lvar-str-for-seed

   :counter-to-sym core/counter-to-str

   :local-var-sym core/local-var-str

   :get-compilable-type-signature
   gjvm/get-compilable-type-signature

   :compile-set-local-var
   (fn [state expr cb]
     (let [lvar (.getData expr)
           sym (xp/call :local-var-sym (.getIndex lvar))
           deps (seed/access-compiled-deps expr)
           v (:value deps)]
       (core/set-compilation-result
        state
        [sym " = " v]
        cb)))

   :compile-get-var (fn [state expr cb]
                      (core/set-compilation-result
                       state
                       (xp/call
                        :local-var-sym
                        (-> expr .getData))
                       cb))

   :compile-coll2
   (fn [comp-state expr cb]
     (let [original-coll (.getData expr)
           args (vec
                 (seed/access-compiled-indexed-deps
                  expr))]
       (cond
         (seq? original-coll) (compile-seq comp-state args cb)
         (vector? original-coll) (compile-vec comp-state args cb)
         (set? original-coll) (compile-set comp-state args cb)
         (map? original-coll) (compile-map
                               comp-state
                               args
                               cb))))

   :compile-class
   (fn [comp-state expr cb]
     (cb (seed/compilation-result comp-state
           "null"          
           )))

   :compile-static-value
   (fn [state expr cb]
     (cb (seed/compilation-result
           state (-> expr .getData str))))

   :make-void make-void

   :compile-nothing (core/constant-code-compiler [])
   
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

   :make-nil #(core/nil-of % java.lang.Object)

   :compile-local-var-seed
   (fn [state seed cb]
     (let [lvar (.getData seed)
           sym (xp/call :local-var-sym (.getIndex lvar))
           java-type (-> lvar .getType .get)
           init-value (default-expr-for-type java-type)]
       (if (class? java-type)
         [(r/typename java-type) sym " = "
          init-value ";"
          (cb (seed/compilation-result
                state ::declare-local-var))]
         (throw (ex-info "Not a Java class"
                         {:java-type java-type
                          :seed seed
                          :lvar lvar})))))

   :compile-if
   (core/wrap-expr-compiler
    (fn [expr]
      (let [deps (seed/access-compiled-deps expr)]
        (render-if (:cond deps)
                   (:on-true deps)
                   (:on-false deps)))))

   :compile-bind-name to-java-identifier

   :compile-return-value
   (fn [datatype expr]
     (if (nil? datatype)
       "return /*nil datatype*/;"
       ["return " expr ";"]))

   :compile-nil?
   (fn [comp-state expr cb]
     (cb (seed/compilation-result comp-state
           (wrap-in-parens
            [(-> expr sd/access-compiled-deps :value)
             "== null"]))))

   :binary-add (partial call-operator "+")
   :unary-add (partial call-operator "+")
   :binary-div (partial call-operator "/")
   :binary-sub (partial call-operator "-")
   :binary-mul (partial call-operator "*")
   :negate (partial call-operator "-")
   :not (partial call-operator "!")

   :quot (partial call-method :static "quotient" clojure.lang.Numbers)
   :rem (partial call-method :static "remainder" clojure.lang.Numbers)

   :== (cmp-operator "==")
   :<= (cmp-operator "<=")
   :>= (cmp-operator ">=")
   :< (cmp-operator "<")
   :> (cmp-operator ">")
   :!= (cmp-operator "!=")

   ;;; Bitwise
   :bit-not (partial call-operator "~")
   :bit-shift-left (partial call-operator "<<")
   :unsigned-bit-shift-left (partial call-operator "<<<")
   :bit-shift-right (partial call-operator ">>")
   :unsigned-bit-shift-right (partial call-operator ">>>")
   :bit-and (partial call-operator "&")
   :bit-flip (partial call-operator "^")
   :bit-or (partial call-operator "|")
   

   :make-array make-array-from-size
   :aget get-array-element
   :aset set-array-element
   :alength array-length

   :conj
   (fn [dst x]
     (call-method :static :pure
      "conj"
      clojure.lang.RT
      (cast-any-to-seed clojure.lang.IPersistentCollection dst)
      (cast-any-to-seed java.lang.Object x)))

   :first (collection-op "first")
   :rest (collection-op "more")
   :count (collection-op "count")
   :seq (collection-op "seq")

   := clj-equiv

   :iterable iterable

   :compile-nil
   (fn [comp-state expr cb]
     (cb (seed/compilation-result comp-state "null")))

   :cast cast-any-to-seed

   :unwrap unpack

   :finite? (numeric-class-method "isFinite")
   :infinite? (numeric-class-method "isInfinite")
   :nan? (numeric-class-method "isNaN")

   :basic-random (partial call-method :static "random" java.lang.Math)

   :call-method call-method


   ;; Default types for this platform
   :size-type (constantly Integer/TYPE)
   :float-type (constantly Double/TYPE)
   :int-type (constantly Integer/TYPE)

   :error throw-error

   :compile-recur compile-recur
   :compile-loop compile-loop2
   }))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  User terface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro typed-defn
  "Create a callable function from Geex code. See unit tests for examples."
  [& args0]
  (let [args (merge (parse-typed-defn-args args0)
                    {:ns (str *ns*)})
        arglist (:arglist args)
        package-name (java-package-name args)
        class-name (java-class-name args)
        fn-name (:name args)
        arg-names (mapv :name (:arglist args))
        body-expr (make-typed-defn-body-fn
                   arglist
                   (quote-args arglist)
                   (:body args))
        body-fn (clojure.core/eval
                 body-expr)
        [code log final-state] (generate-typed-defn
                 package-name
                 class-name
                 body-fn
                 (mapv eval-arg-type arglist))
        disp-time? (.hasFlag final-state :disp-time)
        seed-count (.getSeedCount final-state)]
    `(do
       (let [obj# (janino-cook-and-load-object
                   ~(full-java-class-name args)
                   ~code)]
         ~(if disp-time?
            `(let [log# (timelog/log ~log "Compiled it")]
               (println "--- Time report ---")
               (timelog/disp log#)
               (println "\nNumber of seeds:" ~seed-count)
               (println "Time per seed:" (/ (timelog/total-time log#)
                                            ~seed-count)))
            nil)
         (defn ~fn-name [~@arg-names]
           (.apply obj# ~@arg-names))))))

(defn eval-body-fn [body-fn]
  (let [tmp-name (str (gensym "Eval"))
        fg (core/full-generate
            [{:platform :java}]
            (core/return-value (body-fn)))
        code (:result fg)
        cs (:state fg)
        all-code ["public class " tmp-name " {"
                  "/* Static code */"
                  (core/get-top-code cs)
                  "/* Methods */"
                  ["public " (return-type-signature fg)
                   " eval() {"
                   code
                   "}"]
                  "}"]
        formatted (format-nested-show-error all-code)
        _ (when (:disp-final-source (:final-state fg))
            (println formatted))
        obj (janino-cook-and-load-object
             tmp-name formatted)]
    (.eval obj)))

(defmacro eval
  "Evaluate geex code"
  [& args]
  `(eval-body-fn (fn [] ~@args)))

