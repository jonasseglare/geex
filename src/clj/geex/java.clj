(ns geex.java

  "Generation of Java backed code"

  (:require [geex.java.defs :as jdefs]
            [geex.core :as geex]
            [bluebell.utils.wip.debug :as debug]
            [geex.core.defs :as defs]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [clojure.pprint :as pp]
            [geex.core.typesystem :as ts]
            [bluebell.utils.wip.setdispatch :as setdispatch]
            [geex.core :as core]
            [geex.core.exprmap :as exprmap]
            [bluebell.utils.wip.specutils :as specutils]
            [bluebell.utils.wip.core :as utils]
            [geex.core.seed :as sd]
            [bluebell.utils.wip.defmultiple :refer [defmultiple-extra]]
            [geex.core.exprmap :as exm]
            [geex.core.jvm :as gjvm]
            [geex.core.stringutils :as su :refer [wrap-in-parens compact]]
            [bluebell.utils.wip.tag.core :as tg]
            [geex.core.xplatform :as xp]
            [clojure.reflect :as r]
            [geex.core.datatypes :as dt]
            [clojure.string :as cljstr]
            [geex.core.seedtype :as seedtype]
            [bluebell.utils.wip.party.coll :as partycoll]
            
            )
  
  (:import [org.codehaus.janino SimpleCompiler]
           [com.google.googlejavaformat.java Formatter]))

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
(declare unbox)
(declare box)
(declare j-nth)
(declare j-first)
(declare j-next)
(declare j-count)
(declare j-val-at)
(declare call-operator)
(declare call-method-sub)
(declare call-static-method-sub)
(declare call-operator-with-ret-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn seed-typename [x]
  {:pre [(sd/seed? x)]}
  (let [dt (sd/datatype x)]
    (assert (class? dt))
    (r/typename dt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Unpacking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn compile-cast [comp-state expr cb]
  (cb (defs/compilation-result
        comp-state
        (wrap-in-parens
         ["(" (.getName (sd/datatype expr)) ")"
          (-> expr
              defs/access-compiled-deps
              :value)]))))

(defn cast-seed [type value]
  {:pre [(sd/seed? value)]}
  (if (and (dt/unboxed-type? type)
           (not (dt/unboxed-type? (sd/datatype value)))) 
    (unbox (cast-seed (dt/box-class type) value))
    (geex/with-new-seed
      "cast-seed"
      (fn [seed]
        (-> seed
            (sd/add-deps {:value value})
            (sd/compiler compile-cast)
            (sd/datatype type))))))

(def compile-void (core/wrap-expr-compiler (fn [_] "/*void*/")))

(defn make-void []
  (geex/with-new-seed
    "void"
    (fn [seed]
      (-> seed
          (sd/datatype Void/TYPE)
          (sd/access-bind? false)
          (sd/compiler compile-void)))))

(defn cast-any-to-seed [type x]
  (cast-seed type (core/to-seed x)))


;; The difference is that if src-seed is already a subtype of dst-seed, then no cast will take place.
(defn unpack-to-seed [dst-seed src-seed]
  (assert (sd/seed? src-seed))
  (assert (sd/seed? dst-seed))
  (let [dst-type (defs/datatype dst-seed)]
    (if (isa? (defs/datatype src-seed) dst-type) src-seed
      (cast-seed dst-type src-seed))))

(defn unpack-to-vector [dst-type src-seed]
  (mapv (fn [index dst-element-type]
          (unpack dst-element-type (j-nth src-seed (int index))))
        (range (count dst-type))
        dst-type))

(defn unpack-to-seq [dst-type src-seed]
  (second
   (reduce
    (fn [[src-seq dst] element-type]
      [(unpack-to-seed (sd/typed-seed clojure.lang.ISeq)
                       (j-next src-seq))
       (conj dst (unpack element-type (j-first src-seq)))])
    [src-seed '()]
    dst-type)))

(defn unpack-to-map [dst-type src-seed]
  (into {} (map (fn [[k v]]
                  [k (unpack v (j-val-at src-seed (cast-seed
                                                   java.lang.Object
                                                   (core/to-seed k))))])
                dst-type)))

(defn unpack [dst-type src-seed]
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

(defn make-marker [col]
  (str (apply str (take col (repeat " ")))
       "^ ERROR HERE!"))

(defn point-at-error [source-code location]
  {:pre [(string? source-code)
         (instance? org.codehaus.commons.compiler.Location
                    location)]}
  (if (nil? location)
    source-code

    (cljstr/join
     "\n"
     (utils/insert-at (cljstr/split-lines source-code)
                      (.getLineNumber location)
                      [(make-marker
                        (dec (.getColumnNumber location)))]))))

(defn janino-cook-and-load-class [class-name source-code]
  "Dynamically compile and load Java code as a class"
  [class-name source-code]
  (try
    (let [sc (SimpleCompiler.)]
      (.cook sc source-code)
      (.loadClass (.getClassLoader sc) class-name))
    (catch org.codehaus.commons.compiler.CompileException e
      (let [location (.getLocation e)
            marked-source-code (point-at-error source-code location)]
        (println marked-source-code)
        (throw (ex-info "Failed to compile code"
                        {:code marked-source-code
                         :location location
                         :exception e}))))))

;; Either we load it dynamically, or we load it from disk.
(defn janino-cook-and-load-object  [class-name source-code]
  (.newInstance (janino-cook-and-load-class
                 class-name
                 source-code)))

(defn parse-typed-defn-args [args0]
  (specutils/force-conform ::jdefs/defn-args args0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Identifiers on Java
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn special-char-to-escaped [x]
  (case x
    \: "_c"
    \- "_d"
    \_ "__"
    \/ "_s"
    \. "_p"
    \? "_q"
    (str x)))

(defn str-to-java-identifier [& args]
  (->> args
       (cljstr/join "_")
       vec
       (map special-char-to-escaped)
       (apply str)))

(setdispatch/def-dispatch to-java-identifier ts/system ts/feature)

(setdispatch/def-set-method to-java-identifier [[:symbol x]]
  (str-to-java-identifier (name x)))

(setdispatch/def-set-method to-java-identifier [[:string x]]
  (str-to-java-identifier x))



(defn seed-or-class? [x]
  (or (sd/seed? x)
      (class? x)))

(defn class-to-typed-seed [x]
  (if (class? x)
    (sd/typed-seed x)
    x))

(defn import-type-signature [x]
  (second
   (core/flat-seeds-traverse
    seed-or-class?
    x
    (comp sd/strip-seed class-to-typed-seed))))





(defn java-class-name [parsed-args]
  (-> parsed-args
      :name
      name
      str-to-java-identifier))



(defn java-package-name [parsed-args]
  (-> parsed-args
      :ns
      str-to-java-identifier))

(defn full-java-class-name [parsed-args]
  (str (java-package-name parsed-args)
       "."
       (java-class-name parsed-args)))



(defn quote-arg-name [arg]
  (assert (map? arg))
  (merge arg
         {:name `(quote ~(:name arg))}))

(defn make-arg-decl [parsed-arg]
  (let [tp (:type parsed-arg)]
    [{:prefix " "
      :step ""}
     (r/typename (gjvm/get-type-signature tp))
     (to-java-identifier (:name parsed-arg))
     ]))

(defn join-args2
  ([]
   nil)
  ([c0 c1]
   (if (nil? c0)
     c1
     (into [] [c0 [", "] c1]))))

(defn join-args [args]
  (or (reduce join-args2 args) []))

(defn make-arg-list [parsed-args]
  (or (reduce join-args2 (map make-arg-decl parsed-args)) []))

(defn find-member-info [cl member-name0]
  (assert (class? cl))
  (let [member-name (symbol member-name0)]
    (->> cl
         clojure.reflect/reflect
         :members
         (filter #(= (:name %) member-name)))))

(defn compile-call-method [comp-state expr cb]
  (cb
   (defs/compilation-result
     comp-state
     (wrap-in-parens
      [(:obj (sd/access-compiled-deps expr))
       "."
       (defs/access-method-name expr)
       (let [dp (sd/access-compiled-indexed-deps expr)]
         (wrap-in-parens (join-args dp)))]))))

(defn compile-call-static-method [comp-state expr cb]
  (cb
   (defs/compilation-result
     comp-state
     (wrap-in-parens
      [(.getName (defs/access-class expr))
       "."
       (defs/access-method-name expr)
       (let [dp (sd/access-compiled-indexed-deps expr)]
         (wrap-in-parens (join-args dp)))]))))

;; (supers (class (fn [x] (* x x))))
;; #{java.lang.Runnable java.util.Comparator java.util.concurrent.Callable clojure.lang.IObj java.io.Serializable clojure.lang.AFunction clojure.lang.Fn clojure.lang.IFn clojure.lang.AFn java.lang.Object clojure.lang.IMeta}
(defn to-binding [quoted-arg]
  (let [tp (:type quoted-arg)
        t (gjvm/get-type-signature tp)]
    ;;; TODO: Get the type, depending on what...
    (unpack
     
     ;; The actual type used by us:
     tp 

     ;; A seed holding the raw runtime value
     (core/bind-name t (:name quoted-arg)))))

(defn format-source [src]
  (try
    (.formatSource (Formatter.) src)
    (catch Exception e
      (println "Failed to format this:")
      (println src)
      (throw e))))

(defn append-void-if-empty [x]
  {:pre [(or (sequential? x)
             (nil? x))]}
  (if (empty? x)
    `((make-void))
    x))

(defn quote-args [arglist]
  (mapv quote-arg-name arglist))

(def format-nested (comp format-source utils/indent-nested))

(defn return-type-signature [fg]
  (-> fg
      :expr
      gjvm/get-type-signature
      r/typename))

(defn generate-typed-defn [args]
  (let [arglist (:arglist args)
        quoted-args (quote-args arglist)]
    `(let [fg# (geex/full-generate
                         [{:platform :java}]
                         (core/return-value
                          (apply
                           (fn [~@(map :name arglist)]
                             ~@(append-void-if-empty
                                (:body args)))

                           ;; Unpacking happens here
                           (map to-binding ~quoted-args))))
           code# (:result fg#)
           cs# (:comp-state fg#)
           all-code# [[{:prefix " "
                        :step ""}
                       "package " ~(java-package-name args) ";"]
                      ~(str "public class " (java-class-name args) " {")
                      "/* Static code */"
                      (exprmap/get-static-code cs#)
                      "/* Methods */"
                      ["public " (return-type-signature fg#)
                       " apply("
                       (make-arg-list ~quoted-args)
                       ") {"
                       code#
                       "}"]
                      "}"]]
       (try
         (format-nested all-code#)
         (catch Throwable e#
           (println "The input code")
           (pp/pprint all-code#)
           (throw e#)))
       #_(try
         
         #_(catch Throwable e#
           (throw (ex-info "Failed to render Java code from nested structure"
                           {:structure all-code#
                            :reason e#})))))))

(defn preprocess-method-args [args0]
  (let [args (mapv geex/to-seed args0)
        arg-types (into-array java.lang.Class (mapv sd/datatype args))]
    (utils/map-of args arg-types)))

(defn compile-operator-call [comp-state expr cb]
  (let [args (sd/access-compiled-indexed-deps expr)
        op (defs/access-operator expr)]
    (cb (defs/compilation-result
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

(defn render-var-init [tp name val]
  [tp " " name " = " val ";"])

(defn bind-statically [comp-state binding-type binding-name binding-value]
  (defs/compilation-result
    (exprmap/add-static-code
     comp-state
     [compact "static " (render-var-init binding-type
                                         binding-name
                                         binding-value)])
    binding-name))

(defn escape-char [x]
  (or (char-escape-string x) x))

(defn java-string-literal [s]
  (str "\"" (apply str (map escape-char s)) "\""))

(defn compile-interned [comp-state expr cb]
  (let [data (sd/access-seed-data expr)
        kwd (:value data)
        tp (:type data)]
    (cb
     (bind-statically
      comp-state
      (seed-typename expr)
      (str-to-java-identifier (core/contextual-genstring (str tp "_" kwd)))
      [(str "clojure.lang." tp ".intern(")
       (let [kwdns (namespace kwd)]
         (if (nil? kwdns)
           []
           [(java-string-literal kwdns)
            ", "]))
       (java-string-literal (name kwd)) ")"]))))

(defn compile-string [comp-state expr cb]
  (cb
   (defs/compilation-result
     comp-state
     (java-string-literal (sd/access-seed-data expr)))))

(defn make-seq-expr [args]
  [compact
   "clojure.lang.PersistentList.EMPTY"
   (map (fn [arg]
          [".cons((java.lang.Object)(" arg "))"])
        (reverse args))])

(defn object-args [args]
  (or (join-args (map (fn [arg] ["(java.lang.Object)(" arg ")"]) args))
      []))

(defn make-vec-expr [args]
  [compact
   "clojure.lang.PersistentVector.create(new java.lang.Object[]{"
   (object-args args)
   "})"])

(defn make-map-expr [args]
  [compact
   "clojure.lang.PersistentHashMap.create("
   (object-args args)
   ")"])

(defn make-set-expr [args]
  [compact
   "clojure.lang.PersistentHashSet.create("
   (object-args args)
   ")"])

(defn compile-seq [comp-state args cb]
  (cb (defs/compilation-result comp-state (make-seq-expr args))))

(defn compile-vec [comp-state args cb]
  (cb (defs/compilation-result comp-state (make-vec-expr args))))

(defn compile-map [comp-state args cb]
  (cb (defs/compilation-result comp-state (make-map-expr args))))

(defn compile-set [comp-state args cb]
  (cb (defs/compilation-result comp-state (make-set-expr args))))

(defn compile-array-from-size [comp-state expr cb]
  (cb (defs/compilation-result
        comp-state
        (wrap-in-parens
         [compact
          "new " (-> expr
                     seed/access-seed-data
                     :component-class
                     r/typename) "["
          (-> expr seed/access-compiled-deps :size) "]"]))))

(def compile-set-array (core/wrap-expr-compiler
                        (fn [expr]
                          (let [deps (seed/access-compiled-deps expr)]
                            [(:dst deps) "[" (:index deps) "] = " (:value deps)]))))

(def compile-get-array (core/wrap-expr-compiler
                        (fn [expr]
                          (let [deps (seed/access-compiled-deps expr)]
                            (wrap-in-parens [(:src deps) "[" (:index deps) "]"])))))

(def compile-array-length (core/wrap-expr-compiler
                           (fn [expr]
                             (let [deps (seed/access-compiled-deps expr)]
                               (wrap-in-parens [compact (:src deps) ".length"])))))

(defn render-if [condition true-branch false-branch]
  ["if (" condition ") {"
   true-branch
   "} else {"
   false-branch
   "}"])

(def var-name-java-sym (comp to-java-identifier
                             :name
                             :var))

(defn make-loop-binding [comp-state lvar-key]
  (assert (keyword? lvar-key))
  (let [lvar (exm/get-seed comp-state lvar-key)
        dep (:value (exm/get-compiled-deps comp-state lvar))]
    (render-var-init
     (-> lvar sd/datatype r/typename)
     (-> lvar core/access-bind-symbol to-java-identifier)
     dep)))

(defn bind-java-identifier [expr]
  (-> expr
      core/access-bind-symbol
      to-java-identifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Compile a return value
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-assign [comp-state expr cb]
  (cb
   (defs/compilation-result
     comp-state
     (let [v (-> expr defs/access-compiled-deps :value)]
       [(:dst-name expr) " = " v]))))

(defn assign [dst-var-name src]
  {:pre [(string? dst-var-name)]}
  (core/with-new-seed
    "assign"
    (fn [s]
      (-> s
          (defs/datatype nil)
          (defs/access-deps {:value src})
          (sd/mark-dirty true)
          (assoc :dst-name dst-var-name)
          (sd/compiler compile-assign)))))


(defn make-tmp-step-assignment [src dst]
  (render-var-init (-> dst sd/datatype r/typename)
                   (to-java-identifier (::tmp-var dst))
                   src))

(defn make-final-step-assignment [dst]
  [(bind-java-identifier dst) " = " (to-java-identifier (::tmp-var dst)) ";"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Basic platform operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cmp-operator [op]
  (partial
   call-operator-with-ret-type
   Boolean/TYPE
   op))

(defn call-static-method-sub [info cl args0]
  {:pre [(class? cl)]}
  (let [method-name (:name info)
        {:keys [args arg-types]} (preprocess-method-args args0)
        method (.getMethod cl method-name arg-types)]
    (geex/with-new-seed
      "call-static-method"
      (fn [x]
        (-> x
            (sd/datatype (.getReturnType method))
            (defs/access-class cl)
            (sd/mark-dirty (:dirty? info))
            (sd/access-indexed-deps args)
            (sd/compiler compile-call-static-method)
            (defs/access-method-name method-name))))))

(defn make-method-info [parsed-method-args]
  (let [dirs (:directives parsed-method-args)]
    (merge
     {:dirty? (not (contains? dirs :pure))
      :name (:name parsed-method-args)})))


(defn call-method-sub [info obj0 args0]
  (let [method-name (:name info)
        obj (geex/to-seed obj0)
        {:keys [args arg-types]} (preprocess-method-args args0)
        cl (sd/datatype obj)
        method (.getMethod cl method-name arg-types)]
    (geex/with-new-seed
      "call-method"
      (fn [x]
        (-> x
            (sd/datatype (.getReturnType method))
            (sd/add-deps {:obj obj})
            (sd/access-indexed-deps args)
            (sd/compiler compile-call-method)
            (sd/mark-dirty (:dirty? info))
            (defs/access-method-name method-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-array-from-size [component-class size]
  {:pre [(class? component-class)]}
  (core/with-new-seed
    "array-seed"
    (fn [x]
      (-> x
          (sd/access-seed-data {:component-class component-class})
          (sd/datatype (class (make-array component-class 0)))
          (sd/add-deps {:size size})
          (sd/compiler compile-array-from-size)))))

(defn set-array-element [dst-array index value]
  (core/with-new-seed
    "array-set"
    (fn [x]
      (-> x
          (sd/datatype nil)
          (sd/add-deps {:dst dst-array
                        :index index
                        :value value})
          (sd/mark-dirty true)
          (sd/compiler compile-set-array)))))

(defn get-array-element [src-array index]
  (core/with-new-seed
    "array-get"
    (fn [x]
      (-> x
          (sd/datatype (.getComponentType (sd/datatype src-array)))
          (sd/add-deps {:src src-array
                        :index index})
          (sd/mark-dirty true)
          (sd/compiler compile-get-array)))))

(defn array-length [src-array]
  (core/with-new-seed
    "array-length"
    (fn [x]
      (-> x
          (sd/datatype java.lang.Integer/TYPE)
          (sd/add-deps {:src src-array})
          (sd/mark-dirty true)
          (sd/compiler compile-array-length)))))

(defn make-call-operator-seed [ret-type operator args]
  (core/with-new-seed
    "operator-call"
    (fn [x]
      (-> x
          (sd/datatype ret-type)
          (sd/access-indexed-deps args)
          (defs/access-operator operator)
          (sd/compiler compile-operator-call)))))

(defn call-operator [operator & args0]
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
    (make-call-operator-seed ret-type operator args)))

(defn call-operator-with-ret-type [ret-type operator & args0]
  (let [args (map core/to-seed args0)]
    (make-call-operator-seed ret-type operator args)))

(defn parse-method-args [method-args]
  (update (specutils/force-conform
           ::call-method-args method-args)
          :directives set))

(defn call-method [& method-args]
  (let [args (parse-method-args method-args)]
    ((if (contains? (:directives args) :static)
       call-static-method-sub
       call-method-sub)
     (make-method-info args)
     (:dst args)
     (:args args))))


(defn box [x0]
  (let [x (core/to-seed x0)
        tp (seed/datatype x)]
    (if (dt/unboxed-type? tp)
      (call-method :static "valueOf" (dt/box-class tp) x)
      x)))

(defn unbox [x0]
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

(defmacro typed-defn [& args0]
  (let [args (merge (parse-typed-defn-args args0)
                    {:ns (str *ns*)})
        code (generate-typed-defn args)
        arg-names (mapv :name (:arglist args))
        meta-args (set (:meta args))
        debug? (:print-source meta-args)
        show-graph? (:show-graph meta-args)]
    `(do
       ~@(when debug?
           [`(println ~code)])
       (binding [core/debug-full-graph ~show-graph?]
         (let [obj# (janino-cook-and-load-object ~(full-java-class-name args)
                                                 ~code)]       
           (defn ~(:name args) [~@arg-names]
             (.apply obj# ~@arg-names)))))))

(defmacro disp-ns []
  (let [k# *ns*]
    k#))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implement common methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn collection-op [name]
  (fn [src]
    (call-method
     :static :pure
     name
     clojure.lang.RT
     (cast-any-to-seed java.lang.Object src))))

(defn seq-iterable [src]
  (xp/call :seq (core/wrap src)))

(ts/def-default-set-method iterable [[:any x]]
  x)

(setdispatch/def-set-method iterable
  [[[:seed java.lang.Object] src]]
  (seq-iterable src))

(setdispatch/def-set-method iterable
  [[[:seed clojure.lang.IPersistentVector] src]]
  (seq-iterable src))





  (defn pure-static-methods [cl names]
    (into {}
          (map
           (fn [sp]
             (let [[key name arg-count] sp]
               [(keyword name)
                (partial call-static-pure-method name cl)]))
           names)))
  
(defn java-math-fns [names]
  (pure-static-methods java.lang.Math names))

(defn numeric-class-method [method-name]
  (fn [x0]
    (let [x (core/wrap x0)
          primitive-cl (seed/datatype x)
          cl (or (get dt/unboxed-to-boxed-map primitive-cl)
                 primitive-cl)]
      (call-static-pure-method method-name cl x))))

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
  {:render-bindings
   (fn [tail body]
     [
      (mapv (fn [x]
              [su/compact
               (let [dt (seed/datatype (:seed x))]
                 (if (nil? dt)
                   []
                   (str (r/typename dt)
                        " "
                        (:name x)
                        " = ")))
               (:result x)
               ";"])
            tail)
      body
      ])

   :to-variable-name to-java-identifier

   :get-type-signature gjvm/get-type-signature

   :compile-coll
   (fn [comp-state expr cb]
     (let [original-coll (core/access-original-coll expr)
           args (partycoll/normalized-coll-accessor
                 (exm/lookup-compiled-indexed-results comp-state expr))]
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
     (cb (defs/compilation-result comp-state
           "null"          
           )))

   :compile-static-value
   (fn [state expr cb]
     (cb (defs/compilation-result state (-> expr sd/static-value str))))

   :make-void make-void
   
   :keyword-seed
   (fn  [kwd]
     (core/with-new-seed
       "Keyword"
       (fn [s]
         (-> s
             (sd/access-seed-data {:type "Keyword"
                                   :value kwd})
             (defs/datatype clojure.lang.Keyword)
             (defs/compiler compile-interned)))))

   :symbol-seed
   (fn  [sym]
     (core/with-new-seed
       "Symbol"
       (fn [s]
         (-> s
             (sd/access-seed-data {:type "Symbol"
                                   :value sym})
             (defs/datatype clojure.lang.Symbol)
             (defs/compiler compile-interned)))))

   :string-seed
   (fn [x]
     (core/with-new-seed
       "String"
       (fn [s]
         (-> s
             (sd/access-seed-data x)
             (defs/datatype java.lang.String)
             (defs/compiler compile-string)))))

   :declare-local-vars
   (fn [comp-state cb]
     (let [vars (::defs/local-vars comp-state)]
       (if (empty? vars)
         (cb comp-state)

         ;; Generate the code for local variables
         [(transduce
           (comp (map (comp :vars second))
                 cat
                 (map (fn [x]
                        [compact
                         (-> x
                             :type
                             seed/datatype
                             r/typename)
                         " "
                         (-> x :name to-java-identifier)
                         ";"])))
           conj
           []
           vars)
          (cb (assoc comp-state ::defs/local-vars {}))])))


   :render-sequential-code identity

   :make-nil #(core/nil-of java.lang.Object)

   :check-compilation-result check-compilation-result

   :compile-pack-var
   (fn [comp-state expr cb]
     (let [r (sd/access-compiled-deps expr)
           lhs (var-name-java-sym expr)
           rhs (:expr r)]
       (cb (defs/compilation-result
             comp-state
             [compact lhs " = " rhs ";"]))))

   :compile-unpack-var
   (fn [comp-state expr cb]
     (let [r (sd/access-compiled-deps expr)]
       (cb (defs/compilation-result
             comp-state
             (var-name-java-sym expr)))))

   :compile-if
   (core/wrap-expr-compiler
    (fn [expr]
      (let [deps (seed/access-compiled-deps expr)]
        (render-if (:condition deps)
                   (:true-branch deps)
                   (:false-branch deps)))))

   :compile-bind
   (fn [comp-state expr cb]
     (cb (defs/compilation-result
           comp-state (bind-java-identifier expr))))

   :compile-loop
   (fn [comp-state expr cb]
     (cb (defs/compilation-result
           comp-state
           (let [cdeps (defs/access-compiled-deps expr)]
             (render-if (:loop? cdeps)
                        (:next cdeps)
                        [(:result cdeps) "break;"])))))

   :compile-bind-name to-java-identifier

   :compile-step-loop-state
   (fn  [comp-state expr cb]
     (let [flat-src (sd/access-compiled-indexed-deps expr)
           flat-dst (map (fn [dst-seed]
                           (assoc dst-seed ::tmp-var (core/contextual-genstring "tmp")))
                         (core/flatten-expr (:dst expr)))
           ]
       (assert (every? map? flat-dst))
       (assert (= (count flat-src)
                  (count flat-dst)))
       (cb (defs/compilation-result
             comp-state
             [(map make-tmp-step-assignment flat-src flat-dst)
              (map make-final-step-assignment flat-dst)]))))

   :compile-loop-header
   (fn [comp-state expr cb]
     (let [bindings (sd/access-indexed-deps expr)]
       [(mapv (partial  make-loop-binding comp-state) bindings)
        "while (true) {"
        (cb (defs/compilation-result
              comp-state
              (-> expr
                  defs/access-compiled-deps
                  :wrapped)))
        "}"]))

   :compile-return-value
   (fn [datatype expr]
     (if (nil? datatype)
       "return /*nil datatype*/;"
       ["return " expr ";"]))

   :compile-nil?
   (fn [comp-state expr cb]
     (cb (defs/compilation-result comp-state
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
     (cb (defs/compilation-result comp-state "null")))

   :cast cast-any-to-seed

   :unwrap unpack

   :finite? (numeric-class-method "isFinite")
   :infinite? (numeric-class-method "isInfinite")
   :nan? (numeric-class-method "isNaN")

   :basic-random (partial call-method :static "random" java.lang.Math)

   :call-method call-method
   
   }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Experiments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comment
  (do

    (typed-defn return-primitive-number [(seed/typed-seed java.lang.Double) x]
                1)


    (typed-defn return-some-class [(seed/typed-seed java.lang.CharSequence) ch]
                ch)

    (typed-defn check-cast :debug [(seed/typed-seed java.lang.Object) obj]
                (unpack (seed/typed-seed java.lang.Double) obj))

    
    
    (typed-defn my-plus3 :debug [seedtype/int a
                                 seedtype/float b]
                (call-operator "+" a b))

    
    (typed-defn make-magic-kwd :debug []
                :kattskit)

    (typed-defn eq-ints2 :print-source [seedtype/int a
                          seedtype/int b]
                (call-operator "==" a b))

    

    

    

    

    
    

    

    

    


    

    
    )


  )
