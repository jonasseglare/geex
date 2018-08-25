(ns geex.java

  "Generation of Java backed code"

  (:require [geex.java.defs :as jdefs]
            [geex.core :as geex]
            [bluebell.utils.debug :as debug]
            [geex.core.defs :as defs]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.typesystem :as ts]
            [bluebell.utils.setdispatch :as setdispatch]
            [geex.core :as core]
            [geex.core.exprmap :as exprmap]
            [bluebell.utils.specutils :as specutils]
            [bluebell.utils.core :as utils]
            [geex.core.seed :as sd]
            [bluebell.utils.defmultiple :refer [defmultiple-extra]]
            [geex.core.exprmap :as exm]
            [geex.core.jvm :as gjvm]
            [geex.core.stringutils :as su :refer [wrap-in-parens compact]]
            [bluebell.tag.core :as tg]
            [geex.core.xplatform :as xp]
            [clojure.reflect :as r]
            [geex.core.datatypes :as dt]
            [clojure.string :as cljstr]
            [geex.core.seedtype :as seedtype]
            [bluebell.utils.party.coll :as partycoll]
            
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
(spec/def ::call-method-args (spec/cat :opts (spec/? map?)
                                       :name string?
                                       :dst any?
                                       :args (spec/* any?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Declarations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare unpack)
(declare call-method)
(declare call-static-method)
(declare make-static-method)
(declare unbox)
(declare box)
(declare j-nth)
(declare j-first)
(declare j-next)
(declare j-count)
(declare j-val-at)
(declare call-operator)
(declare call-operator-with-ret-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
(defn str-to-java-identifier [& args]
  (-> (cljstr/join "_" args)
      (cljstr/replace "_" "__")
      (cljstr/replace "-" "_d")
      (cljstr/replace ":" "_c")
      (cljstr/replace "/" "_s")
      (cljstr/replace "." "_p")
      (cljstr/replace "?" "_q")))


(setdispatch/def-dispatch to-java-identifier ts/system ts/feature)

(setdispatch/def-set-method to-java-identifier [[:symbol x]]
  (str-to-java-identifier (name x)))

(setdispatch/def-set-method to-java-identifier [[:string x]]
  (str-to-java-identifier x))






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
  (reduce join-args2 args))

(defn make-arg-list [parsed-args]
  (reduce join-args2 (map make-arg-decl parsed-args)))

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
  (.formatSource (Formatter.) src))

(defn generate-typed-defn [args]
  (let [arglist (:arglist args)
        quoted-args (mapv quote-arg-name arglist)]
    `(let [fg# (geex/full-generate
                         [{:platform :java}]
                         (core/return-value (apply
                                             (fn [~@(map :name arglist)]
                                               ~@(:body args))
                                             (map to-binding ~quoted-args))))
           top# (:expr fg#)
           code# (:result fg#)
           cs# (:comp-state fg#)
           all-code# [[{:prefix " "
                        :step ""}
                       "package " ~(java-package-name args) ";"]
                      ~(str "public class " (java-class-name args) " {")
                      "/* Static code */"
                      (exprmap/get-static-code cs#)
                      "/* Methods */"
                      ["public " (r/typename (gjvm/get-type-signature top#))
                       " apply("
                       (make-arg-list ~quoted-args)
                       ") {"
                       code#
                       "}"]
                      "}"]]
       (format-source (utils/indent-nested
                          all-code#))
       #_(try
         
         #_(catch Throwable e#
           (throw (ex-info "Failed to render Java code from nested structure"
                           {:structure all-code#
                            :reason e#})))))))

(defn contains-debug? [args]
  (some (tg/tagged? :debug) (:meta args)))

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
  (println "Render var init for type=" tp " name=" name)
  ;(assert (not= name "gs_dsym_d147"))
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
      (r/typename (seed/datatype expr))
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
  (join-args (map (fn [arg] ["(java.lang.Object)(" arg ")"]) args)))

(defn make-vec-expr [args]
  [compact
   "clojure.lang.PersistentVector.adopt(new java.lang.Object[]{"
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
          (str "new " (-> expr
                          seed/access-seed-data
                          :component-class
                          r/typename) "[" (-> expr seed/access-compiled-deps :size) "]")]))))

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
    (println "Loop binding var init")
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

(defn make-tmp-step-assignment [src dst]
  (println "tmp-step-assignment")
  (render-var-init (-> dst sd/datatype r/typename)
                   (to-java-identifier (::tmp-var dst))
                   src))

(defn make-final-step-assignment [dst]
  (println "Make final step assignment")
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
  (let [method-name (:method-name info)
        {:keys [args arg-types]} (preprocess-method-args args0)
        method (.getMethod cl method-name arg-types)]
    (println "Call-static-method" method-name)
    (geex/with-new-seed
      "call-static-method"
      (fn [x]
        (println "Initial deps for static method is" (keys (::defs/deps x)))
        (-> x
            (sd/datatype (.getReturnType method))
            (defs/access-class cl)
            (sd/mark-dirty (:dirty? info))
            (sd/access-indexed-deps args)
            (sd/compiler compile-call-static-method)
            (defs/access-method-name method-name))))))

(defn call-static-method [& method-args]
  (let [args (specutils/force-conform
              ::call-method-args method-args)]
    (call-static-method-sub (merge
                             {:method-name (:name args)
                              :dirty? true}
                             (:opts args))
                            (:dst args)
                            (:args args))))

(def call-static-pure-method (partial call-static-method
                                      {:dirty? false}))

(defn make-static-method
  ([opts method-name cl]
   (partial call-static-method opts method-name cl))
  ([method-name cl]
   (partial call-static-method method-name cl)))

(def clj-equiv (make-static-method
                {:dirty? false}
                "equiv" clojure.lang.Util))

(defn call-method-sub [info obj0 args0]
  (let [method-name (:method-name info)
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
            sd/mark-dirty
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

        ;; TODO: Right now, we evaluate a Clojure function
        ;; to infer return type, which is not so accurate...
        ret-type (dt/query-return-type (:clojure-fn op-info)
                                       arg-types)
        
        _ (utils/data-assert (not (nil? ret-type))
                             "Cannot infer return type for operator and types"
                             {:operator operator
                              :arg-types arg-types})]
    (make-call-operator-seed ret-type operator args)))

(defn call-operator-with-ret-type [ret-type operator & args0]
  (let [args (map core/to-seed args0)]
    (make-call-operator-seed ret-type operator args)))

(defn box [x0]
  (let [x (core/to-seed x0)
        tp (seed/datatype x)]
    (if (dt/unboxed-type? tp)
      (call-static-method "valueOf" (dt/box-class tp) x)
      x)))

(defn unbox [x0]
  (let [x (core/to-seed x0)
        tp (seed/datatype x)]
    (if (dt/unboxed-type? tp)
      x
      (let [unboxed-type (dt/unbox-class tp)]
        (call-method (str (.getName unboxed-type) "Value")
                     x)))))

(defn call-method [& method-args]
  (let [args (specutils/force-conform
              ::call-method-args method-args)]
    (call-method-sub (merge
                      {:method-name (:name args)
                       :dirty? true}
                      (:opts args))
                     (:dst args)
                     (:args args))))

(def call-pure-method (partial call-method {:dirty? false}))

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
        debug? (contains-debug? args)]
    `(do
       ~@(when debug?
           [`(println ~code)])
       (binding [core/debug-full-graph ~debug?]
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
    (call-static-pure-method
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
             (let [[key name] (cond
                                (string? sp) [(keyword sp) sp]
                                (vector? sp) sp
                                :default
                                (throw (ex-info
                                        "Invalid arg spec"
                                        {:data sp})
                                       ))]
               [(keyword name)
                (partial call-static-pure-method name cl)]))
           names)))
  
(defn java-math-fns [names]
  (pure-static-methods java.lang.Math names))
  
(xp/register
 :java
 (merge
  (java-math-fns ["sqrt"])
  {:render-bindings
   (fn [tail body]
     [
      (mapv (fn [x]
              [su/compact
               (let [dt (seed/datatype (:seed x))]
                 (if (nil? dt)
                   []
                   (do
                     (println "Rendering-bindings " (:name x))
                     
                     #_(assert (not= (:name x)
                                   "gs_dgs_dcall_dstatic_dmethod_d181_d279"))
                     (str (r/typename dt)
                          " "
                          (:name x)
                          " = "))))
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

   :compile-pack-var
   (fn [comp-state expr cb]
     (let [r (sd/access-compiled-deps expr)
           lhs (var-name-java-sym expr)
           rhs (:expr r)]
       (println "Pack var lhs=" lhs " rhs=" rhs)
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
   :binary-div (partial call-operator "/")
   :binary-sub (partial call-operator "-")
   :binary-mul (partial call-operator "*")
   :negate (partial call-operator "-")
   :not (partial call-operator "!")

   :quot (partial call-static-method "quotient" clojure.lang.Numbers)
   :rem (partial call-static-method "remainder" clojure.lang.Numbers)

   :== (cmp-operator "==")
   :<= (cmp-operator "<=")
   :>= (cmp-operator ">=")
   :< (cmp-operator "<")
   :> (cmp-operator ">")
   :!= (cmp-operator "!=")

   :make-array make-array-from-size
   :aget get-array-element
   :aset set-array-element
   :alength array-length

   :conj
   (fn [dst x]
     (call-static-pure-method
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

    (typed-defn eq-ints2 [seedtype/int a
                          seedtype/int b]
                (call-operator "==" a b))

    

    

    

    

    
    

    

    

    


    

    
    )


  )
