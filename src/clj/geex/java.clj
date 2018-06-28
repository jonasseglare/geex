(ns geex.java

  "Generation of Java backed code"

  (:require [geex.java.defs :as jdefs]
            [geex.core :as geex]
            [geex.core.defs :as defs]
            [geex.platform.low :as low]
            [geex.platform.high :as high]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.typesystem :as ts]
            [bluebell.utils.setdispatch :as setdispatch]
            [geex.core :as core]
            [geex.core.exprmap :as exprmap]
            [bluebell.utils.specutils :as specutils]
            [bluebell.utils.core :as utils]
            [geex.core.seed :as sd]
            [geex.core.exprmap :as exm]
            [geex.core.stringutils :as su :refer [wrap-in-parens compact]]
            [bluebell.tag.core :as tg]
            [clojure.reflect :as r]
            [geex.core.datatypes :as dt]
            [clojure.string :as cljstr]
            [geex.core.seedtype :as seedtype]
            [bluebell.utils.party.coll :as partycoll])
  (:import [org.codehaus.janino SimpleCompiler]))

;; Lot's of interesting stuff going on here.
;; https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html

(def platform-tag [:platform :java])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Declarations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare unpack)
(declare call-method)
(declare call-static-method)
(declare unbox)
(declare box)
(declare j-nth)
(declare j-first)
(declare j-next)
(declare j-count)
(declare j-val-at)


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
  (if (dt/unboxed-type? type)
    (unbox (cast-seed (dt/box-class type) value))
    (geex/with-new-seed
      "cast-seed"
      (fn [seed]
        (-> seed
            (sd/add-deps {:value value})
            (sd/compiler compile-cast)
            (sd/datatype type))))))


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







(defn janino-cook-and-load-class [class-name source-code]
  "Dynamically compile and load Java code as a class"
  [class-name source-code]
  (try
    (let [sc (SimpleCompiler.)]
      (.cook sc source-code)
      (.loadClass (.getClassLoader sc) class-name))
    (catch Throwable e
      (throw (ex-info "Failed to compile code"
                      {:code source-code
                       :exception e})))))

(defn janino-cook-and-load-object  [class-name source-code]
  (.newInstance (janino-cook-and-load-class
                 class-name
                 source-code)))

(defn parse-typed-defn-args [args0]
  (specutils/force-conform ::jdefs/defn-args args0))

(defn java-class-name [parsed-args]
  (-> parsed-args
      :name
      name
      low/str-to-java-identifier))



(defn java-package-name [parsed-args]
  (-> parsed-args
      :ns
      low/str-to-java-identifier))

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
     (r/typename (low/get-type-signature platform-tag tp))
     (low/to-variable-name platform-tag (:name parsed-arg))
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
        t (low/get-type-signature platform-tag tp)]
    ;;; TODO: Get the type, depending on what...
    (unpack
     
     ;; The actual type used by us:
     tp 

     ;; A seed holding the raw runtime value
     (core/bind-name t (:name quoted-arg)))))

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
                      ["public " (r/typename (low/get-type-signature platform-tag top#))
                       " apply("
                       (make-arg-list ~quoted-args)
                       ") {"
                       code#
                       "}"]
                      "}"]]
       (try
         (utils/indent-nested
          all-code#)
         (catch Throwable e#
           (throw (ex-info "Failed to render Java code from nested structure"
                           {:structure all-code#})))))))

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
(defn bind-statically [comp-state binding-type binding-name binding-value]
  (defs/compilation-result
    (exprmap/add-static-code
     comp-state
     [compact "static " binding-type " " binding-name " = " binding-value ";"])
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
      (low/str-to-java-identifier (core/contextual-genstring (str tp "_" kwd)))
      [(str "clojure.lang." tp ".intern(")
       (let [kwdns (namespace kwd)]
         (if (nil? kwdns)
           []
           [(java-string-literal kwdns)
            ", "]))
       (java-string-literal (name kwd)) ")"]))))

(setdispatch/def-set-method core/keyword-seed-platform
  [[[:platform :java] p]
   [:keyword kwd]]
  (core/with-new-seed
    "Keyword"
    (fn [s]
      (-> s
          (sd/access-seed-data {:type "Keyword"
                                :value kwd})
          (defs/datatype clojure.lang.Keyword)
          (defs/compiler compile-interned)))))

(setdispatch/def-set-method core/symbol-seed-platform
  [[[:platform :java] p]
   [:symbol sym]]
  (core/with-new-seed
    "Symbol"
    (fn [s]
      (-> s
          (sd/access-seed-data {:type "Symbol"
                                :value sym})
          (defs/datatype clojure.lang.Symbol)
          (defs/compiler compile-interned)))))

(defn compile-string [comp-state expr cb]
  (cb
   (defs/compilation-result
     comp-state
     (java-string-literal (sd/access-seed-data expr)))))

(setdispatch/def-set-method core/string-seed-platform
  [[[:platform :java] p]
   [:string x]]
  (core/with-new-seed
    "String"
    (fn [s]
      (-> s
          (sd/access-seed-data x)
          (defs/datatype java.lang.String)
          (defs/compiler compile-string)))))

(defn make-seq-expr [args]
  [compact
   "clojure.lang.PersistentList.EMPTY"
   (map (fn [arg]
          [".cons((java.lang.Object)(" arg "))"])
        (reverse args))])

(defn compile-seq [comp-state args cb]
  (println "Compile seq!!!")
  (cb (defs/compilation-result comp-state (make-seq-expr args))))

(setdispatch/def-set-method core/compile-coll-platform
  [[[:platform :java] p]
   [:any comp-state]
   [:any expr]
   [:any cb]]
  (println "Compile the coll" )
  (let [original-coll (core/access-original-coll expr)
        args (partycoll/normalized-coll-accessor
              (exm/lookup-compiled-indexed-results comp-state expr))]
    (cond
      (seq? original-coll) (compile-seq comp-state args cb)))
  #_(cb (defs/compilation-result
       comp-state
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (core/with-new-seed
      "operator-call"
      (fn [x]
        (-> x
            (sd/datatype ret-type)
            (sd/access-indexed-deps args)
            (defs/access-operator operator)
            (sd/compiler compile-operator-call))))))

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

(defn call-method [method-name obj0 & args0]
  (let [obj (geex/to-seed obj0)
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
            (defs/access-method-name method-name))))))

;;; Method shorts
(def j-nth (partial call-method "nth"))
(def j-first (partial call-method "first"))
(def j-next (partial call-method "next"))
(def j-count (partial call-method "count"))
(def j-val-at (partial call-method "valAt"))



(defn call-static-method [method-name cl & args0]
  {:pre [(string? method-name)
         (class? cl)]}
  (let [{:keys [args arg-types]} (preprocess-method-args args0)
        method (.getMethod cl method-name arg-types)]
    (geex/with-new-seed
      "call-static-method"
      (fn [x]
        (-> x
            (sd/datatype (.getReturnType method))
            (defs/access-class cl)
            (sd/access-indexed-deps args)
            (sd/compiler compile-call-static-method)
            (defs/access-method-name method-name))))))

(defmacro typed-defn [& args0]
  (let [args (merge (parse-typed-defn-args args0)
                    {:ns (str *ns*)})
        code (generate-typed-defn args)
        arg-names (mapv :name (:arglist args))]
    `(do
       ~@(when (contains-debug? args)
           [`(println ~code)])
       (let [obj# (janino-cook-and-load-object ~(full-java-class-name args)
                                               ~code)]       
         (defn ~(:name args) [~@arg-names]
           (.apply obj# ~@arg-names))))))

(defmacro disp-ns []
  (let [k# *ns*]
    k#))

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

    (typed-defn make-seq2 :debug [seedtype/int a
                                  seedtype/double b]
                (list a b))

    


    

    
    )


  )
