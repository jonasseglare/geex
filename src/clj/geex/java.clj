(ns geex.java

  "Generation of Java backed code"

  (:require [geex.java.defs :as jdefs]
            [geex.core :as geex]
            [geex.core.defs :as defs]
            [geex.platform.low :as low]
            [geex.platform.high :as high]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core :as core]
            [bluebell.utils.specutils :as specutils]
            [bluebell.utils.core :as utils]
            [geex.core.seed :as sd]
            [bluebell.tag.core :as tg]
            [clojure.reflect :as r]
            [geex.core.datatypes :as dt]
            [clojure.string :as cljstr]
            [geex.core.seedtype :as seedtype])
  (:import [org.codehaus.janino SimpleCompiler]))

(def platform-tag [:platform :java])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare unpack)
(declare call-method)
(declare call-static-method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Unpacking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def compact {:prefix " " :step ""})


(defn compile-cast [comp-state expr cb]
  (cb (defs/compilation-result
        comp-state
        (high/wrap-in-parens
         ["(" (.getName (sd/datatype expr)) ")"
          (-> expr
              defs/access-compiled-deps
              :value)]))))

(defn cast-seed [type value]
  (geex/with-new-seed
    "cast-seed"
    (fn [seed]
      (-> seed
          (sd/add-deps {:value value})
          (sd/compiler compile-cast)
          (sd/datatype type)))))


(defn unpack-to-seed [dst-seed src-seed]
  (assert (sd/seed? src-seed))
  (assert (sd/seed? dst-seed))
  (let [dst-type (defs/datatype dst-seed)]
    (if (isa? (defs/datatype src-seed)
              dst-type)
      src-seed
      (cast-seed dst-type src-seed))))

(defn unpack-to-vector [dst-type src-seed]
  (mapv (fn [index dst-element-type]
          (unpack dst-element-type (call-method "nth" src-seed (int index))))
        (range (count dst-type))
        dst-type))


(defn unpack [dst-type src-seed]
  (assert (sd/seed? src-seed))
  (cond
    (sd/seed? dst-type) (unpack-to-seed dst-type src-seed)
    (vector? dst-type) (unpack-to-vector
                        dst-type
                        (unpack-to-seed
                         (sd/typed-seed clojure.lang.Indexed)
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
     (high/wrap-in-parens
      [(:obj (sd/access-compiled-deps expr))
       "."
       (defs/access-method-name expr)
       (let [dp (sd/access-compiled-indexed-deps expr)]
         (high/wrap-in-parens [compact (join-args dp)]))]))))

(defn compile-call-static-method [comp-state expr cb]
  (cb
   (defs/compilation-result
     comp-state
     (high/wrap-in-parens
      [(.getName (defs/access-class expr))
       "."
       (defs/access-method-name expr)
       (let [dp (sd/access-compiled-indexed-deps expr)]
         (high/wrap-in-parens [compact (join-args dp)]))]))))

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
    `(let [[top# code#] (geex/top-and-code
                         [{:platform :java}]
                         (core/return-value (apply
                                             (fn [~@(map :name arglist)]
                                               ~@(:body args))
                                             (map to-binding ~quoted-args))))]
       (utils/indent-nested
        [[{:prefix " "
           :step ""}
          "package " ~(java-package-name args) ";"]
         ~(str "public class " (java-class-name args) " {")
         ["public " (r/typename (low/get-type-signature platform-tag top#))
          " apply("
          (make-arg-list ~quoted-args)
          ") {"
          code#
          "}"]
         "}"]))))

(defn contains-debug? [args]
  (some (tg/tagged? :debug) (:meta args)))

(defn preprocess-method-args [args0]
  (let [args (mapv geex/to-seed args0)
        arg-types (into-array java.lang.Class (mapv sd/datatype args))]
    (utils/map-of args arg-types)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    `(let [obj# (janino-cook-and-load-object ~(full-java-class-name args)
                                             ~code)]
       ~@(when (contains-debug? args)
           [`(println ~code)])
       (defn ~(:name args) [~@arg-names]
         (.apply obj# ~@arg-names)))))

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

    
    

   

    
    )


  )
