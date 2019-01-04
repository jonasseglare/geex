(ns geex.ebmd.type
  (:import [geex Seed TypedSeed DynamicSeed SeedParameters Mode])
  (:require [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.ops :as ops]
            [bluebell.utils.ebmd.type :as type]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]
            [geex.core.datatypes :as datatypes]))

(def keyword-type-reg (atom {}))

(defn register-type [k tp]
  {:pre [(keyword? k)]}
  (swap! keyword-type-reg assoc k tp))

;; Used to interpret anything that could carry type information
;; into that type information. For instance, certain keywords
;; can represent types, classes are types themselves, typed-seeds
;; have a type associated with them. This function is mainly for convenience
;; in argument lists.
(ebmd/declare-poly resolve-type)


(ebmd/def-poly resolve-type [::ebmd/any-arg any] nil)

(ebmd/def-poly resolve-type [::type/keyword k]
  (get (deref keyword-type-reg) k))

(defn seed-of-type-such-that [pred pos neg]
  {:pred #(and (seed/seed? %)
               (pred (seed/datatype %)))
   :pos pos
   :neg neg})

(ebmd/def-arg-spec seed-with-class
  (seed-of-type-such-that class?
                          [(seed/typed-seed (class "asdf"))]
                          [(class "asdf")]))

(defn seed-of [stype]
  (ebmd/import-arg-spec
   (merge {:key [::seed-of stype]}
          (seed-of-type-such-that (partial = stype)
                                  [(seed/typed-seed stype)]
                                  [(seed/typed-seed ::kattskit)]))))

(def nothing-seed (seed-of ::defs/nothing))




;; deprecate: use ::class instead
(ebmd/def-arg-spec class-arg
  {:pred class?
   :pos [(class 3.0)]
   :neg [3.0]})

(ebmd/def-arg-spec ::seed
  {:pred seed/seed?
   :pos [(seed/typed-seed :kattskit)]
   :neg [:a {:a 9}]})

(ebmd/def-arg-spec ::not-seed
  {:pred (complement seed/seed?)
   :pos [:a {:a 9}]
   :neg [(seed/typed-seed :kattskit)]})

(ebmd/def-arg-spec ::class
  {:pred class?
   :pos [java.lang.ArithmeticException]
   :neg [:a]})


(ebmd/def-poly resolve-type [::class arg] arg)
(ebmd/def-poly resolve-type [::seed x] (seed/datatype x))




(def sample-compilable-seed
  (DynamicSeed.
   (doto (SeedParameters.)
     (set-field description "Compilable seed")
     (set-field type ::defs/nothing)
     (set-field mode Mode/Pure)
     (set-field compiler identity))))

(ebmd/def-arg-spec compilable-seed
  {:pred seed/compilable-seed?
   :pos [sample-compilable-seed]
   :neg [(seed/typed-seed Double/TYPE)
         (seed/typed-seed :kattskit)]})

(defn map-with-key-value [key value]
  (ebmd/import-arg-spec
   {:pred #(and (map? %)
                (=  (get % key) value))
    :key [::map-with-key-value key value]
    :pos [{key value}]
    :neg [{}]}))

(def array-seed
  (ebmd/import-arg-spec
   (merge {:key ::array-seed}
          (seed-of-type-such-that
           datatypes/array-class?
           [(seed/typed-seed
             (datatypes/array-class
              Double/TYPE))]
           [(seed/typed-seed Double/TYPE)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Common values (not seeds)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-elementary-entry [[basic-name basic-class]]
  (let [n (str *ns*)]
    [(keyword basic-name) {:class basic-class
                           :value (keyword n (str basic-name
                                                  "-value"))
                           :seed (keyword n (str basic-name
                                                 "-seed"))
                           :any (keyword n basic-name)}]))

(def elementary-types
  (into {}
        (map make-elementary-entry
             [["double" Double/TYPE]
              ["float" Float/TYPE]
              ["int" Integer/TYPE]
              ["long" Long/TYPE]
              ["short" Short/TYPE]
              ["byte" Byte/TYPE]
              ["char" Character/TYPE]
              ["boolean" Boolean/TYPE]
              ["symbol" clojure.lang.Symbol]
              ["string" String]
              ["keyword" clojure.lang.Keyword]])))

(defn instance-of?
  ([classes]
   (partial instance-of? classes))
  ([classes x]
   (let [cl (class x)]
     (some #(isa? % cl) classes))))

(ebmd/def-arg-spec ::double-value
  {:pred (instance-of? [Double Double/TYPE])
   :pos [3.4]
   :neg [4]
   :reg-spec? true})

(ebmd/def-arg-spec ::float-value
  {:pred (instance-of? [Float Float/TYPE])
   :pos [(float 3.4)]
   :neg [(double 3.4)]
   :reg-spec? true})

(ebmd/def-arg-spec ::int-value
  {:pred (instance-of? [Integer Integer/TYPE])
   :pos [(int 3)]
   :neg [(long 3)]
   :reg-spec? true})

(ebmd/def-arg-spec ::long-value
  {:pred (instance-of? [Long Long/TYPE])
   :pos [(long 3)]
   :neg [(int 3)]
   :reg-spec? true})

(ebmd/def-arg-spec ::short-value
  {:pred (instance-of? [Short Short/TYPE])
   :pos [(short 3)]
   :neg [(int 3)]
   :reg-spec? true})

(ebmd/def-arg-spec ::byte-value
  {:pred (instance-of? [Byte Byte/TYPE])
   :pos [(byte 3)]
   :neg [(int 3)]
   :reg-spec? true})

(ebmd/def-arg-spec ::char-value
  {:pred (instance-of? [Character Character/TYPE])
   :pos [(char 3)]
   :neg [(int 3)]
   :reg-spec? true})

(ebmd/def-arg-spec ::boolean-value
  {:pred (instance-of? [Boolean Boolean/TYPE])
   :pos [(boolean true)]
   :neg [(int 3)]
   :reg-spec? true})

(ebmd/def-arg-spec ::symbol-value
  {:pred symbol?
   :pos ['a 'asdf]
   :neg [:a]
   :reg-spec? true})

(ebmd/def-arg-spec ::string-value
  {:pred string?
   :pos ["asdf" "sd"]
   :neg [:a 'a]
   :reg-spec? true})

(ebmd/def-arg-spec ::keyword-value
  {:pred keyword?
   :pos [:a :b :c]
   :neg ['a]
   :reg-spec? true})

(ebmd/def-arg-spec ::map-value
  {:pred map?
   :pos [{:a :aktt}]
   :neg [#{3 4 }]
   :reg-spec? true})

(ebmd/def-arg-spec ::sequential-value
  {:pred sequential?
   :pos [[1 2 3] '(1 2 3)]
   :neg [#{3 4 }]
   :reg-spec? true})

(ebmd/def-arg-spec ::set-value
  {:pred set?
   :pos [#{:a 1} #{}]
   :neg [{:a 4}]
   :reg-spec? true})

(defn array-instance-of? [cl]
   (instance-of? [(datatypes/array-class-of-type cl)]))

(ebmd/def-arg-spec ::double-array-value
  {:pred (array-instance-of? Double/TYPE)
   :pos [(double-array [1 2 3])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::float-array-value
  {:pred (array-instance-of? Float/TYPE)
   :pos [(float-array [1 2 3])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::long-array-value
  {:pred (array-instance-of? Long/TYPE)
   :pos [(long-array [1 2 3])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::int-array-value
  {:pred (array-instance-of? Integer/TYPE)
   :pos [(int-array [1 2 3])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::short-array-value
  {:pred (array-instance-of? Short/TYPE)
   :pos [(short-array [1 2 3])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::byte-array-value
  {:pred (array-instance-of? Byte/TYPE)
   :pos [(byte-array [1 2 3])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::char-array-value
  {:pred (array-instance-of? Character/TYPE)
   :pos [(char-array [\a \b])]
   :neg [3]
   :reg-spec? true})

(ebmd/def-arg-spec ::boolean-array-value
  {:pred (array-instance-of? Boolean/TYPE)
   :pos [(boolean-array [false true])]
   :neg [3]
   :reg-spec? true})

(defn array-instance? [x]
  (datatypes/array-class? (class x)))

(ebmd/def-arg-spec ::array-value
  {:pred array-instance?
   :pos [(object-array [1 2 3])
         (float-array [1 2 3])]
   :neg [:a 3]
   :reg-spec? true})

(def floating-point-type-info
  {::double-value {:conv double}
   ::float-value {:conv float}})

(def integer-type-info
  {::int-value {:conv int}
   ::long-value {:conv long}
   ::short-value {:conv short}
   ::byte-value {:conv byte}})

(def number-type-info
  (merge floating-point-type-info
         integer-type-info))

(def type-info
  (merge
   number-type-info
   {::char-value {:conv char}
    ::boolean-value {:conv boolean}}))

(def number-promotion-order [:byte
                             :short
                             :int
                             :long
                             :float
                             :double])


(def all-numeric-promotions
  (reduce
   into []
   (for [[from & tos] (take-while
                       (complement empty?)
                       (iterate rest number-promotion-order))]
     (for [to tos]
       [from to]))))


(defn promotion-of-type [tp [from-k to-k]]
  [(-> elementary-types (get from-k) tp)
   (-> elementary-types (get to-k) tp)])

(defn numeric-promotions-of-type [tp]
  (mapv (partial promotion-of-type :value)
        all-numeric-promotions))



(doseq [[from to] (numeric-promotions-of-type :value)]
  (let [ti (get type-info to)]
     (ebmd/register-promotion to
                              (:conv ti)
                              from)))


(ebmd/def-arg-spec-union
 ::floating-point-value
 
 ::double-value ::float-value)

(ebmd/def-arg-spec-union
 ::integer-value
 ::byte-value ::short-value ::int-value ::long-value)


(ebmd/def-arg-spec-union
 ::coll-value

 ::set-value ::map-value ::sequential-value)

(ebmd/def-arg-spec-union
 ::floating-point-array-value

 ::float-array-value ::double-array-value)

(ebmd/def-arg-spec-union
 ::integer-array-value

 ::int-array-value ::short-array-value
 ::long-array-value ::byte-array-value)

(ebmd/def-arg-spec-union
 ::real-array-value

 ::floating-point-array-value ::integer-array-value)

(ebmd/def-arg-spec ::number-value {:pred number?
                                   :pos [3/4 9 7]
                                   :neg [:a]})

(ebmd/def-arg-spec-union
  ::real-value

  ::integer-value ::floating-point-value
  
  ::number-value)

(ebmd/def-arg-spec-union
  ::array
  
  ::array-seed ::array-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Seed types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seed-of-class?
  ([classes x]
   (and (seed/seed? x)
        (let [dt (seed/datatype x)]
          (some #(isa? % dt) classes))))
  ([classes]
   (partial seed-of-class? classes)))

(defn seed-arg-spec-of [cl]
  {:pred (seed-of-class? [cl])
   :pos [(seed/typed-seed cl)]
   :neg [9]
   :reg-spec? true})

(ebmd/def-arg-spec ::double-seed (seed-arg-spec-of Double/TYPE))
(ebmd/def-arg-spec ::float-seed (seed-arg-spec-of Float/TYPE))
(ebmd/def-arg-spec ::int-seed (seed-arg-spec-of Integer/TYPE))
(ebmd/def-arg-spec ::long-seed (seed-arg-spec-of Long/TYPE))
(ebmd/def-arg-spec ::short-seed (seed-arg-spec-of Short/TYPE))
(ebmd/def-arg-spec ::byte-seed (seed-arg-spec-of Byte/TYPE))
(ebmd/def-arg-spec ::char-seed (seed-arg-spec-of Character/TYPE))
(ebmd/def-arg-spec ::boolean-seed (seed-arg-spec-of Boolean/TYPE))
(ebmd/def-arg-spec ::any-seed
  {:pred seed/seed?
   :pos [(seed/typed-seed java.util.concurrent.Callable)]
   :neg [2 :a]
   :reg-spec? true})

(ebmd/def-arg-spec ::double-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Double/TYPE)))
(ebmd/def-arg-spec ::float-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Float/TYPE)))
(ebmd/def-arg-spec ::int-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Integer/TYPE)))
(ebmd/def-arg-spec ::long-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Long/TYPE)))
(ebmd/def-arg-spec ::short-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Short/TYPE)))
(ebmd/def-arg-spec ::byte-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Byte/TYPE)))
(ebmd/def-arg-spec ::char-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type
                     Character/TYPE)))
(ebmd/def-arg-spec ::boolean-array-seed
  (seed-arg-spec-of (datatypes/array-class-of-type Boolean/TYPE)))


(ebmd/def-arg-spec-union
 ::floating-point-seed

 ::float-seed ::double-seed)

(ebmd/def-arg-spec-union
 ::integer-seed

 ::byte-seed ::short-seed ::int-seed ::long-seed)


(ebmd/def-arg-spec-union
 ::floating-point-array-seed

 ::float-array-seed ::double-array-seed)

(ebmd/def-arg-spec-union
  ::integer-array-seed

  ::byte-array-seed ::short-array-seed
  ::int-array-seed ::long-array-seed)


(ebmd/def-arg-spec-union
 ::real-seed

 ::double-seed ::integer-seed ::float-seed
  ::short-seed ::byte-seed ::long-seed)

(ebmd/def-arg-spec-union
  ::comparable-seed
  ::real-seed
  ::char-seed)

(ebmd/def-arg-spec-union
  ::comparable-value
  ::real-value
  ::char-value)

(ebmd/def-arg-spec-union
  ::comparable
  ::comparable-seed
  ::comparable-value)

(ebmd/def-arg-spec-union
 ::real-array-seed

 ::double-array-seed ::integer-array-seed ::float-array-seed
 ::short-array-seed ::byte-array-seed ::long-array-seed)

(ebmd/def-arg-spec ::symbol-seed
  (seed-arg-spec-of clojure.lang.Symbol))
(ebmd/def-arg-spec ::string-seed
  (seed-arg-spec-of String))
(ebmd/def-arg-spec ::keyword-seed
  (seed-arg-spec-of clojure.lang.Keyword))

(ebmd/def-arg-spec ::map-seed
  (seed-arg-spec-of clojure.lang.IPersistentMap))
(ebmd/def-arg-spec ::set-seed
  (seed-arg-spec-of clojure.lang.IPersistentSet))

(defn sequential-seed? [x]
  (and (seed/seed? x)
       (let [cl (seed/datatype x)]
         (or (= cl clojure.lang.IPersistentVector)
             (= cl clojure.lang.ISeq)))))

(ebmd/def-arg-spec ::sequential-seed
  {:pred sequential-seed?
   :pos [(seed/typed-seed clojure.lang.IPersistentVector)
         (seed/typed-seed clojure.lang.ISeq)]
   :neg []
   :reg-spec? true})

(ebmd/def-arg-spec ::any ebmd/any-arg)

(ebmd/def-arg-spec-union ::coll-seed
  ::map-seed ::set-seed ::sequential-seed)

(ebmd/def-arg-spec-union ::coll
  ::coll-value ::coll-seed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Either value or seed
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn joint-arg-spec [base-name]
  (let [n (str *ns*)
        joint-name (keyword n base-name)
        value-name (keyword n (str base-name "-value"))
        seed-name (keyword n (str base-name "-seed"))]
    (ebmd/def-arg-spec-union joint-name value-name seed-name)))

(def value-to-seed-promotions
  ["float"
   "double"
   "long"
   "short"
   "int"
   "byte"
   "char"
   "boolean"
   "symbol"
   "string"
   "keyword"
   "sequential"
   "map"
   "set"
   ])

(def ns-str (str *ns*))

(defmacro make-value-to-seed-promotions [promoting-fn]
  `(do
     ~@(for [s value-to-seed-promotions]
         `(ebmd/register-promotion
           ~(keyword ns-str (str s "-seed"))
           ~promoting-fn
           ~(keyword ns-str (str s "-value"))))))

(joint-arg-spec "double")
(joint-arg-spec "float")
(joint-arg-spec "byte")
(joint-arg-spec "short")
(joint-arg-spec "int")
(joint-arg-spec "long")
(joint-arg-spec "char")
(joint-arg-spec "boolean")
(joint-arg-spec "double-array")
(joint-arg-spec "float-array")
(joint-arg-spec "byte-array")
(joint-arg-spec "short-array")
(joint-arg-spec "int-array")
(joint-arg-spec "long-array")
(joint-arg-spec "char-array")
(joint-arg-spec "boolean-array")
(joint-arg-spec "string")
(joint-arg-spec "symbol")
(joint-arg-spec "keyword")
(joint-arg-spec "sequential")
(joint-arg-spec "map")
(joint-arg-spec "set")
(joint-arg-spec "real")
(joint-arg-spec "integer")
(joint-arg-spec "floating-point")
(joint-arg-spec "real-array")
(joint-arg-spec "integer-array")
(joint-arg-spec "floating-point-array")


(defmacro register-type-and-its-array [ks tp-sym]
  {:pre [(string? ks)
         (symbol? tp-sym)]}
  `(do (register-type ~(keyword ns-str ks)
                      ~tp-sym)
       (register-type ~(keyword ns-str (str ks "-array"))
                      (datatypes/array-class-of-type ~tp-sym))))

(register-type-and-its-array "double" Double/TYPE)
(register-type-and-its-array "float" Float/TYPE)
(register-type-and-its-array "boolean" Boolean/TYPE)
(register-type-and-its-array "character" Character/TYPE)
(register-type-and-its-array "int" Integer/TYPE)
(register-type-and-its-array "long" Long/TYPE)
(register-type-and-its-array "short" Short/TYPE)
(register-type-and-its-array "byte" Byte/TYPE)
(register-type-and-its-array "real" Double/TYPE)
(register-type-and-its-array "floating-point" Double/TYPE)
(register-type-and-its-array "integer" Long/TYPE)
(register-type-and-its-array "object" java.lang.Object)
(register-type ::symbol clojure.lang.Symbol)
(register-type ::string String)
(register-type ::keyword clojure.lang.Keyword)
