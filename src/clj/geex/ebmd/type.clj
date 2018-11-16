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
  (ebmd/import-and-check-arg-spec
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
  (ebmd/import-and-check-arg-spec
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

(ebmd/def-arg-spec maybe-seed-of-integer
  {:pred #(or (int? %)
              (and (seed/seed? %)
                   (contains? (set datatypes/integer-types)
                              (seed/datatype %))))
   :pos [1 2 3 (seed/typed-seed Integer/TYPE)]
   :neg [3.4 (seed/typed-seed Double/TYPE)]})

(defn- maybe-seed-of-primitive-pred [boxed-classes]
  (let [unboxed (set (map datatypes/unbox-class boxed-classes))]
    (fn [x]
      (or (contains? boxed-classes (class x))
          (and (seed/seed? x)
               (contains? unboxed (seed/datatype x)))))))

(ebmd/def-arg-spec maybe-seed-of-number
  {:pred (maybe-seed-of-primitive-pred datatypes/common-boxed-numbers)
   :pos [2 3 (seed/typed-seed Double/TYPE)]
   :neg [{} (seed/typed-seed Double)]})

(ebmd/def-arg-spec maybe-seed-of-primitive
  {:pred (maybe-seed-of-primitive-pred datatypes/boxed-primitives)
   :pos [2 3 (seed/typed-seed Double/TYPE)]
   :neg [{} (seed/typed-seed Double)]})


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
   :neg [4]})

(ebmd/def-arg-spec ::float-value
  {:pred (instance-of? [Float Float/TYPE])
   :pos [(float 3.4)]
   :neg [(double 3.4)]})

(ebmd/def-arg-spec ::int-value
  {:pred (instance-of? [Integer Integer/TYPE])
   :pos [(int 3)]
   :neg [(long 3)]})

(ebmd/def-arg-spec ::long-value
  {:pred (instance-of? [Long Long/TYPE])
   :pos [(long 3)]
   :neg [(int 3)]})

(ebmd/def-arg-spec ::short-value
  {:pred (instance-of? [Short Short/TYPE])
   :pos [(short 3)]
   :neg [(int 3)]})

(ebmd/def-arg-spec ::byte-value
  {:pred (instance-of? [Byte Byte/TYPE])
   :pos [(byte 3)]
   :neg [(int 3)]})

(ebmd/def-arg-spec ::char-value
  {:pred (instance-of? [Character Character/TYPE])
   :pos [(char 3)]
   :neg [(int 3)]})

(ebmd/def-arg-spec ::boolean-value
  {:pred (instance-of? [Boolean Boolean/TYPE])
   :pos [(boolean true)]
   :neg [(int 3)]})

(ebmd/def-arg-spec ::symbol-value
  {:pred symbol?
   :pos ['a 'asdf]
   :neg [:a]})

(ebmd/def-arg-spec ::string-value
  {:pred string?
   :pos ["asdf" "sd"]
   :neg [:a 'a]})

(ebmd/def-arg-spec ::keyword-value
  {:pred keyword?
   :pos [:a :b :c]
   :neg ['a]})

(ebmd/def-arg-spec ::map-value
  {:pred map?
   :pos [{:a :aktt}]
   :neg [#{3 4 }]})

(ebmd/def-arg-spec ::sequential-value
  {:pred sequential?
   :pos [[1 2 3] '(1 2 3)]
   :neg [#{3 4 }]})

(ebmd/def-arg-spec ::set-value
  {:pred set?
   :pos [#{:a 1} #{}]
   :neg [{:a 4}]})

(defn array-instance-of? [cl]
   (instance-of? [(datatypes/array-class-of-type cl)]))

(ebmd/def-arg-spec ::double-array-value
  {:pred (array-instance-of? Double/TYPE)
   :pos [(double-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::float-array-value
  {:pred (array-instance-of? Float/TYPE)
   :pos [(float-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::long-array-value
  {:pred (array-instance-of? Long/TYPE)
   :pos [(long-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::int-array-value
  {:pred (array-instance-of? Integer/TYPE)
   :pos [(int-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::short-array-value
  {:pred (array-instance-of? Short/TYPE)
   :pos [(short-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::byte-array-value
  {:pred (array-instance-of? Byte/TYPE)
   :pos [(byte-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::char-array-value
  {:pred (array-instance-of? Character/TYPE)
   :pos [(char-array [\a \b])]
   :neg [3]})

(ebmd/def-arg-spec ::boolean-array-value
  {:pred (array-instance-of? Boolean/TYPE)
   :pos [(boolean-array [false true])]
   :neg [3]})

(defn array-instance? [x]
  (datatypes/array-class? (class x)))

(ebmd/def-arg-spec ::array-value
  {:pred array-instance?
   :pos [(object-array [1 2 3])
         (float-array [1 2 3])]
   :neg [:a 3]})

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


(ebmd/declare-poly floating-point-value?)
(ebmd/declare-poly integer-value?)
(ebmd/declare-poly real-value?)
(ebmd/declare-poly coll-value?)
(ebmd/declare-poly floating-point-array-value?)
(ebmd/declare-poly integer-array-value?)
(ebmd/declare-poly real-array-value?)


(ebmd/def-poly floating-point-value? [ebmd/any-arg _] false)
(ebmd/def-poly integer-value? [ebmd/any-arg _] false)
(ebmd/def-poly real-value? [ebmd/any-arg _] false)
(ebmd/def-poly coll-value? [ebmd/any-arg _] false)
(ebmd/def-poly floating-point-array-value? [ebmd/any-arg _] false)
(ebmd/def-poly integer-array-value? [ebmd/any-arg _] false)
(ebmd/def-poly real-array-value? [ebmd/any-arg _] false)

(ebmd/def-poly floating-point-value? [::double-value _] true)
(ebmd/def-poly floating-point-value? [::float-value _] true)

(ebmd/def-poly floating-point-array-value? [::double-array-value _]
  true)
(ebmd/def-poly floating-point-array-value? [::float-array-value _]
  true)
(ebmd/def-poly integer-value? [::long-value _] true)
(ebmd/def-poly integer-value? [::int-value _] true)
(ebmd/def-poly integer-value? [::short-value _] true)
(ebmd/def-poly integer-value? [::byte-value _] true)
(ebmd/def-poly integer-array-value? [::long-array-value _]
  true)
(ebmd/def-poly integer-array-value? [::int-array-value _]
  true)
(ebmd/def-poly integer-array-value? [::short-array-value _]
  true)
(ebmd/def-poly integer-array-value? [::byte-array-value _]
  true)

(ebmd/def-poly coll-value? [::set-value _] true)
(ebmd/def-poly coll-value? [::map-value _] true)
(ebmd/def-poly coll-value? [::sequential-value _] true)

(ebmd/def-arg-spec ::floating-point-value
  {:pred floating-point-value?
   :pos [3.4]
   :neg [:a]})

(ebmd/def-arg-spec ::integer-value
  {:pred integer-value?
   :pos [3]
   :neg [3.4]})

(ebmd/def-arg-spec ::coll-value
  {:pred coll-value?
   :pos [{:a 3} #{1 2 3}]
   :neg [:a 3 4 "adsf"]})

(ebmd/def-arg-spec ::floating-point-array-value
  {:pred floating-point-array-value?
   :pos [(float-array [1 2 3])]
   :neg [3]})

(ebmd/def-arg-spec ::integer-array-value
  {:pred integer-array-value?
   :pos [(int-array [1 2 3])]
   :neg [3]})

(ebmd/def-poly real-value? [::floating-point-value _] true)
(ebmd/def-poly real-value? [::integer-value _] true)
(ebmd/def-poly real-array-value? [::floating-point-array-value _]
  true)
(ebmd/def-poly real-array-value? [::integer-array-value _]
  true)

(ebmd/def-arg-spec ::real-value
  {:pred real-value?
   :pos [3.3 4]
   :neg [:a]})

(ebmd/def-arg-spec ::real-array-value
  {:pred real-array-value?
   :pos [(double-array [1 2 3]) (int-array [1 2 3])]
   :neg [:a]})

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
   :neg [9]})

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
   :neg [2 :a]})

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


(ebmd/declare-poly floating-point-seed?)
(ebmd/declare-poly integer-seed?)
(ebmd/declare-poly real-seed?)
(ebmd/declare-poly floating-point-array-seed?)
(ebmd/declare-poly integer-array-seed?)
(ebmd/declare-poly real-array-seed?)
(ebmd/declare-poly coll-seed?)

(ebmd/def-poly floating-point-seed? [::ebmd/any-arg _] false)
(ebmd/def-poly integer-seed? [::ebmd/any-arg _] false)
(ebmd/def-poly real-seed? [::ebmd/any-arg _] false)
(ebmd/def-poly floating-point-array-seed? [::ebmd/any-arg _] false)
(ebmd/def-poly integer-array-seed? [::ebmd/any-arg _] false)
(ebmd/def-poly real-array-seed? [::ebmd/any-arg _] false)
(ebmd/def-poly coll-seed? [::ebmd/any-arg _] false)

(ebmd/def-poly floating-point-seed? [::double-seed _] true)
(ebmd/def-poly floating-point-seed? [::float-seed _] true)
(ebmd/def-poly integer-seed? [::long-seed _] true)
(ebmd/def-poly integer-seed? [::int-seed _] true)
(ebmd/def-poly integer-seed? [::short-seed _] true)
(ebmd/def-poly integer-seed? [::byte-seed _] true)
(ebmd/def-poly floating-point-array-seed? [::double-array-seed _]
  true)
(ebmd/def-poly floating-point-array-seed? [::float-array-seed _]
  true)
(ebmd/def-poly integer-array-seed? [::long-array-seed _] true)
(ebmd/def-poly integer-array-seed? [::int-array-seed _] true)
(ebmd/def-poly integer-array-seed? [::short-array-seed _] true)
(ebmd/def-poly integer-array-seed? [::byte-array-seed _] true)

(ebmd/def-arg-spec ::floating-point-seed
  {:pred floating-point-seed?
   :pos [(seed/typed-seed Double/TYPE)]
   :neg []})
(ebmd/def-arg-spec ::integer-seed
  {:pred integer-seed?
   :pos [(seed/typed-seed Integer/TYPE)]
   :neg []})

(ebmd/def-arg-spec ::floating-point-array-seed
  {:pred floating-point-array-seed?
   :pos [(seed/typed-seed (datatypes/array-class-of-type
                           Double/TYPE))
         (seed/typed-seed (datatypes/array-class-of-type
                           Float/TYPE))]
   :neg []})

(ebmd/def-arg-spec ::integer-array-seed
  {:pred integer-array-seed?
   :pos [(seed/typed-seed
          (datatypes/array-class-of-type
           Integer/TYPE))
         (seed/typed-seed
          (datatypes/array-class-of-type
           Long/TYPE))]
   :neg []})

(ebmd/def-poly real-array-seed? [::floating-point-array-seed _]
  true)
(ebmd/def-poly real-array-seed? [::integer-array-seed _]
  true)
(ebmd/def-poly real-seed? [::floating-point-seed _] true)
(ebmd/def-poly real-seed? [::integer-seed _] true)

(ebmd/def-arg-spec ::real-seed
  {:pred real-seed?
   :pos [(seed/typed-seed Double/TYPE)
         (seed/typed-seed Integer/TYPE)
         (seed/typed-seed Float/TYPE)
         (seed/typed-seed Short/TYPE)
         (seed/typed-seed Byte/TYPE)
         (seed/typed-seed Long/TYPE)]
   :neg []})

(ebmd/def-arg-spec ::real-array-seed
  {:pred real-array-seed?
   :pos [(seed/typed-seed
          (datatypes/array-class-of-type Double/TYPE))
         (seed/typed-seed
          (datatypes/array-class-of-type Integer/TYPE))
         (seed/typed-seed
          (datatypes/array-class-of-type Float/TYPE))
         (seed/typed-seed
          (datatypes/array-class-of-type Short/TYPE))
         (seed/typed-seed
          (datatypes/array-class-of-type Byte/TYPE))
         (seed/typed-seed
          (datatypes/array-class-of-type Long/TYPE))]
   :neg []})

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
   :neg []})

(ebmd/def-arg-spec ::any ebmd/any-arg)

(ebmd/def-poly coll-seed? [::map-seed _] true)
(ebmd/def-poly coll-seed? [::sequential-seed _] true)
(ebmd/def-poly coll-seed? [::set-seed _] true)

(ebmd/def-arg-spec ::coll-seed
  {:pred coll-seed?
   :pos [(seed/typed-seed clojure.lang.IPersistentSet)
         (seed/typed-seed clojure.lang.IPersistentMap)
         (seed/typed-seed clojure.lang.IPersistentVector)
         (seed/typed-seed clojure.lang.ISeq)]
   :neg [(seed/typed-seed String)]})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Either value or seed
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro joint-arg-spec [base-name]
  (let [n (str *ns*)
        joint-name (keyword n base-name)
        value-name (keyword n (str base-name "-value"))
        seed-name (keyword n (str base-name "-seed"))]
    `(ebmd/def-arg-spec ~joint-name
       (ops/or ~value-name
               ~seed-name))))

#_(ebmd/def-arg-spec ::real
  (ops/or ::real-seed
          ::real-value))

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
