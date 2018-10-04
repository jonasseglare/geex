(ns geex.core.datatypes
  (:require [clojure.spec.alpha :as spec]
            [clojure.reflect :as r]
            [clojure.string :as cljstr]
            [clojure.set :as cljset])
  (:refer-clojure :exclude [void char boolean byte short int long float double]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Declarations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare box-class)
(declare unbox-class)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Sample values
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn add-sample-type [dst x]
  (assoc dst (class x) x))

(def sample-type-map (reduce add-sample-type
                             {}
                             [34.0
                              (clojure.core/float 3.4)
                              false
                              34
                              (bigint 34)
                              (bigdec 34.0)
                              3/4
                              (clojure.core/byte 3)
                              (clojure.core/short 3)
                              (clojure.core/int 3)
                              \a
                              :a]))

(defn array-class-of-type [tp]
  (class (make-array tp 0)))

(def array-class array-class-of-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Java arrays
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sample-value [x]
  (or (get sample-type-map x)
      (get sample-type-map (box-class x))))

(defn query-return-type [f args]
  (let [samples (map get-sample-value args)]
    (try
      (if (every? (complement nil?) samples)
        (unbox-class (class (apply f samples))))
      (catch Throwable e nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  For identifying things
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn boxed-type-symbol [name]
  (symbol (str "java.lang." name)))

(defn unboxed-type-symbol [name]
  (symbol (str "java.lang." name "/TYPE")))

(defn make-primitive-type-info [name]
  (eval {:unboxed-name name
         :unboxed-type (unboxed-type-symbol name)
         :boxed-type (boxed-type-symbol name)}))

(def unboxable-type-names
  ["Float"
   "Double"
   "Character"
   "Short"
   "Integer"
   "Long"
   "Boolean"
   "Void"])

(def primitive-type-list
  (map make-primitive-type-info
       unboxable-type-names))

(def boxed-primitives (set (map :boxed-type primitive-type-list)))

(def common-boxed-numbers
  #{Float Double
    Character Byte
    Short Integer Long})

(def boxed-to-unboxed-map (into {} (map (fn [p]
                                          [(:boxed-type p) (:unboxed-type p)])
                                        primitive-type-list)))

(def unboxed-to-boxed-map (into {} (map (fn [p]
                                          [(:unboxed-type p) (:boxed-type p)])
                                        primitive-type-list)))

(defn unbox-class [x]
  (or (get boxed-to-unboxed-map x) x))

(defn box-class [x]
  (or (get unboxed-to-boxed-map x) x))

(defn unboxed-class-of [value]
  (-> value
      class
      unbox-class))

(defmacro inject-type-defs []
  `(do
     ~@(map (fn [info]
              `(def
                 ~(symbol (.getName (:unboxed-type info)))
                 ~(unboxed-type-symbol (:unboxed-name info))))
            primitive-type-list)))

(def unboxed-type-set (into #{} (map :unboxed-type primitive-type-list)))

(defn unboxed-type? [x]
  {:pre [(class? x)]}
  (contains? unboxed-type-set x))

(defn component-type [array-class]
  (.getComponentType array-class))

(def boxed-type? (complement unboxed-type?))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Type rules for operators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See this page:
;;; https://www.math.uni-hamburg.de/doc/java/tutorial/java/nutsandbolts/arithmetic.html
(def float-or-double #{Double/TYPE Float/TYPE})




;;;; Encodes the result type of
;;;; + - * / %
(defn binary-math-op-result-type [a b]
  {:pre [(unboxed-type? a)
         (unboxed-type? b)]}
  (let [ab (set [a b])
        has-long? (contains? ab Long/TYPE)
        has-float? (not (empty? (cljset/intersection
                                 ab float-or-double)))]
    (cond
      (and has-long? (not has-float?)) Long/TYPE
      (not has-float?) Integer/TYPE
      (contains? ab Double/TYPE) Double/TYPE
      (contains? ab Float/TYPE) Float/TYPE
      :default
      (throw
       (ex-info
        "Failed to resolve return type for math operator"
        {:a a
         :b b})))))

(defn unary-plus-minus-result-type [x]
  {:pre [(unboxed-type? x)]}
  (if (contains? #{Byte/TYPE Short/TYPE Character/TYPE}
                 x)
    Integer/TYPE
    x))


(defn math-op-result-type [args]
  {:pre [(sequential? args)]}
  (case (count args)
    0 (throw (ex-info "Without any arguments the result type is undefined"))
    1 (unary-plus-minus-result-type (first args))
    (reduce binary-math-op-result-type args)))

;; https://en.wikipedia.org/wiki/Bitwise_operation#Shifts_in_Java
;;
;; https://en.wikiversity.org/wiki/Advanced_Java/Bitwise_Operators#Bitwise_Operations
;;
;; (Note that the operands can be any integral type; but if it is a type smaller than int, it will be promoted to an int type, and the result will be int


(def integer-types
  [
   Byte/TYPE
   Character/TYPE
   Short/TYPE
   Integer/TYPE
   Long/TYPE
   ])

(def primitive-number-types
  [
   Byte/TYPE
   Short/TYPE
   Integer/TYPE
   Long/TYPE
   Character/TYPE
   Double/TYPE
   Float/TYPE
   ])

(def int-type-to-rank (zipmap integer-types
                              (range (count integer-types))))

(def rank-to-int-type (zipmap (range (count integer-types))
                              integer-types))

(defn bit-op-result-type [input-types]
  {:pre [(sequential? input-types)
         (every? (partial contains? int-type-to-rank)
                 input-types)]}
  (get rank-to-int-type
       (transduce
        (map int-type-to-rank)
        (completing max)
        (get int-type-to-rank Integer/TYPE)
        input-types)))

(defn array-class? [x]
  (and (class? x)
       (.isArray x)))

