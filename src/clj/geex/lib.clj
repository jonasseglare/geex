(ns geex.lib
  (:require [geex.core :as core]
            [clojure.core :as c]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [bluebell.utils.setdispatch :as setdispatch]
            [bluebell.utils.lufn :as lufn]
            [geex.core.typesystem :as ts]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as dt]
            [geex.core.xplatform :as xp])
  (:refer-clojure :only [defn
                         fn
                         apply
                         defmacro
                         case
                         comp
                         identity
                         fn?
                         let
                         map?
                         ->]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Code private to this file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn wrapped-step? [x]
  (c/and (map? x)
         (fn? (:wrap x))
         (fn? (:unwrap x))
         (fn? (:step x))))

(defn wrap-step [step]
  {:pre [(c/or (wrapped-step? step)
               (fn? step))]}
  (if (fn? step)
    {:wrap identity
     :unwrap identity
     :step step}
    step))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Common stuff
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn seed-wrapper [predicate]
  (fn [x]
    (if (c/or (seed/seed? predicate)
              (c/not (predicate x)))
      x
      (core/to-seed x))))

(def number-to-seed (seed-wrapper c/number?))
(def char-to-seed (seed-wrapper c/char?))
(def string-to-seed (seed-wrapper c/string?))
(def keyword-to-seed (seed-wrapper c/keyword?))
(def symbol-to-seed (seed-wrapper c/symbol?))

(defn wrap-args [wrapper f]
  (fn [& args]
    (apply f (c/map wrapper args))))

(def wrap-numeric-args (c/partial wrap-args number-to-seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Various utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn with-platform [f]
  (fn [& args]
    (apply f (c/conj (c/seq args) (defs/get-platform-tag)))))

(defn lufn-with-platform [f]
  (fn [& args]
    (apply f (c/conj (c/seq args) (defs/get-platform)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Polymorphic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Numeric operations
(def numeric-op (comp wrap-numeric-args with-platform))

(def xp-numeric (comp wrap-numeric-args xp/caller))

(def negate (xp-numeric :negate))
(def binary-add (xp-numeric :binary-add))
(def binary-sub (xp-numeric :binary-sub))
(def binary-div (xp-numeric :binary-div))
(def binary-mul (xp-numeric :binary-mul))

(defmacro generalize-binary-op [name
                                op
                                args
                                zero-arg-output
                                one-arg-output]
  `(defn ~name [& ~args]
     (c/case (c/count ~args)
       0 ~zero-arg-output
       1 ~one-arg-output
       (c/reduce ~op ~args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Outer API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Forward decls
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def typed-seed seed/typed-seed)
(def nil-of core/nil-of)
(def wrap core/to-seed)
(def unwrap core/basic-unwrap)

(def nil? core/basic-nil?)

;;;------- Common math operators -------

(generalize-binary-op + binary-add args
                      0
                      (c/first args))

(generalize-binary-op - binary-sub args
                      0
                      (negate (c/first args)))

(generalize-binary-op / binary-div args
                      (throw
                       (c/ex-info
                        "Insufficient number of arguments to /" {}))
                      (c/first args))

(generalize-binary-op * binary-mul args
                      1
                      (c/first args))



;;;------- More math functions -------
(defn inc [x]
  (+ x 1))

(defn dec [x]
  (- x 1))

(defn sqr [x]
  (* x x))

(def quot (xp-numeric :quot))
(def rem (numeric-op core/platform-rem))
(def sqrt (numeric-op core/platform-sqrt))

;;;------- Comparison operators -------

(def == (lufn-with-platform core/platform-==))
(def <= (lufn-with-platform core/platform-<=))
(def >= (lufn-with-platform core/platform->=))
(def > (lufn-with-platform core/platform->))
(def < (lufn-with-platform core/platform-<))
(def != (lufn-with-platform core/platform-!=))

(def = (lufn-with-platform core/platform-=))

;;;------- Logic operators -------

(defmacro and [& args]
  (if (c/empty? args)
    `(core/to-seed true)
    `(core/If ~(c/first args)
              (and ~@(c/rest args))
              (core/to-seed false))))

(defmacro or [& args]
  (if (c/empty? args)
    `(core/to-seed false)
    `(core/If ~(c/first args)
              (core/to-seed true)
              (or ~@(c/rest args)))))

(def not (xp/caller :not))

(def not= (comp not =))

(defmacro implies [a b]
  `(or (not ~a) ~b))




;;;------- Array functions -------
(def array-class dt/array-class)


(def make-array core/basic-make-array)

(ts/def-default-set-method aget [[[:seed :array] x]
                              [(ts/maybe-seed-of :integer) i]]
  (core/basic-aget x i))

(ts/def-default-set-method aset [[[:seed :array] x]
                              [(ts/maybe-seed-of :integer) i]
                              [:any value]]
  (core/basic-aset x i value))

(ts/def-default-set-method aget [[[:seed :array] x]
                                  [(ts/maybe-seed-of :integer) i]]
  (core/basic-aget x i))

(ts/def-default-set-method alength [[[:seed :array] x]]
  (core/basic-alength x))


;;;------- Collection functions -------

(ts/def-default-set-method conj [[:any dst]
                              [:any x]]
  (core/basic-conj dst x))

(ts/def-default-set-method seq [[:any x]]
  (core/basic-seq x))

(ts/def-default-set-method empty? [[:any x]]
  (nil? (seq x)))

(ts/def-default-set-method first [[:any x]]
  (core/basic-first x))

(ts/def-default-set-method rest [[:any x]]
  (core/basic-rest x))

(ts/def-default-set-method count [[:any x]]
  (core/basic-count x))

(setdispatch/def-set-method count [[:array x]]
  (alength x))

(ts/def-default-set-method cast [[:any dst-type]
                                 [:any src-value]]
  (core/cast dst-type src-value))

;; Mainly when working with array indices
(defn to-int [x]
  (cast Integer/TYPE x))



;; Normalize a value to a type such that when we apply rest, we get the same type back.
(ts/def-default-set-method iterable [[:any x]]
  (core/iterable x))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Iteration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reduce
  ([f input0]
   (c/let [input (iterable input0)]
     (reduce f (first input) (rest input))))
  ([f result input]
   (core/basic-loop {:init {:result result
                            :remain (iterable input)}
                     :remain input
                     :eval identity
                     :loop? (comp not empty? :remain)
                     :next (fn [x]
                             {:result (f (:result x) (first (:remain x)))
                              :remain (rest (:remain x))})
                     :result :result})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Transducers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map [f]
  {:pre [(fn? f)]}
  (fn [s]
    {:pre [(wrapped-step? s)]}
    (c/update s :step (fn [step] (fn [result x] (step result (f x)))))))

(defn filter [f]
  {:pre [(fn? f)]}
  (fn [s]
    {:pre [(wrapped-step? s)]}
    (c/update s :step (fn [step]
                        (fn [result x]
                          (core/If (f x)
                              (step result x)
                              result))))))

(defn transduce [transduce-function
                 step-function
                 accumulator
                 src-collection]
  (let [tr (transduce-function (wrap-step step-function))]
    ((:unwrap tr)
     (reduce (:step tr)
             ((:wrap tr) accumulator)
             src-collection))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Sliceable array
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sliceable-array
  ([src-array]
   (sliceable-array src-array (alength src-array)))
  ([src-array size]
   (sliceable-array src-array size (wrap (c/int 0))))
  ([src-array size offset]
   (let [k {:type :sliceable-array
            :data src-array
            :size (to-int size)
            :offset (to-int offset)}]
     k)))

(setdispatch/def-set-method count [[[:map-type :sliceable-array] arr]]
  (:size arr))

(setdispatch/def-set-method first [[[:map-type :sliceable-array] arr]]
  (aget (:data arr) (:offset arr)))

(setdispatch/def-set-method rest [[[:map-type :sliceable-array] arr]]
  (c/merge arr
           {:size (to-int (dec (:size arr)))
            :offset (to-int (inc (:offset arr)))}))

(setdispatch/def-set-method iterable [[[:seed :array] x]]
  (sliceable-array x))

(setdispatch/def-set-method empty? [[[:map-type :sliceable-array] arr]]
  (== 0 (:size arr)))

(ts/def-default-set-method slice [[[:map-type :sliceable-array] arr]
                                  [(ts/maybe-seed-of :integer) from]
                                  [(ts/maybe-seed-of :integer) to]]
  (c/merge arr
           {:offset (to-int (+ (:offset arr) from))
            :size (to-int (- to from))}))


(ts/def-default-set-method slice [[[:seed :array] x]
                                  [(ts/maybe-seed-of :integer) from]
                                  [(ts/maybe-seed-of :integer) to]]
  (slice (sliceable-array x) from to))

(defn slice-from [src from]
  (slice src from (count src)))

(defn slice-to [src to]
  (slice src 0 to))

(defn slice-but [src n]
  (slice-to src (- (count src) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Structured arrays
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrap-struct-array [type src-data]
  (let [struct-size (core/size-of type)]
    {:data src-data
     :type :struct-array
     :public-type type
     :struct-size struct-size
     :size (to-int (quot (cast Long/TYPE (alength src-data))
                         (cast Long/TYPE struct-size)))
     :offset (wrap (c/int 0))}))

(defn make-struct-array [public-type private-type size]
  (wrap-struct-array public-type (make-array private-type (* size (core/size-of public-type)))))

(defn populate-and-cast [dst-type src]
  {:pre [(c/vector? src)]}
  (let [flat-dst (core/flatten-expr dst-type)]
    (c/assert (c/= (c/count flat-dst)
                   (c/count src)))
    (core/populate-seeds
     dst-type
     (c/map (fn [d s]
              (cast (defs/datatype d) s))
            flat-dst
            src))))

(defn compute-struct-array-offset [src i]
  (* (+ (:offset src) i)
     (:struct-size src)))

(setdispatch/def-set-method aget [[[:map-type :struct-array] arr]
                                  [(ts/maybe-seed-of :integer) i]]
  (let [at (compute-struct-array-offset arr i)]
    (populate-and-cast
     (:public-type arr)
     (c/vec
      (c/map (fn [p] (aget (:data arr) (to-int (+ at p))))
             (c/range (:struct-size arr)))))))
