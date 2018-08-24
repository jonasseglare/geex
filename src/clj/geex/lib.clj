(ns geex.lib
  (:require [geex.core :as core]
            [clojure.core :as c]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [bluebell.utils.setdispatch :as setdispatch]
            [bluebell.utils.lufn :as lufn]
            [geex.core.typesystem :as ts]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as dt])
  (:refer-clojure :only [defn fn apply defmacro case comp identity fn? let map?]))


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
(def numeric-lufn-op (comp wrap-numeric-args lufn-with-platform))

(def negate (numeric-op core/negate))
(def binary-add (numeric-op core/binary-add))
(def binary-sub (numeric-op core/binary-sub))
(def binary-div (numeric-op core/binary-div))
(def binary-mul (numeric-op core/binary-mul))

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

(def quot (numeric-op core/platform-quot))
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

(def not (with-platform core/platform-not))

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

(def empty? (comp nil? seq))

(ts/def-default-set-method first [[:any x]]
  (core/basic-first x))

(ts/def-default-set-method rest [[:any x]]
  (core/basic-rest x))

(ts/def-default-set-method count [[:any x]]
  (core/basic-count x))

(ts/def-default-set-method cast [[:any dst-type]
                                 [:any src-value]]
  (core/cast dst-type src-value))


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
            :size size
            :offset offset}]
     k)))

(setdispatch/def-set-method count [[[:map-type :sliceable-array] arr]]
  (:size arr))

(setdispatch/def-set-method first [[[:map-type :sliceable-array] arr]]
  (aget (:data arr) (:offset arr)))

(setdispatch/def-set-method rest [[[:map-type :sliceable-array] arr]]
  (c/merge arr
           {:size (dec (:size arr))
            :offset (inc (:offset arr))}))

(setdispatch/def-set-method iterable [[[:seed :array] x]]
  (sliceable-array x))
