(ns geex.lib
  (:require [geex.core :as core]
            [clojure.core :as c]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [bluebell.utils.setdispatch :as setdispatch]
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

(defmacro generalizable-fn [name arglist & body]
  `(ts/def-default-set-method ~name
     ~(c/mapv (fn [a]
                [:any a])
              arglist)
     ~@body))

(defn make-arglist [n]
  (c/mapv (fn [i] (c/symbol (c/str "arg" i))) (c/range n)))

(defmacro generalize-fn [new-name arg-count specific-name]
  (let [arglist (make-arglist arg-count)]
    `(generalizable-fn ~new-name ~arglist
                       (~specific-name ~@arglist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Polymorphic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def xp-numeric (comp wrap-numeric-args xp/caller))

(generalize-fn negate 1 (xp-numeric :negate))
(generalize-fn binary-add 2 (xp-numeric :binary-add))
(generalize-fn binary-sub 2 (xp-numeric :binary-sub))
(generalize-fn binary-div 2 (xp-numeric :binary-div))
(generalize-fn binary-mul 2 (xp-numeric :binary-mul))

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
(def unwrap (xp/caller :unwrap))

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

(generalize-fn quot 2 (xp-numeric :quot))
(generalize-fn rem 2 (xp-numeric :rem))
(generalize-fn sqrt 1 (xp-numeric :sqrt))

;;;------- Comparison operators -------

(generalize-fn == 2 (xp-numeric :==))
(generalize-fn <= 2 (xp-numeric :<=))
(generalize-fn >= 2 (xp-numeric :>=))
(generalize-fn > 2 (xp-numeric :>))
(generalize-fn < 2 (xp-numeric :<))
(generalize-fn != 2 (xp-numeric :!=))

(generalize-fn = 2 (xp/caller :=))

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


(def make-array (xp/caller :make-array))

(ts/def-default-set-method aget [[[:seed :array] x]
                                 [(ts/maybe-seed-of :integer) i]]
  (xp/call :aget x i))

(ts/def-default-set-method aset [[[:seed :array] x]
                              [(ts/maybe-seed-of :integer) i]
                              [:any value]]
  (xp/call :aset x i value))

(ts/def-default-set-method alength [[[:seed :array] x]]
  (xp/call :alength x))


;;;------- Collection functions -------

(generalizable-fn conj [dst x]
  (xp/call :conj dst x))

(generalizable-fn seq [x]
  (xp/call :seq x))

(generalizable-fn empty? [x]
  (nil? (seq x)))

(generalizable-fn first [x]
  (xp/call :first x))

(generalizable-fn rest [x]
  (xp/call :rest x))

(generalizable-fn count [x]
  (xp/call :count x))

(setdispatch/def-set-method count [[:array x]]
  (alength x))

(generalizable-fn cast [dst-type src-value]
  (core/cast dst-type src-value))

;; Mainly when working with array indices
(defn to-int [x]
  (cast Integer/TYPE x))



;; Normalize a value to a type such that when we apply rest, we get the same type back.
(generalizable-fn iterable [x]
  (xp/call :iterable x))








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

(defn wrapped-step? [x]
  ;;; See jo-reproduced-bug!!!!
  (and (map? x)
       (fn? (:wrap x))
       (fn? (:unwrap x))
       (fn? (:step x))))

(defn wrap-step [step]

  ;; THIS IS GOOD
  #_{:pre [(c/or (wrapped-step? step)
                 (fn? step))]}

  ;; This used to be BAD, is it still???
  {:pre [(or (wrapped-step? step) (fn? step))]}
  
  (if (fn? step)
    {:wrap identity
     :unwrap identity
     :step step}
    step))

(defn map [f]
  {:pre [(fn? f)]}
  (fn [s]
    {:pre [(wrapped-step? s)]}
    (c/update s :step (fn [step] (fn [result x] (step result (f x)))))))

(defn filter [f]
  {:pre [(fn? f)]}
  (fn [s]
    {:pre [(wrapped-step? s)                                        
           ]}
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
