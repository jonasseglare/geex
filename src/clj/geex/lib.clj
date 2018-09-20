(ns geex.lib
  (:require [geex.core :as core]
            [clojure.core :as c]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [bluebell.utils.wip.setdispatch :as setdispatch]
            [geex.core.typesystem :as ts]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as dt]
            [geex.core.xplatform :as xp]
            [geex.ebmd.type :as geextype]
            [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.type :as etype]
            [geex.java.defs :as jdefs])
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

#_(defmacro generalize-fns [names]
  `(do
     ~@(c/map
        (fn [[fn-name arg-count]]
          `(generalize-fn ~(c/symbol fn-name) ~arg-count
                          (xp-numeric
                           ~(-> fn-name
                                c/symbol))))
        names)))
#_(generalize-fns
   [["bit-and" 2]])

(generalize-fn bit-not 1 (xp-numeric :bit-not))
(generalize-fn bit-shift-left 2 (xp-numeric :bit-shift-left))
(generalize-fn unsigned-bit-shift-left 2
               (xp-numeric :unsigned-bit-shift-left))
(generalize-fn bit-shift-right
               2 (xp-numeric :bit-shift-right))
(generalize-fn unsigned-bit-shift-right 2
               (xp-numeric :unsigned-bit-shift-right))


(generalize-fn binary-bit-flip 2 (xp-numeric :bit-flip))
(generalize-fn binary-bit-and 2 (xp-numeric :bit-and))
(generalize-fn binary-bit-or 2 (xp-numeric :bit-or))

(generalize-fn negate 1 (xp-numeric :negate))
(generalize-fn binary-add 2 (xp-numeric :binary-add))
(generalize-fn unary-add 1 (xp-numeric :unary-add))
(generalize-fn binary-sub 2 (xp-numeric :binary-sub))
(generalize-fn binary-div 2 (xp-numeric :binary-div))
(generalize-fn binary-mul 2 (xp-numeric :binary-mul))

(def basic-random (xp/caller :basic-random))


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

(def void (xp/caller :make-void))

(def nil? core/basic-nil?)

(def call-method (xp/caller :call-method))

;;;------- Common math operators -------

(generalize-binary-op + binary-add args
                      0
                      (unary-add (c/first args)))

(generalize-binary-op - binary-sub args
                      0
                      (negate (c/first args)))

(defn insufficient-number-of-args [op-name]
  
  (throw
   (c/ex-info
    (c/str "Insufficient number of arguments to '"
           op-name
           "'")
    {})))

(generalize-binary-op / binary-div args
                      (insufficient-number-of-args "/")
                      (c/first args))

(generalize-binary-op * binary-mul args
                      1
                      (c/first args))

(generalize-binary-op bit-and binary-bit-and args
                      (insufficient-number-of-args
                       "bit-and")
                      (insufficient-number-of-args
                       "bit-and"))

(generalize-binary-op bit-or binary-bit-or args
                      (insufficient-number-of-args
                       "bit-or")
                      (insufficient-number-of-args
                       "bit-or"))

(generalize-binary-op bit-flip binary-bit-flip args
                      (insufficient-number-of-args
                       "bit-flip")
                      (insufficient-number-of-args
                       "bit-flip"))



;;;------- More math functions -------
(defn inc [x]
  (+ x 1))

(defn dec [x]
  (- x 1))

(defn sqr [x]
  (* x x))

(generalize-fn quot 2 (xp-numeric :quot))
(generalize-fn rem 2 (xp-numeric :rem))

(defmacro math-functions-from-java []
  `(do
     ~@(c/map
        (fn [[k _ arg-count]]
          (let [sym (-> k
                        c/name
                        c/symbol)]
            (c/assert (c/symbol? sym))
            `(generalize-fn ~sym ~arg-count (xp-numeric ~k))))
        jdefs/math-functions)))
(math-functions-from-java)

;(generalize-fn sqrt 1 (xp-numeric :sqrt))

;;;------- Comparison operators -------

(generalize-fn == 2 (xp-numeric :==))
(generalize-fn <= 2 (xp-numeric :<=))
(generalize-fn >= 2 (xp-numeric :>=))
(generalize-fn > 2 (xp-numeric :>))
(generalize-fn < 2 (xp-numeric :<))
(generalize-fn != 2 (xp-numeric :!=))

(generalize-fn = 2 (xp/caller :=))

(generalize-fn finite? 1 (xp-numeric :finite?))
(generalize-fn infinite? 1 (xp-numeric :infinite?))
(generalize-fn nan? 1 (xp-numeric :nan?))



;;;------- More math functions -------

(defn pos? [x]
  (< 0 x))

(defn neg? [x]
  (< x 0))

(defn zero? [x]
  (== x 0))

(generalizable-fn mod [a b]
                  (let [c (rem a b)]
                    (core/If (< c 0)
                             (+ c b)
                             c)))

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

(ebmd/declare-poly first)

(ebmd/def-poly first [etype/any x]
  (xp/call :first x))

(generalizable-fn rest [x]
  (xp/call :rest x))

(ebmd/declare-poly count)

(ebmd/def-poly count [etype/any x]
  (xp/call :count x))


(generalizable-fn cast [dst-type src-value]
  (core/cast dst-type src-value))

;; Mainly when working with array indices
(defn to-int [x]
  (cast Integer/TYPE x))



;; Normalize a value to a type such that when we apply rest, we get the same type back.
(generalizable-fn iterable [x]
                  (xp/call :iterable x))

(defn result-vector
  "Returns an empty vector suitable for conj-ing into."
  []
  (cast clojure.lang.IPersistentCollection (wrap [])))








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
                     ;:remain input
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
  (c/and (map? x)
       (fn? (:wrap x))
       (fn? (:unwrap x))
       (fn? (:step x))))

(defn bad-wrapped-step? [x]
  ;;; See jo-reproduced-bug!!!!
  (and true                             ;(map? x)
       true
                                        ;(fn? (:wrap x))
                                        ;(fn? (:unwrap x))
                                        ;(fn? (:step x))
       ))

(defn wrap-step [step]

  ;; THIS IS GOOD
  {:pre [(c/or (wrapped-step? step)
                 (fn? step))]}

  ;; This used to be BAD, is it still???
  ;{:pre [
  
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
  (bad-wrapped-step? step-function)
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

(def sliceable-array-arg (geextype/map-with-key-value
                          :type :sliceable-array))

(ebmd/def-poly count [sliceable-array-arg arr]
  (:size arr))

(ebmd/def-poly first [sliceable-array-arg arr]
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  More slicing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn slice-from [src from]
  (slice src from (count src)))

(defn slice-to [src to]
  (slice src 0 to))

(defn slice-but [src n]
  (slice-to src (- (count src) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ranges
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn range
  ([n] (range 0 n))
  ([lower upper] (range lower upper 1))
  ([lower0 upper0 step0]
   (let [lower (wrap lower0)
         upper (wrap upper0)
         step (wrap step0)]
     {:type :range
      :offset lower
      :size (/ (- upper lower) step)
      :step step})))

(def range-arg (geextype/map-with-key-value :type :range))

(ebmd/def-poly count [range-arg x]
  (:size x))

(ebmd/def-poly first [range-arg x]
  (c/assert (map? x))
  (:offset x))

(setdispatch/def-set-method rest [[[:map-type :range] x]]
  (c/merge x
           {:offset (+ (:offset x) (:step x))
            :size (dec (:size x))}))

(setdispatch/def-set-method iterable [[[:map-type :range] x]] x)

(setdispatch/def-set-method empty? [[[:map-type :range] x]]
  (<= (:size x) 0))

(setdispatch/def-set-method aget [[[:map-type :range] x]
                                  [(ts/maybe-seed-of :integer) i]]
  (+ (:offset x)
     (* i (:step x))))

(ts/def-default-set-method slice [[[:map-type :range] x]
                                  [(ts/maybe-seed-of :integer) from]
                                  [(ts/maybe-seed-of :integer) to]]
  (c/merge x
           {:offset (+ (:offset x)
                       (* from (:step x)))
            :size (- to from)}))


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
  (wrap-struct-array
   public-type (make-array private-type
                           (* size (core/size-of public-type)))))

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
  (+ (* i (:struct-size src))
     (:offset src)))

(defn aget-struct-array [arr i]
  (let [at (compute-struct-array-offset arr i)]
    (populate-and-cast
     (:public-type arr)
     (c/vec
      (c/map (fn [p] (aget (:data arr) (to-int (+ at p))))
             (c/range (:struct-size arr)))))))

(setdispatch/def-set-method aget [[[:map-type :struct-array] arr]
                                  [(ts/maybe-seed-of :integer) i]]
  (aget-struct-array arr i))


(defn aset-struct-array [arr i x]
  (let [data (:data arr)
        inner-type (dt/component-type (seed/datatype data))
        at (compute-struct-array-offset arr i)
        flat-x (core/flatten-expr x)
        n (:struct-size arr)]
    (c/assert (c/number? n))
    (c/assert (c/= (c/count flat-x) n))
    (c/doseq [i (c/range n)]
      (aset data i (cast inner-type (c/nth flat-x i))))))

(setdispatch/def-set-method aset [[[:map-type :struct-array] arr]
                                  [(ts/maybe-seed-of :integer) i]
                                  [:any x]]
  (aset-struct-array arr i x))

(def struct-array-arg (geextype/map-with-key-value
                       :type :struct-array))

(ebmd/def-poly count [struct-array-arg arr]
  (:size arr))

(ebmd/def-poly first [struct-array-arg arr]
  (aget-struct-array arr 0))

(setdispatch/def-set-method rest [[[:map-type :struct-array] arr]]
  (c/merge arr {:offset (+ (:struct-size arr)
                           (:offset arr))
                :size (to-int (dec (:size arr)))}))

(setdispatch/def-set-method iterable [[[:map-type :struct-array] x]] x)

(setdispatch/def-set-method empty? [[[:map-type :struct-array] x]]
  (<= (:size x) 0))

(setdispatch/def-set-method slice [[[:map-type :struct-array] arr]
                                   [(ts/maybe-seed-of :integer) lower]
                                   [(ts/maybe-seed-of :integer) upper]]
  (c/merge arr
           {:size (- upper lower)
            :offset (+ (:offset arr)
                       (* lower (:struct-size arr)))}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  More control structures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn iterate-while [initial-state
                     next-state-fn
                     loop-condition-fn]
  {:pre [(fn? next-state-fn)
         (fn? loop-condition-fn)]}
  (core/basic-loop
   {:init initial-state
    :eval identity
    :loop? loop-condition-fn
    :next next-state-fn
    :result identity}))

(defn iterate-until [initial-state
                     next-state-fn
                     stop-condition-fn]
  {:pre [(fn? stop-condition-fn)]}
  (iterate-while initial-state next-state-fn
                 (c/comp not stop-condition-fn)))

(defmacro doseq [[item input-seq] & body]
  `(do
     (reduce
      (fn [result# ~item]
        ~@body
        result#)
      (wrap nil)
      ~input-seq)
     (void)))
