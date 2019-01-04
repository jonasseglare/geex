(ns geex.base

  "Main API for using Geex independently of which platform you are using it for."
  
  (:require [geex.core :as core]
            [clojure.core :as c]
            [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as dt]
            [geex.core.xplatform :as xp]
            [geex.ebmd.type :as gtype]
            [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.wip.debug :as debug]
            [bluebell.utils.render-text :as render-text]
            [bluebell.utils.ebmd.type :as etype]
            [geex.java.defs :as jdefs]
            [clojure.pprint :as pp])
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

(ebmd/declare-def-poly
 disp-sub [etype/any x]
 (pp/pprint x)
 x)

(defmacro disp [x]
  `(do
     (c/println "\nDISP " ~(c/str x))
     (disp-sub ~x)))

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
    (debug/exception-hook
     (apply f (c/map wrapper args))
     (render-text/disp
      (render-text/add-line "Error for these raw args")
      (render-text/pprint args)))))

(def wrap-numeric-args (c/partial wrap-args number-to-seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Various utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro generalizable-fn [name arglist & body]
  `(do
     (ebmd/declare-poly ~name)
     (ebmd/def-poly ~name
       ~(c/reduce c/into []
                  (c/mapv (fn [a]
                            ['etype/any a])
                          arglist))
       ~@body)))

(c/defn- make-arglist [arg-spec n]
  (c/mapv (fn [i] (c/symbol (c/str "arg" i))) (c/range n)))

;; Declare a function in a way that makes it overridable,
;; where all arg-specs are the same
(defmacro generalize-fn [new-name arg-spec arg-count specific-fn]
  (let [arg-symbols (make-arglist arg-spec arg-count)
        arg-types (c/vec (c/take arg-count (c/repeat arg-spec)))
        arg-list (c/reduce c/into [] (c/map c/vector arg-types arg-symbols))]
    `(ebmd/declare-def-poly ~new-name ~arg-list
                            (~specific-fn ~@arg-symbols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Polymorphic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def xp-numeric (comp wrap-numeric-args xp/caller))

;; Bit ops
(generalize-fn bit-not ::gtype/integer-seed
               1 (xp-numeric :bit-not))
(ebmd/def-poly bit-not [::gtype/integer-value x]
  (c/bit-not x))
(generalize-fn bit-shift-left ::gtype/integer-seed
               2 (xp-numeric :bit-shift-left))
(ebmd/def-poly bit-shift-left [::gtype/integer-value x
                               ::gtype/integer-value y]
  (c/bit-shift-left x y))

(generalize-fn bit-shift-right ::gtype/integer-seed
               2 (xp-numeric :bit-shift-right))
(ebmd/def-poly bit-shift-right [::gtype/integer-value x
                               ::gtype/integer-value y]
  (c/bit-shift-right x y))

(generalize-fn unsigned-bit-shift-right ::gtype/integer-seed
               2 (xp-numeric :unsigned-bit-shift-right))
(ebmd/def-poly unsigned-bit-shift-right [::gtype/integer-value x
                                        ::gtype/integer-value y]
  (c/unsigned-bit-shift-right x y))

(generalize-fn binary-bit-flip ::gtype/integer-seed
               2 (xp-numeric :bit-flip))
(ebmd/def-poly binary-bit-flip [::gtype/integer-value x
                                ::gtype/integer-value y]
  (c/bit-flip x y))
(generalize-fn binary-bit-and ::gtype/integer-seed
               2 (xp-numeric :bit-and))
(ebmd/def-poly binary-bit-and [::gtype/integer-value x
                               ::gtype/integer-value y]
  (c/bit-and x y))

(generalize-fn binary-bit-or ::gtype/integer-seed
               2 (xp-numeric :bit-or))
(ebmd/def-poly binary-bit-or [::gtype/integer-value x
                              ::gtype/integer-value y]
  (c/bit-or x y))






(generalize-fn negate ::gtype/real 1
               (xp-numeric :negate))
(ebmd/def-poly negate [::gtype/real-value x]
  (c/- x))

(generalize-fn binary-add ::gtype/real 2
               (xp-numeric :binary-add))
(ebmd/def-poly binary-add [::gtype/real-value a
                           ::gtype/real-value b]
  (c/+ a b))

(generalize-fn unary-add ::gtype/real 1
               (xp-numeric :unary-add))
(ebmd/def-poly unary-add [::gtype/real-value x]
  (c/+ x))

(generalize-fn binary-sub ::gtype/real 2
               (xp-numeric :binary-sub))
(ebmd/def-poly binary-sub [::gtype/real-value x
                           ::gtype/real-value y]
  (c/- x y))

(generalize-fn binary-div ::gtype/real 2
               (xp-numeric :binary-div))
(ebmd/def-poly binary-div [::gtype/real-value x
                           ::gtype/real-value y]
  (c// x y))

;; In order to be consistent with generated code:
(ebmd/def-poly binary-div [::gtype/integer-value x
                           ::gtype/integer-value y]
  (c/quot x y))

(generalize-fn binary-mul ::gtype/real 2
               (xp-numeric :binary-mul))
(ebmd/def-poly binary-mul [::gtype/real-value a
                           ::gtype/real-value b]
  (c/* a b))

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

(ebmd/declare-poly nil?)
(ebmd/def-poly nil? [::gtype/seed x]
  (core/basic-nil? x))
(ebmd/def-poly nil? [::etype/any x]
  (c/nil? x))

(def call-method (xp/caller :call-method))

(def flatten-expr core/flatten-expr)
(def size-of core/size-of)
(def populate-seeds core/populate-seeds)

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
                      (binary-div 1 (c/first args)))

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



;;;------- Platform properties -------
(def int-type (xp/caller :int-type))
(def float-type (xp/caller :float-type))
(def size-type (xp/caller :size-type))



;;;------- Errors -------

(defn error [message]
  (xp/call :error message))

(defmacro check
  ([condition]
   (check condition "(no message)"))
  ([condition message]
   {:pre [(c/string? message)]}
   (let [full-message (c/format "CHECK '%s' FAILED: %s"
                                (c/str condition)
                                message)]
     `(core/If ~condition
               [] ;; TODO: What about void???
               (do (error ~full-message)
                   [])))))


;;;------- More math functions -------
(defn inc [x]
  (+ x 1))

(defn dec [x]
  (- x 1))

(defn sqr [x]
  (* x x))

(generalize-fn quot ::gtype/real 2 (xp-numeric :quot))
(ebmd/def-poly quot [::gtype/real-value x
                     ::gtype/real-value y]
  (c/quot x y))

(generalize-fn rem ::gtype/real 2 (xp-numeric :rem))
(ebmd/def-poly rem [::gtype/real-value x
                    ::gtype/real-value y]
  (c/rem x y))

(defmacro math-functions-from-java []
  `(do
     ~@(c/map
        (fn [[k _ arg-count]]
          (let [sym (-> k
                        c/name
                        c/symbol)]
            (c/assert (c/symbol? sym))
            `(generalize-fn ~sym ::gtype/real
                            ~arg-count (xp-numeric ~k))))
        jdefs/math-functions)))
(math-functions-from-java)


;;;------- Comparison operators -------

(generalize-fn <= ::gtype/comparable  2 (xp-numeric :<=))
(ebmd/def-poly <= [::gtype/comparable-value a
                   ::gtype/comparable-value b]
  (c/<= a b))
(generalize-fn >= ::gtype/comparable  2 (xp-numeric :>=))
(ebmd/def-poly >= [::gtype/comparable-value a
                   ::gtype/comparable-value b]
  (c/>= a b))
(generalize-fn > ::gtype/comparable  2 (xp-numeric :>))
(ebmd/def-poly > [::gtype/comparable-value a
                  ::gtype/comparable-value b]
  (c/> a b))
(generalize-fn < ::gtype/comparable 2 (xp-numeric :<))
(ebmd/def-poly < [::gtype/comparable-value a
                  ::gtype/comparable-value b]
  (c/< a b))

(generalize-fn != ::gtype/seed 2 (xp-numeric :!=))
(ebmd/def-poly != [::gtype/not-seed a
                   ::gtype/not-seed b]
  (c/not= a b))
(generalize-fn == ::gtype/seed 2 (xp-numeric :==))
(ebmd/def-poly == [::gtype/not-seed a
                   ::gtype/not-seed b]
  (c/= a b))


(generalize-fn finite? ::gtype/real 1 (xp-numeric :finite?))
(generalize-fn infinite? ::gtype/real 1 (xp-numeric :infinite?))
(generalize-fn nan? ::gtype/real 1 (xp-numeric :nan?))


;;;------- More math functions -------

(defn pos? [x]
  (< 0 x))

(defn neg? [x]
  (< x 0))

(defn zero? [x]
  (== x 0))

(ebmd/declare-def-poly
 mod [::gtype/real a
      ::gtype/real b]
 (let [c (rem a b)]
   (core/If (< c 0)
            (+ c b)
            c)))

;;;------- Logic operators -------

(defmacro and [& args]
  (if (c/empty? args)
    true
    `(core/If ~(c/first args)
              (and ~@(c/rest args))
              false
              )))

(defmacro or [& args]
  (if (c/empty? args)
    false
    `(core/If ~(c/first args)
              true
              (or ~@(c/rest args)))))

(ebmd/declare-poly not)
(ebmd/def-poly not [::etype/any x]
  (c/not x))
(ebmd/def-poly not [::gtype/seed x]
   (xp/call :not x))

(defmacro implies [a b]
  `(or (not ~a) ~b))





(ebmd/declare-poly simple=)
(ebmd/def-poly simple= [::gtype/any x
                        ::gtype/any y]
  (xp/call := x y))

(ebmd/def-poly simple= [::gtype/not-seed x
                        ::gtype/not-seed y]
  (c/= x y))

(ebmd/declare-poly =)

(defn and-fn-2 [x y]
  (and x y))

(defn and-fn [& args]
  (case (c/count args)
    0 true
    1 (c/first args)
    (c/reduce and-fn-2 args)))

(defn nested= [x y]
  (and (c/= (core/type-signature x)
            (core/type-signature y))
       (c/apply
        and-fn
        (c/map (fn [a b] (simple= a b))
               (core/flatten-expr x)
               (core/flatten-expr y)))))

(ebmd/def-poly = [::gtype/coll-value x
                  ::gtype/coll-value y]
  (nested= x y))

(ebmd/def-poly = [::etype/any x
                  ::etype/any y]
  (simple= x y))

(def not= (comp not =))





;;;------- Array functions -------
(def array-class dt/array-class)


(def make-array (xp/caller :make-array))

;; aget
(ebmd/declare-poly aget)
(ebmd/def-poly aget [::gtype/array-seed x
                     ::gtype/integer i]
  (xp/call :aget x i))
(ebmd/def-poly aget [::gtype/array-value x
                     ::gtype/integer-value i]
  (c/aget x i))

(ebmd/declare-poly aset)
(ebmd/def-poly aset [::gtype/array-seed x
                     ::gtype/integer i
                     ::etype/any value]
  (xp/call :aset x i value))
(ebmd/def-poly aset [::gtype/array-value x
                     ::gtype/integer-value i
                     ::etype/any value]
  (c/aset x i value))

(ebmd/declare-poly alength)
(ebmd/def-poly alength [::gtype/array-seed x]
  (xp/call :alength x))
(ebmd/def-poly alength [::gtype/array-value x]
  (c/alength x))


;;;------- Collection functions -------
(ebmd/declare-poly conj)
(ebmd/def-poly conj [::gtype/coll-seed dst
                     ::etype/any x]
  (xp/call :conj dst x))
(ebmd/def-poly conj [::gtype/coll-value dst
                     ::etype/any x]
  (c/conj dst x))

(ebmd/declare-poly seq)
(ebmd/def-poly seq [::gtype/coll-seed x]
  (xp/call :seq x))
(ebmd/def-poly seq [::gtype/coll-value x]
  (c/seq x))


(ebmd/declare-poly empty?)
(ebmd/def-poly empty? [::gtype/coll-seed x]
  (nil? (seq x)))
(ebmd/def-poly empty? [::gtype/coll-value x]
  (c/empty? x))


(ebmd/declare-poly first)
(ebmd/def-poly first [::gtype/coll-seed x]
  (xp/call :first x))
(ebmd/def-poly first [::gtype/coll-value x]
  (c/first x))

(ebmd/declare-poly rest)
(ebmd/def-poly rest [::gtype/coll-seed x]
  (xp/call :rest x))
(ebmd/def-poly rest [::gtype/coll-value x]
  (c/rest x))

(ebmd/declare-poly count)
(ebmd/def-poly count [::gtype/coll-seed x]
  (xp/call :count x))
(ebmd/def-poly count [::gtype/coll-value x]
  (c/count x))
(ebmd/def-poly count [::gtype/array x]
  (alength x))


(generalizable-fn cast [dst-type src-value]
  (core/cast dst-type src-value))

(c/doseq [prom gtype/all-numeric-promotions]
  (let [[from to] prom
        [from-seed to-seed] (gtype/promotion-of-type :seed prom)
        cl (c/get-in gtype/elementary-types [to :class])]
    (ebmd/register-promotion to-seed
                             (c/partial cast cl)
                             from-seed
                             2.0 ;; Extra cost for casting at runtime
                             )))

(gtype/make-value-to-seed-promotions core/wrap)



;; Mainly when working with array indices
(defn to-size-type [x]
  (cast (size-type) x))



;; Normalize a value to a type such that when we apply rest, we get the same type back.


(ebmd/declare-poly iterable)

(ebmd/def-poly iterable [etype/any x]
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
   (core/Loop
    [result result
     remain (iterable input)]
    (core/If (empty? remain)
             result
             (core/Recur
              (f result
                 (first remain))
              (rest remain))))))

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
            :size (to-size-type size)
            :offset (to-size-type offset)}]
     k)))

(ebmd/def-arg-spec ::sliceable-array
  (gtype/map-with-key-value
   :type :sliceable-array))

(ebmd/def-poly count [::sliceable-array arr]
  (:size arr))

(ebmd/def-poly first [::sliceable-array arr]
  (aget (:data arr) (:offset arr)))

(ebmd/def-poly rest [::sliceable-array arr]
  (c/merge arr
           {:size (to-size-type (dec (:size arr)))
            :offset (to-size-type (inc (:offset arr)))}))

(ebmd/def-poly iterable [gtype/array-seed x]
  (sliceable-array x))

(ebmd/def-poly empty? [::sliceable-array arr]
  (== 0 (:size arr)))

(ebmd/declare-poly slice)

(ebmd/def-poly slice [::sliceable-array arr
                      ::gtype/integer from
                      ::gtype/integer to]
  (c/merge arr
           {:offset (to-size-type (+ (:offset arr) from))
            :size (to-size-type (- to from))}))


(ebmd/def-poly slice [::gtype/array-seed x
                      ::gtype/integer from
                      ::gtype/integer to]
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
  ([n] (range (c/long 0) n))
  ([lower upper] (range lower upper (c/long 1)))
  ([lower0 upper0 step0]
   (let [lower (wrap lower0)
         upper (wrap upper0)
         step (wrap step0)]
     {:type :range
      :offset lower
      :size (/ (- upper lower) step)
      :step step})))

(ebmd/def-arg-spec ::range (gtype/map-with-key-value :type :range))

(ebmd/def-poly count [::range x]
  (:size x))

(ebmd/def-poly first [::range x]
  (c/assert (map? x))
  (:offset x))

(ebmd/def-poly rest [::range x]
  (c/merge x
           {:offset (+ (:offset x) (:step x))
            :size (dec (:size x))}))

(ebmd/def-poly iterable [::range x] x)

(ebmd/def-poly empty? [::range x]
  (<= (:size x) 0))

(ebmd/def-poly aget [::range x
                     ::gtype/integer i]
  (+ (:offset x)
     (* i (:step x))))

(ebmd/def-poly slice [::range x
                      ::gtype/integer from
                      ::gtype/integer to]
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
  (let [struct-size (size-of type)]
    {:data src-data
     :type :struct-array
     :public-type type
     ::struct-size struct-size
     :size (to-size-type (quot (cast Long/TYPE (alength src-data))
                         (cast Long/TYPE struct-size)))
     :offset (wrap (c/int 0))}))

(ebmd/def-arg-spec struct-size-key-arg
  {:pred (c/partial c/= ::struct-size)
   :pos [::struct-size]
   :neg [:kattskit]})

(ebmd/def-poly core/wrap-at-key? [struct-size-key-arg _]
  false)

(defn make-struct-array [public-type private-type size]
  (wrap-struct-array
   public-type (make-array private-type
                           (* size (size-of public-type)))))

(defn populate-and-cast [dst-type src]
  {:pre [(c/vector? src)]}
  (let [flat-dst (flatten-expr dst-type)]
    (c/assert (c/= (c/count flat-dst)
                   (c/count src)))
    (populate-seeds
     dst-type
     (c/map (fn [d s]
              (cast (seed/datatype d) s))
            flat-dst
            src))))

(defn compute-struct-array-offset [src i]
  (+ (* i (::struct-size src))
     (:offset src)))

(defn aget-struct-array [arr i]
  (let [at (compute-struct-array-offset arr i)]
    (populate-and-cast
     (:public-type arr)
     (c/vec
      (c/map (fn [p] (aget (:data arr) (to-size-type (+ at p))))
             (c/range (::struct-size arr)))))))

(def struct-array-arg (gtype/map-with-key-value
                       :type :struct-array))

(ebmd/def-poly aget [struct-array-arg arr
                     ::gtype/integer i]
  (aget-struct-array arr i))


(defn aset-struct-array [arr i x]
  (let [data (:data arr)
        inner-type (dt/component-type (seed/datatype data))
        at (compute-struct-array-offset arr i)
        flat-x (flatten-expr x)
        n (::struct-size arr)]
    (c/assert (c/number? n))
    (c/assert (c/= (c/count flat-x) n))
    (c/doseq [i (c/range n)]
      (aset data i (cast inner-type (c/nth flat-x i))))))

(ebmd/def-poly aset [struct-array-arg arr
                     ::gtype/integer i
                     ::etype/any x]
  (aset-struct-array arr i x))

(ebmd/def-poly count [struct-array-arg arr]
  (:size arr))

(ebmd/def-poly first [struct-array-arg arr]
  (aget-struct-array arr 0))

(ebmd/def-poly rest [struct-array-arg arr]
  (c/merge arr {:offset (+ (::struct-size arr)
                           (:offset arr))
                :size (to-size-type (dec (:size arr)))}))

(ebmd/def-poly iterable [struct-array-arg x] x)

(ebmd/def-poly empty? [struct-array-arg x]
  (<= (:size x) 0))

(ebmd/def-poly slice [struct-array-arg arr
                      ::gtype/integer lower
                      ::gtype/integer upper]
  (c/merge arr
           {:size (- upper lower)
            :offset (+ (:offset arr)
                       (* lower (::struct-size arr)))}))


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
  (core/Loop [state initial-state]
             (core/If (loop-condition-fn state)
                      (core/Recur (next-state-fn state))
                      state)))

(defn iterate-times [n init-state next-state]
  {:pre [(fn? next-state)]}
  (core/Loop [n n
              state init-state]
             (core/If (< 0 n)
                      (core/Recur (dec n) (next-state state))
                      state)))

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
      []
      ~input-seq)
     (void)))
