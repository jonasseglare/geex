(ns geex.lib-test
  (:require [geex.java :as java :refer [typed-defn] :as java]
            [geex.core :as core]
            [geex.lib :as lib]
            [bluebell.utils.symset :as ss]
            [clojure.test :refer :all]))

(typed-defn add-3 [Double/TYPE a
                   Double/TYPE b
                   Double/TYPE c]
            (lib/+ a b c))

(deftest add-3-test
  (is (= (add-3 9.0 4.1 1.15)
         14.25)))

(typed-defn and-3
            [Boolean/TYPE a
             Boolean/TYPE b
             Boolean/TYPE c
             ]
            (lib/and a b c))

(defn gen-combos [n]
  (if (= n 1)
    [[true] [false]]
    (transduce
     (comp (map (fn [combo]
                  [(conj combo false) (conj combo true)]))
           cat)
     conj
     []
     (gen-combos (dec n)))))

(defn true-and-3 [a b c]
  (and a b c))

(deftest and-3-test
  (doseq [combo (gen-combos 3)]
    (let [actual (apply and-3 combo)
          expected (apply true-and-3 combo)]
      (is (= actual expected))
      (when (not= actual expected)
        (println "Combo" combo)
        (println "Actual" actual)
        (println "Expected" expected)))))


(typed-defn or-3
            [Boolean/TYPE a
             Boolean/TYPE b
             Boolean/TYPE c]
            (lib/or a b c))

(defn true-or-3 [a b c]
  (or a b c))

(deftest test-or-3
  (doseq [combo (gen-combos 3)]
    (is (= (apply or-3 combo)
           (apply true-or-3 combo)))))

(typed-defn add-with-constant
            [Long/TYPE x]
            (lib/+ x 119))

(deftest add-with-constant-test
  (is (= 121 (add-with-constant 2))))

(typed-defn my-negate [Double/TYPE x]
            (lib/- x))

(deftest my-negate-test
  (is (= -3.0 (my-negate 3))))

(typed-defn my-sub [Double/TYPE a
                    Double/TYPE b]
            (lib/- a b))

(deftest my-sub-test
  (is (= -314.0
         (my-sub 10 324))))

(typed-defn my-sub-3 [Double/TYPE a
                      Double/TYPE b
                      Double/TYPE c]
            (lib/- a b c))

(deftest sub-3-test
  (is (= -12.0
         (my-sub-3 1 4 9))))

(typed-defn my-not [Boolean/TYPE x]
            (lib/not x))

(deftest not-test
  (is (= false (my-not true)))
  (is (= true (my-not false))))

(typed-defn my-implies [Boolean/TYPE a
                        Boolean/TYPE b]
            (lib/implies a b))

(deftest implies-test
  (is (true? (my-implies false false)))
  (is (true? (my-implies false true)))
  (is (false? (my-implies true false)))
  (is (true? (my-implies true true))))

(typed-defn my-raw-eq-test [Long/TYPE a
                            Long/TYPE b]
            (lib/== a b))

(deftest raw-eq-test
  (is (my-raw-eq-test 9 9))
  (is (not (my-raw-eq-test 9 8))))

(typed-defn in-interval? [Long/TYPE a]
            (lib/and (lib/<= 4 a)
                     (lib/<= a 9)))

(deftest in-interval-test
  (is (in-interval? 4))
  (is (not (in-interval? 0))))

(typed-defn compare-against-119 [Long/TYPE x]
            [(lib/== 119 x)
             (lib/<= 119 x)
             (lib/>= 119 x)
             (lib/< 119 x)
             (lib/> 119 x)
             (lib/!= 119 x)
             (lib/= 119 x)
             (lib/not= 119 x)
             ])

(deftest compare-agains-119-test
  (is (= [true true true false false false true false]
         (compare-against-119 119)))
  (is (= [false false true false true true false true]
         (compare-against-119 118)))
  (is (= [false true false true false true false true]
         (compare-against-119 120))))


;; TODO: Comparison of general objects.
(typed-defn eq-ops [clojure.lang.IPersistentVector a
                    clojure.lang.IPersistentVector b]
            (lib/== a b))

(deftest test-common-eq
  (let [x [:a :b :c]]
    (is (eq-ops x x))
    (is (not (eq-ops x [:a :b])))))

(typed-defn mixed-add [Double/TYPE a
                       Long/TYPE b]
            (lib/+ a b))



(deftest mixed-add-test
  (is (= 7.0 (mixed-add 3 4))))


(typed-defn fn-returning-nil [Long/TYPE x]
            (core/If (lib/< x 9)
                     (lib/wrap "Less than 9")
                     (lib/nil-of java.lang.String)))

(deftest check-nil-ret
  (is (= "Less than 9" (fn-returning-nil 1)))
  (is (nil? (fn-returning-nil 19))))

(typed-defn div-120 [Double/TYPE x]
            (lib// 120.0 x))

(deftest div-test
  (is (= 40.0 (div-120 3))))

(typed-defn mul-120 [Double/TYPE x]
            (lib/* 120.0 x))

(deftest mul-test
  (is (= 1200.0 (mul-120 10))))

(typed-defn aset-test-fn [Integer/TYPE size
                       Integer/TYPE magic-pos]
            (let [dst (lib/make-array Double/TYPE size)]
              (lib/aset dst magic-pos 119.0)
              dst))

(deftest aset-test0
  (is (= [0.0 0.0 0.0 119.0 0.0 0.0 0.0 0.0 0.0 0.0]
         (vec (aset-test-fn 10 3)))))

(typed-defn make-magic-array [Integer/TYPE m]
            (let [dst (lib/make-array Double/TYPE 10)]
              (lib/aset dst 3 12.0)
              (lib/aset dst 4 14.0)
              (lib/aset dst 5 (lib/aget dst m))
              dst))

(deftest magic-array-test
  (is (= [0.0 0.0 0.0 12.0 14.0 12.0 0.0 0.0 0.0 0.0]
         (vec (make-magic-array 3)))))

(typed-defn alength-fn [(lib/array-class Double/TYPE) x]
            (lib/alength x))

(deftest alength-test
  (is (= (alength-fn (make-array Double/TYPE 4)) 4)))


(typed-defn some-collections []
            [(lib/wrap {:a 3})
             (lib/wrap [])
             (lib/wrap '())
             (lib/wrap #{:a})])

(deftest some-coll-test
  (is (= (some-collections)
         [{:a 3} [] () #{:a}])))

(typed-defn conj-some-values []
            (lib/conj
             (lib/conj
              (lib/wrap [])
              3) :a))

(deftest test-conj
  (is (= [3 :a]
         (conj-some-values))))

(typed-defn is-it-nil? [(lib/array-class Double/TYPE) x]
            (lib/nil? x))

(deftest is-it-nil-test
  (is (false? (is-it-nil? (make-array Double/TYPE 3))))
  (is (true? (is-it-nil? nil))))

(typed-defn is-it-empty? [clojure.lang.IPersistentVector x]
            (lib/empty? x))

(deftest emptiness-test
  (is (true? (is-it-empty? [])))
  (is (true? (is-it-empty? nil)))
  (is (false? (is-it-empty? [:a]))))

(typed-defn get-the-first [clojure.lang.IPersistentVector v]
            (lib/first v))

(deftest first-test
  (is (= (get-the-first [119 2 3])
         119)))

(typed-defn get-the-count [clojure.lang.IPersistentVector v]
            {:the-count-is (lib/count v)})

(deftest count-test
  (is (= {:the-count-is 3}
         (get-the-count [1 2 4]))))

(typed-defn get-the-first-of-rest [clojure.lang.IPersistentVector v]
            (-> v
                lib/rest
                lib/first))

(deftest rest-test
  (is (= 119
         (get-the-first-of-rest [120 119]))))


(typed-defn first-two-of-obj [java.lang.Object x]
            [(lib/first x)
             (-> x lib/rest lib/first)])

(deftest first-obj-test
  (is (= [3 4]
         (first-two-of-obj [3 4 9 4]))))

(defn squared-norm-impl [src-coll]
  (lib/reduce
   (fn [result input-obj]
     (lib/+ result
            (lib/sqr (lib/unwrap Double/TYPE input-obj))))
   (lib/wrap 0.0)
   src-coll))

(typed-defn squared-norm [java.lang.Object src-coll]
            (squared-norm-impl src-coll))

(deftest squared-norm-test
  (is (= 25.0 (squared-norm '(3.0 4.0)))))

(typed-defn basic-wrapping [Double/TYPE x]
            [x x x])


(deftest basic-wrapping-test
  (is (= [4.0 4.0 4.0]
         (basic-wrapping 4.0))))

(typed-defn squared-norm-v [clojure.lang.IPersistentVector src-coll]
            (squared-norm-impl src-coll))

(deftest squared-norm-v-test
  (is (= 25.0 (squared-norm-v [3.0 4.0]))))

(typed-defn inc-vector-values [clojure.lang.IPersistentVector src]
            (lib/transduce
             (lib/map (comp lib/inc (partial lib/unwrap Double/TYPE)))
             lib/conj
             (lib/cast clojure.lang.IPersistentCollection (lib/wrap []))
             src))

(deftest transducer-test
  (is (= (inc-vector-values [1.0 2.0 3.0])
         [2.0 3.0 4.0])))

(defn small-value? [x]
  (lib/<= x 3.0))

(typed-defn inc-and-keep-small [clojure.lang.IPersistentVector v]
            (lib/transduce
             
             (comp (lib/map (comp lib/inc (partial lib/unwrap Double/TYPE)))

                   identity ;(lib/map identity)
                   
                   )
             
             
             lib/conj
             (lib/cast clojure.lang.IPersistentCollection (lib/wrap []))
             v))
