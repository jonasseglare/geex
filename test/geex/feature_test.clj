(ns geex.feature-test
  (:import [java.util ArrayList]
           [java.awt Point])
  (:require [geex.core :as core]
            [geex.java :as java]
            [geex.common :as c]
            [clojure.test :refer :all]))

(def darr (c/array-type Double/TYPE))

;; New features are tested here

(java/typed-defn array-call-get [(c/array-type Double/TYPE) x]
                 (x 1))

(java/typed-defn array-call-set [(c/array-type Double/TYPE) x]
                 (x 1 119.0))

(deftest array-call-test
  (is (= 34.0 (array-call-get (double-array [9 34 2]))))
  (let [dst (double-array [0 0 0])]
    (array-call-set dst)
    (is (= 119.0 (aget dst 1)))))

(java/typed-defn
 build-array-list []
 (let [dst (java/new ArrayList)]
   (dst 'add (java/new Integer (int 3)))
   (dst 'add (java/new Integer (int 4)))
   (dst 'add (java/new Integer (int 5)))
   dst))

(deftest method-call-test
  (is (= (build-array-list)
         [3 4 5])))

(java/typed-defn point-to-clojure [Point pt]
                 [(pt "x")
                  (pt "y")])

(java/typed-defn make-point []
                 (let [dst (java/new Point)]
                   (dst "x" 9)
                   (dst "y" 20)
                   dst))

(deftest field-access
  (is (= (point-to-clojure (Point. 3 4))
         [3 4]))
  (is (= (point-to-clojure
          (make-point))
         [9 20])))

(java/typed-defn get-key-in-map [clojure.lang.IPersistentMap m]
                 (m :kattskit))

(java/typed-defn set-key-in-map [clojure.lang.IPersistentMap m]
                 (m :kattskit 119))

(deftest kwd-access
  (is (= 119 (get-key-in-map {:kattskit 119})))
  (is (= {:kattskit 119} (set-key-in-map {}))))

(java/typed-defn ops-on-a-map []
                 (let [rng (c/range 12)
                       m (c/map c/sqr rng)]
                   {:count (c/count m)
                    :first (c/first m)
                    :first-of-rest-rest (-> m
                                            c/rest
                                            c/rest
                                            c/first)
                    :fifth (c/nth m 4) ;; zero-based indexing ;-)
                    :slice-info (let [sliced (c/slice m 7 11)]
                                  {:count (c/count sliced)
                                   :first (c/first sliced)})}))

(deftest map-test
  (is (= (ops-on-a-map)
         {:fifth 16,
          :slice-info {:count 4, :first 49},
          :count 12,
          :first 0,
          :first-of-rest-rest 4})))

(java/typed-defn every-some-test-fn []
                 (let [less-than-10 #(c/< % 10)
                       at-least-10 (c/complement less-than-10)]
                   {:a (c/every?
                        less-than-10
                        (c/range 5))
                    :b (c/every?
                        less-than-10
                        (c/range 100))
                    :c (c/some
                        less-than-10
                        (c/range 100))
                    :d (c/some
                        at-least-10
                        (c/range 7))}))

(deftest test-of-every-and-some
  (is (= (every-some-test-fn)
         {:a true :b false :c true :d false})))

(java/typed-defn dot-product [(c/array-type Double/TYPE) a
                              (c/array-type Double/TYPE) b]
                 (c/reduce c/+ 0.0 (c/map c/* a b)))

(deftest dot-product-test
  (let [a [3 4 5]
        b [9 2 4]]
    (is (= (double (apply + (map * a b)))
           (dot-product (double-array a)
                        (double-array b))))))

(java/typed-defn equal-integers? [(c/array-type Integer/TYPE) a
                                  (c/array-type Integer/TYPE) b]
                 (c/every?
                  identity
                  (c/map
                   c/=
                   a
                   b)))


(deftest int-eq-test
  (is (equal-integers? (int-array [1 4 7])
                       (int-array [1 4 7])))
  (is (not (equal-integers? (int-array [1 4 8])
                            (int-array [1 4 7])))))

(java/typed-defn undefined-f [Boolean/TYPE x]
                 (core/If x
                          119.0
                          ::core/undefined))

(deftest undef-test
  (is (= 119.0 (undefined-f true)))
  (is (= 0.0 (undefined-f false))))

(java/typed-defn first-or-whatever [(c/array-type Double/TYPE) x]
                 (c/first-or-undefined x))

(java/typed-defn look-ahead-f [(c/array-type Double/TYPE) x]
                 (-> x
                     c/look-ahead-seq
                     c/rest
                     c/rest
                     c/first
                     ))

(java/typed-defn only-odd [(c/array-type Double/TYPE) x]
                 (let [s (c/filter c/odd? x)]
                   [(c/first s)
                    (-> s c/rest c/first)
                    ]))

(deftest filter-test
  (is (= 5.0 (look-ahead-f (double-array [3 4 5 6 ]))))
  (is (= {:defined? false :value 0.0}
         (first-or-whatever (double-array []))))
  (is (= {:defined? true :value 9.0}
         (first-or-whatever (double-array [9]))))
  (is (= (only-odd (double-array [1 2 3 4]))
         [1.0 3.0])))



(java/typed-defn drop-comp-lazy [darr x]
                 (->> x
                      (c/drop-while c/even?)
                      (c/drop-while c/odd?)
                      c/first))

(java/typed-defn drop-comp-tr [darr x]
                 (c/transduce
                  (comp (c/drop-while #(c/= 2.0 %))
                        (c/drop-while #(c/= 1.0 %)))
                  c/+
                  0.0
                  x))

(deftest lazy-drop-test
  (is (= (drop-comp-lazy (double-array [1 2 3 4]))
         2.0))
  (is (= (drop-comp-lazy (double-array [2 3 4 5]))
         4.0)))

(deftest transduce-drop-test
  (is (= 6.0 (drop-comp-tr (double-array [3 2 1]))))
  (is (= 5.0 (drop-comp-tr (double-array [1 3 2]))))
  (is (= 5.0 (drop-comp-tr (double-array [1 2 3]))))
  (is (= 3.0 (drop-comp-tr (double-array [2 1 3])))))


(java/typed-defn take-while-odd [darr x]
                 (c/transduce
                  (c/take-while c/odd?)
                  c/+
                  0.0
                  x))

(deftest take-while-test
  (is (= 11.0 (take-while-odd (double-array [1 3 7 0 2 4 5])))))

(java/typed-defn take-while-odd-lazy [darr x]
                 (c/reduce c/+ 0.0 (c/take-while c/odd? x)))

(deftest lazy-take-while-test
  (is (= 4.0
         (take-while-odd-lazy
             (double-array [1 1 1 1 2 1])))))
