(ns geex.feature-test
  (:import [java.util ArrayList]
           [java.awt Point])
  (:require [geex.core :as core]
            [geex.java :as java]
            [geex.common :as c]
            [clojure.test :refer :all]))


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