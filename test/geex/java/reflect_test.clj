(ns geex.java.reflect-test
  (:require [geex.java.reflect :refer :all]
            [clojure.test :refer :all]))

(deftest arglist-matches-test
  (is (arglist-matches?
       [clojure.lang.IPersistentVector]
       [clojure.lang.IPersistentCollection]))
  (is (not (arglist-matches?
            [clojure.lang.IPersistentCollection]
            [clojure.lang.IPersistentVector])))
  (is (not (arglist-matches?
            [clojure.lang.IPersistentCollection]
            [clojure.lang.IPersistentVector
             clojure.lang.IPersistentVector])))
  (is (not (arglist-matches?
            [clojure.lang.IPersistentCollection]
            [clojure.lang.IPersistentVector
             clojure.lang.IPersistentVector])))
  (is (arglist-matches?
       [clojure.lang.IPersistentVector Double/TYPE]
       [clojure.lang.IPersistentCollection Double])))

(deftest list-matching-methods-test
  (is (= 1 (count
            (list-matching-methods
             clojure.lang.RT "conj"
             [clojure.lang.IPersistentVector
              java.lang.Object])))))

(def dominates-scores
  [[clojure.lang.IPersistentCollection
    clojure.lang.IPersistentCollection 0]
   [clojure.lang.IPersistentCollection
    clojure.lang.IPersistentVector -1]
   [Double
    clojure.lang.IPersistentVector nil]])

(defn neg [x]
  (if (nil? x)
    nil
    (- x)))

(deftest domination-test
  (doseq [[a b score] dominates-scores]
    (is (= score (dominates-score a b)))
    (is (= (neg score) (dominates-score b a)))))

(deftest matching-method-test
  (is (get-matching-method
       clojure.lang.RT
       "conj"
       [clojure.lang.IPersistentVector
        Object]))
  (is (thrown?
       Exception
       (get-matching-method
        clojure.lang.RT
        "conj"
        [clojure.lang.IPersistentVector]))))
