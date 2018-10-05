(ns geex.core.datatypes-test
  (:require [geex.core.datatypes :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as cljset]
            [clojure.reflect :as r]
            [bluebell.utils.wip.core :as utils]))

(deftest unboxing-test
  (is (= java.lang.Long/TYPE
         (unboxed-class-of 9))))

(deftest math-op-test
  (let [a Integer/TYPE
        b Integer/TYPE
        c Integer/TYPE]
    (is (= c (binary-math-op-result-type a b))))
  (let [a Boolean/TYPE
        b Character/TYPE
        c Integer/TYPE]
    (is (= c (binary-math-op-result-type a b))))
  (let [a Long/TYPE
        b Character/TYPE
        c Long/TYPE]
    (is (= c (binary-math-op-result-type a b))))
  (let [a Long/TYPE
        b Float/TYPE
        c Float/TYPE]
    (is (= c (binary-math-op-result-type a b))))
  (let [a Character/TYPE
        b Double/TYPE
        c Double/TYPE]
    (is (= c (binary-math-op-result-type a b))))

  (is (= Integer/TYPE
         (unary-plus-minus-result-type Character/TYPE)))
  (is (= Long/TYPE
         (unary-plus-minus-result-type Long/TYPE)))

  (is (= Integer/TYPE
         (bit-op-result-type [Character/TYPE])))

  (is (= Long/TYPE
         (bit-op-result-type [Long/TYPE])))

  )

(deftest typename-test
  (is (= "double[][]"
         (r/typename (class (into-array [(double-array [])])))))
  #_(do
    (is (= (typename (class (double-array [])))
           "double[]"))
    (is (= (typename (class 9))
           "java.lang.Long"))
    (is (= (typename Long/TYPE)
           "long"))))
