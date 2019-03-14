(ns geex.resolved-test
  (:import [java.util ArrayList])
  (:require [geex.core :as core]
            [geex.java :as java]
            [geex.common :as c]
            [geex.core.seed :as seed]
            [clojure.test :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This file is for tests that previously did not work
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is the one that does not work.
(java/typed-defn find-index
                 [(c/array-type Integer/TYPE) data
                  Integer/TYPE value]
                 (let [len (c/count data)]
                   (core/Loop
                    [index 0]
                    (core/If
                     (c/= index len)
                     -1
                     (let [x (c/aget data index)]
                       (core/If (c/= x value)
                                (c/+ 1000 index)
                                (core/Recur (c/inc index))))))))


(deftest nested-if-problem
  (doseq [[number-to-find expected] (map vector
                                         [0 1 2 3 4 5 6]
                                         [-1 -1 1000 1001 1002 -1 -1])]
    (is (= expected (find-index (int-array [2 3 4])
                                number-to-find)))))


(java/typed-defn set-element []
                 (let [dst (c/make-array Float/TYPE 1)]
                   (c/aset dst 0 (float 3.0))))

(java/typed-defn nth-char [Long/TYPE n]
                 (c/nth "mjao" n))

(deftest nth-char-test
  (is (= \j (nth-char 1))))

(def n 30)

(java/typed-defn setter-test []
                 ;;(core/set-flag! :disp :format)
                 (let [dst (java/new ArrayList)]
                   (c/doseq [i (c/range  n)]
                     (let [i (java/cast-to-int i)]
                       (java/call-method "add" dst i nil)))
                   dst))

(deftest calling-void-test
  (let [out (setter-test)]
    (is (= 30 (count out)))
    (is (every? nil? out))))

(deftest extends-test
  (let [c (java/make-class
           {:name "MyExtension"
            :extends geex.test.Add1ToSomething})]
    (is (= 119.0 (.add1 (.newInstance c))))))

(java/typed-defn
 disp-it [String s]
 (let [out (java/system-out)]
   (out 'println s)))

(java/typed-defn type-checks []
                 (assert (= Integer/TYPE
                            (seed/datatype
                             (c/inc
                              (c/wrap (int 0))))))
                 (c/doseq [i (c/range (int 3))]
                   (assert (= Integer/TYPE
                              (seed/datatype i))))
                 (let [x (c/* (c/wrap (int 3))
                              (int 4))]
                   (assert (= Integer/TYPE
                              (seed/datatype x)))))

(java/typed-defn decorate [clojure.lang.IPersistentMap m]
                 (m 'assoc :kattskit 3))

(deftest better-java-interop-test
  (is (= (decorate {})
         {:kattskit 3})))

(java/typed-defn boxed-long-str [Long x]
                 (c/to-string x))

(java/typed-defn primitive-long-str [Long/TYPE x]
                 (c/to-string x))


(java/typed-defn cat-longs [Long/TYPE a
                            Long/TYPE b]
                 (c/str a b))

(java/typed-defn cat-longs2 [Long/TYPE b]
                 (c/str 3 b))

(deftest various-string-tests
  (is (= "3" (boxed-long-str 3)))
  (is (= "3" (primitive-long-str 3)))
  (is (= "3" (c/to-string 3)))
  (is (= "34" (cat-longs 3 4)))
  (is (= "34" (cat-longs2 4)))
  (is (= "34" (c/str 3 4))))



(defn fib []
  (c/iterate
   (fn [[a b]]
     [b (c/+ a b)])
   [0 1]))

(java/typed-defn iter-fn []
                 (let [s (fib)]
                   (-> s
                       c/rest
                       c/rest
                       c/rest
                       c/rest
                       c/rest
                       c/first)))

(java/typed-defn iter-fn-empty? []
                 (c/empty? (fib)))

(java/typed-defn
 smallest-fib100 []
 (core/set-flag! :disp :format)
 (core/Loop [s (c/map first (fib))]
            (let [x (c/first s)]
              (core/If (c/< 100 x)
                       x
                       (core/Recur (c/rest s))))))

(deftest iterate-test
  (is (= (iter-fn) [5 8]))
  (is (not (iter-fn-empty?)))
  (is (= 144 (smallest-fib100))))


(java/typed-defn drop-while-too-small
                 [(c/array-type Double/TYPE) arr]
                 (c/first
                  (c/drop-while
                   #(c/< % 10)
                   arr)))

(java/typed-defn drop-while-tr [(c/array-type Double/TYPE) arr]
                 (c/transduce
                  (c/drop-while #(c/< % 10))
                  c/+
                  0.0
                  arr))

(deftest drop-while-test
  (is (= 13.0 (drop-while-too-small
                  (double-array [1 2 4 13 545]))))
  (is (= 45.0 (drop-while-tr (double-array [0 11 34])))))
