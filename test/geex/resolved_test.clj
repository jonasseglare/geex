(ns geex.resolved-test
  (:import [java.util ArrayList])
  (:require [geex.core :as core]
            [geex.java :as java]
            [geex.common :as c]
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
