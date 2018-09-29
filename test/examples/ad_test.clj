(ns examples.ad-test
  (:require [geex.lib :as lib]
            [geex.core :as core]
            [geex.java :as java]
            [clojure.test :refer :all]))

(defn ad [x dx]
  {:x x :dx dx})

(defn variable [x]
  (ad x 1.0))

(defn constant [x]
  (ad x 0.0))

(defn add [a b]
  {:x (lib/+  (:x a) (:x b))
   :dx (lib/+ (:dx a) (:dx b))})

(defn sub [a b]
  {:x (lib/- (:x a) (:x b))
   :dx (lib/- (:dx a) (:dx b))})

(deftest test-the-ops
  (is (= {:x 7 :dx 2.0}
         (java/eval (add (variable 3) (variable 4)))))
  (is (= {:x -1 :dx 0.0}
         (java/eval (sub (variable 3) (variable 4))))))
