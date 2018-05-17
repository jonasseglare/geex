(ns lime.java-test
  (:require [clojure.test :refer :all]
            [lime.java :refer :all]))

(def c (time (janino-cook-and-load
              "Kattskit"
              "public class Kattskit {public double sq(double x) {return x*x;}}")))


(deftest cooked-c-test
  (is (= 81.0 (.sq c 9))))
