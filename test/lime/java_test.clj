(ns lime.java-test
  (:require [clojure.test :refer :all]
            [lime.java :refer :all]
            [lime.core.seed :as seed]
            [bluebell.utils.debug :as debug]))

(def c (time (janino-cook-and-load-object
              "Kattskit"
              "public class Kattskit {public double sq(double x) {return x*x;}}")))


(deftest cooked-c-test
  (is (= 81.0 (.sq c 9))))

(deftest arglist-parse-test
  (is (= (parse-typed-defn-args '(kattskit [:cobra b :mjao d] (+ b d)))
         '{:name kattskit, :arglist [{:type :cobra, :name b}
                                     {:type :mjao, :name d}],
           :body [(+ b d)]})))


(typed-defn return-119-2 [(seed/typed-seed java.lang.Double) x] 119.0)

(deftest return-119-test
  (is (= 119.0 (return-119-2 30))))

#_(typed-defn second-arg-fun [(seed/typed-seed java.lang.Double) x
                            (seed/typed-seed java.lang.Long) y
                              (seed/typed-seed java.lang.Float) z] y)
(typed-defn second-arg-fun2 [(seed/typed-seed java.lang.Double) x
                             (seed/typed-seed java.lang.Long) y
                             (seed/typed-seed java.lang.Float) z] y)
(deftest second-arg-test
  (is (= 119 (second-arg-fun2 3 119 4))))
