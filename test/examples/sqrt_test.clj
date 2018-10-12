(ns examples.sqrt-test
  (:require [geex.lib :as lib]
            [geex.java :as java]
            [geex.core :as core]
            [clojure.test :refer :all]))

(defn sqrt-iteration [k x]
  (lib/- x (lib// (lib/- (lib/* x x) k)
                  (lib/* 2.0 x))))

(java/typed-defn unrolled-sqrt [Double/TYPE x]
                 ;(core/set-flag! :disp-final-source)
                 ;(core/set-flag! :disp-time)
                 (->> x
                      (iterate (partial sqrt-iteration x))
                      (take 10)
                      last))

(deftest sqrt-test
  (is (< (Math/abs (- (unrolled-sqrt 2)
                      (Math/sqrt 2)))
         1.0e-6)))
