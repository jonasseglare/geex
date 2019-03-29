(ns examples.sqrt-test
  (:require [geex.common :as c]
            [geex.java :as java]
            [geex.core :as gx]
            [clojure.test :refer :all]))

(defn sqrt-iteration [k x]
  (c/- x (c// (c/- (c/* x x) k)
              (c/* 2.0 x))))

(java/typed-defn unrolled-sqrt [Double/TYPE x]

                 ;; Display time and generated code:
                 ;;(gx/set-flag! :disp :disp-time :format)
                 
                 (->> x
                      (iterate (partial sqrt-iteration x))
                      (take 10)
                      last))


(unrolled-sqrt 2.0)
;; => 1.4142135623730951

(deftest sqrt-test
  (is (< (Math/abs (- (unrolled-sqrt 2)
                      (Math/sqrt 2)))
         1.0e-6)))
