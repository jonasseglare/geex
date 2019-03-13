(ns sample-project.core-test
  (:require [clojure.test :refer :all]
            [sample-project.core :refer :all]))

(deftest some-test
  (let [s (make-scaler 3.0)]
    (is (= 1009.0 (.scale s 3.0)))))

(deftest a-test
  (let [inst (.newInstance kattskit)]
    (is (= (.wrap inst "Mjao")
           {:x "Mjao"}))))
