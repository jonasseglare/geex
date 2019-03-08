(ns sample-project.core-test
  (:require [clojure.test :refer :all]
            [sample-project.core :refer :all]))

(deftest a-test
  (let [inst (.newInstance kattskit)]
    (is (= (.wrap inst "Mjao")
           {:x "Mjao"}))))
