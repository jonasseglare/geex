(ns geex.core2-test
  (:require [geex.core2 :refer :all]
            [clojure.test :refer :all]))

(deftest state-test
  (is (state? empty-state))
  (is (= 1 (:counter (step-counter empty-state))))
  (is (= 1 (:counter (with-state empty-state
                       (fn []
                         (swap-state! step-counter))))))
  (is (map? (with-state empty-state
              (fn []
                (get-injection-deps))))))
