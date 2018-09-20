(ns geex.core2-test
  (:require [geex.core2 :refer :all]
            [clojure.test :refer :all]
            [geex.core.seed :as seed]))

(deftest state-test
  (is (state? empty-state))
  (is (= 1 (:counter (step-counter empty-state))))
  (is (= 1 (:counter (with-state empty-state
                       (fn []
                         (swap-state! step-counter))))))
  (is (map? (with-state empty-state
              (fn []
                (get-injection-deps)))))
  (is (= {[:kattskit] 119}
         (:injection-deps
          (with-state empty-state
            (fn []
              (set-injection-deps! {[:kattskit] 119}))))))
  (let [state (with-state empty-state
                (fn []
                  (wrap 9.0)))]
    (is (= 1 (count (seed-map state)))))
  #_(let [r (make-seed empty-state (seed/typed-seed :mjao))]
    (println "r=" r))
  #_(is (= (with-state empty-state
           (fn []
             3)))))
