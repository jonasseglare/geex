(ns geex.core2-test
  (:require [geex.core2 :refer :all]
            [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.core.xplatform :as xp]
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
  (let [state (eval-body empty-state
                (fn []
                  9.0))]
    (is (= 1 (count (seed-map state)))))
  (is (= :clojure
         (:platform
          (with-state empty-state
            (fn []
              (defs/get-platform))))))
  (let [state (eval-body empty-state
                (fn []
                  (wrap 9.0)))]
    (is (= 1 (count (seed-map state)))))
  (let [state (eval-body empty-state
                (fn []
                  (demo-add 1.0 3.0)))])
  )

(deftest codegen-test
  (is (= 9 (generate-code
            (eval-body empty-state
                       (fn []
                         9)))))
  (is (= 9 (generate-code
            (eval-body empty-state
                       (fn []
                         (wrap 9))))))
  (is (= 3 (generate-code
            (eval-body empty-state
                       (fn []
                         (wrap 1)
                         (wrap 2)
                         (wrap 3))))))
  (is (= 3 (generate-code
            (eval-body empty-state
                       (fn []
                         (wrap 1)
                         (wrap 2)
                         3))))))

(deftest max-mode-test-begin-scope
  (let [state (eval-body empty-state
                       (fn []
                         (begin-scope!)
                         9))]
    (is (= (select-keys state [:mode-stack :max-mode])
           {:mode-stack [:pure], :max-mode :pure}))
    (is (thrown? Exception (= 9 (generate-code state))))))

(deftest small-scope-test
  (let [state (eval-body empty-state
                       (fn []
                         (begin-scope!)
                         (end-scope! 9)))]
    (is (= 3 (count (:seed-map state))))
    (is (= 9 (generate-code state))))
  (let [state (eval-body empty-state
                       (fn []
                         (begin-scope!)
                         (wrap 5)
                         (wrap 6)
                         (end-scope! 9)
                         11))]
    (is (= 6 (count (:seed-map state))))
    (is (= 11 (generate-code state)))))

(deftest coll-test
  (let [state (eval-body
               empty-state
               (fn []
                 (wrap [1 2 3])))]
    (is (= 4 (-> state
                 :seed-map
                 count)))
    (is (= 3 (-> state
                 :seed-map
                 (get 4)
                 seed/access-deps
                 count)))
    (is (= [1 2 3]
           (generate-code
            (eval-body empty-state
                       (fn [] (wrap [1 2 3]))))))
    (is (= (generate-code
             (eval-body empty-state
                        (fn [] (wrap [1 2 {:a 3}]))))
           [1 2 {:a 3}]))
    
    (is (= [[1 2] [1 2]]
           (generate-code
            (eval-body empty-state
                       (fn [] (wrap [[1 2] [1 2]]))))))))