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
  
  (let [state (with-state empty-state
                (fn []
                  (wrap 9.0)))]
    (is (= 1 (count (seed-map state)))))
  (let [state (eval-body-fn empty-state
                (fn []
                  9.0))]
    )
  (is (= :clojure
         (:platform
          (with-state empty-state
            (fn []
              (defs/get-platform))))))
  (let [state (eval-body-fn empty-state
                (fn []
                  (wrap 9.0)))])
  (let [state (eval-body-fn empty-state
                (fn []
                  (demo-add 1.0 3.0)))])
  )

(deftest codegen-test
  (is (= 9 (generate-code
            (eval-body-fn empty-state
                       (fn []
                         9)))))
  (is (= 9 (generate-code
            (eval-body-fn empty-state
                       (fn []
                         (wrap 9))))))
  (is (= 3 (generate-code
            (eval-body-fn empty-state
                       (fn []
                         (wrap 1)
                         (wrap 2)
                         (wrap 3))))))
  (is (= 3 (generate-code
            (eval-body-fn empty-state
                       (fn []
                         (wrap 1)
                         (wrap 2)
                         3))))))

(deftest max-mode-test-begin-scope
  (let [state (eval-body-fn empty-state
                       (fn []
                         (begin-scope!)
                         9))]
    (is (= (select-keys state [:mode-stack :max-mode])
           {:mode-stack [:pure], :max-mode :pure}))
    (is (thrown? Exception (= 9 (generate-code state))))))

(deftest small-scope-test
  (let [state (eval-body-fn empty-state
                       (fn []
                         (begin-scope!)
                         (end-scope! 9)))]
    (is (= 9 (generate-code state))))
  (let [state (eval-body-fn empty-state
                       (fn []
                         (begin-scope!)
                         (wrap 5)
                         (wrap 6)
                         (end-scope! 9)
                         11))]
    (is (= 11 (generate-code state)))))

(deftest coll-test
  (let [state (eval-body-fn
               empty-state
               (fn []
                 (wrap [1 2 3])))]

    (is (= [1 2 3]
           (generate-code
            (eval-body-fn empty-state
                       (fn [] (wrap [1 2 3]))))))
    (is (= (generate-code
             (eval-body-fn empty-state
                        (fn [] (wrap [1 2 {:a 3}]))))
           [1 2 {:a 3}]))
    
    (is (= [[1 2] [1 2]]
           (eval
            (generate-code
             (eval-body-fn empty-state
                        (fn [] (flush!
                                (wrap [[1 2] [1 2]]))))))))
    (is (= (generate-code
            (eval-body-fn empty-state
                       (fn [] 
                         (begin-scope!)
                         (wrap [1 2])
                         (end-scope! (wrap [3 4]))
                         (wrap [1 2]))))
           [1 2]))))

(deftest fn-test
  (is (= 6 (eval (generate-code
                  (eval-body
                   empty-state
                   (demo-pure-add 1 2 3))))))
  (is (= [6 12 6]
         (eval (generate-code 
                (eval-body empty-state
                           (let [k (demo-pure-add 1 2 3)
                                 j (demo-pure-add k k)]
                             [k j k]))))))
  (is (= [6 12 6 12]
         (eval (generate-code 
                (eval-body empty-state
                           (let [k (demo-pure-add 1 2 3)
                                 j (demo-pure-add k k)]
                             [k j k j])))))))

(deftest embed-test
  (is (= 6 (demo-embed (demo-pure-add 1 2 3)))))

(deftest side-effect-test
  (is (= {:kattskit 1}
         (let [s (atom {})]
           (demo-embed (demo-step-counter 's :kattskit)))))
  (is (=
       [{:katt 3} {:katt 2} {:katt 1}]
       (let [s (atom {})]
         (demo-embed 
          (vec (reverse
                [
                 (demo-step-counter 's :katt)
                 (demo-step-counter 's :katt)
                 (demo-step-counter 's :katt)])))))))

(deftest seq-coll-test
  (is (= '(1 2 3) (demo-embed '(1 2 3)))))

#_(generate-code
             (eval-body empty-state
                       (demo-step-counter 's :katt)))

(deftest side-effects-in-scope-test
  (is (= {:a 2 :b 1}
         (let [s (atom {}) ]
                  (demo-embed
                   (begin-scope!)
                   (demo-step-counter 's :a)
                   (demo-step-counter 's :a)
                   (flush! (end-scope! nil))
                   (demo-step-counter 's :b)))))
  (is (= {:a 2 :b 1}
         (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (demo-step-counter 's :a)
            (demo-step-counter 's :a)
            (end-scope! (flush! nil))
            (demo-step-counter 's :b)))))
  (is (= {:a 2 :b 1}
         (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (demo-step-counter 's :a)
            (demo-step-counter 's :a)
            (flush! nil)
            (end-scope! nil)
            (demo-step-counter 's :b)))))
  (is (= {:b 1}
         (let [s (atom {}) ]
                  (demo-embed
                   (begin-scope!)
                   (demo-step-counter 's :b)
                   (end-scope! (flush! nil)))
                  (deref s))))
  (is (= {:b 1}
         (let [s (atom {})]
           (demo-embed
            (begin-scope!)
            (begin-scope!)
            (end-scope! nil)
            (demo-step-counter 's :b)
            (end-scope! (flush! nil)))
           (deref s))))
  (is (= {:b 1}
         (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (begin-scope!)
            (end-scope! nil)
            (begin-scope!)
            (demo-step-counter 's :b)
            (end-scope! nil)
            (end-scope! (flush! nil)))
           (deref s))))
  (is (= (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (begin-scope!)
            (end-scope! nil)
            (begin-scope!)
            (begin-scope!)
            (demo-step-counter 's :b)
            (end-scope! nil)
            (begin-scope!)
            (end-scope! nil)
            (end-scope! nil)
            (end-scope! (flush! nil)))
           (deref s))))
  (is (= {:b 1}
         (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (begin-scope!)
            (end-scope! nil)
            (begin-scope!)
            (begin-scope!)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (demo-step-counter 's :b)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (end-scope! nil)
            (begin-scope!)
            (end-scope! nil)
            (end-scope! nil)
            (end-scope! (flush! nil)))
           (deref s))))
  (is (= {:a 2, :b 2}
         (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (begin-scope!)
            (end-scope! nil)
            (begin-scope!)
            (demo-step-counter 's :a)
            (begin-scope!)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (demo-step-counter 's :a)
            (begin-scope!)(end-scope! nil)
            (demo-step-counter 's :b)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (begin-scope!)(end-scope! nil)
            (end-scope! nil)
            (begin-scope!)
            (end-scope! nil)
            (demo-step-counter 's :b)
            (end-scope! nil)
            (end-scope! (flush! nil)))
           (deref s))))
  (is (= (let [s (atom {}) ]
              (demo-embed
               (reverse
               [[(begin-scope!)
                  (end-scope! (demo-step-counter 's :a))]
                 [(begin-scope!)
                  (end-scope! (demo-step-counter 's :a))]])))
         '([nil {:a 2}] [nil {:a 1}]))))

; Problematic
#_(let [s (atom {}) ]
                  (demo-embed
                   (set-flag! :disp-trace :disp-final-state)
                   (begin-scope!)
                   (begin-scope!)
                   (end-scope! nil)
                   (demo-step-counter 's :b)
                   (end-scope! (flush! nil)))
                  (deref s))


#_(let [s (atom {}) ]
              (macroexpand
               '(demo-embed
                   (set-flag! :disp-generated-output
                              :disp-final-state)
                   (begin-scope!)
                   (begin-scope!)
                   (end-scope! nil)             ;; <<FÅR RESULATET
                   (demo-step-counter 's :b)    ;; ReSULTATED
                   (end-scope! (flush! nil))))) ;; <<BORDE FÅ RESULTATE
