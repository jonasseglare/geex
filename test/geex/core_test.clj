(ns geex.core-test
  (:require [geex.core :refer :all]
            [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.core.xplatform :as xp]
            [geex.core.seed :as seed]
            [geex.core.utils :as cutils]
            [geex.core.datatypes :as datatypes]))

(deftest accessor-test
  (eval-body
   empty-state
   (is (defs/seed? (to-seed 9)))
   (is (defs/seed? (to-seed [:a :b :c])))
   (is (defs/seed? (-> 9 to-seed to-seed to-seed)))
   (is (= (type-signature [9 9 (to-seed 10)])
          [9 9 (defs/datatype {} (datatypes/unboxed-class-of 9))]))
   (is (= (flatten-expr {:a 9 :b (to-seed 10)})
          [(to-seed 10)]))
   (is (= (type-signature {:a (to-seed 10.0)})
          {:a #:geex.core.defs{:type java.lang.Double/TYPE}}))
   (is (not (= (to-seed 10.0) (to-seed 10))))
   (is (= (to-seed 10.0) (to-seed 10.0)))

   (type-signature [:a {:b 'k}]))
  
  #_(is (= (seed/access-seed-coll {:a 9 :b 10})
         [:a 9 :b 10]))
  #_(is (= (seed/access-seed-coll (to-seed {:a 4 :b 5}))
         [:a 4 :b 5])))

(deftest max-seed-mode
  (is (= :pure (seed/max-mode :pure))))


(deftest state-test
  (is (state? empty-state))
  (is (= 1 (:counter (step-counter empty-state))))
  (is (= 1 (:counter (with-state empty-state
                       (fn []
                         (swap-the-state! step-counter))))))
  
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
  (is (thrown?
       Exception
       (eval-body-fn empty-state
                     (fn []
                       (begin-scope!)
                       9)))))

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
  (is (= {:a 3, :b 2}
         (let [s (atom {}) ]
           (demo-embed
            (begin-scope!)
            (flush! (end-scope! nil))
            (begin-scope!)
            (demo-step-counter 's :a)
            (demo-step-counter 's :a)
            (demo-step-counter 's :b)

            ;; Note: As a rule of thumb,
            ;; Always flush before entering a scope and before
            ;; leaving a scope!
            (flush! nil)
            
            (begin-scope!)
            (demo-step-counter 's :a)
              (end-scope! (flush! nil))
            (demo-step-counter 's :b)
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

(deftest local-vars-test
  (is (= [0 1]
         (demo-embed
          [(declare-local-var!)
           (declare-local-var!)])))
  (is (nil? (demo-embed
             (let [id (declare-local-var!)]
               (set-local-var! id 119.0)))))
  (is (thrown? Exception
               (generate-and-eval
                (let [id (declare-local-var!)]
                  (set-local-var! id 119.0)
                  (set-local-var! id [])))))
  (is (nil? (generate-and-eval
             (let [id (declare-local-var!)]
               (set-local-var! id 119.0)
               (set-local-var! id 120.0)))))
  (is (= 119.0 (demo-embed
                (let [id (declare-local-var!)]
                  (set-local-var! id 119.0)
                  (get-local-var! id)))))
  (is (= 120.0
         (demo-embed
          (let [id (declare-local-var!)]
            (set-local-var! id 119.0)
            (set-local-var! id 120.0)
            (get-local-var! id)))))
  (is (= 119.0 (demo-embed
                (set-local-struct! :kattskit {:a (wrap 9)
                                              :b (wrap 10)})
                119.0)))
  (is (= (demo-embed
             (set-local-struct! :kattskit {:a (wrap 9)})
             (get-local-struct! :kattskit))
         {:a 9}))
  (is (= (demo-embed
          (set-local-struct! :kattskit {:a (wrap 9)
                                        :b (wrap 10)})
             (get-local-struct! :kattskit))
         {:a 9 :b 10}))
  (is (= (demo-embed
          (set-local-struct! :kattskit {:a (wrap 11)
                                        :b (wrap 20)})
          (set-local-struct! :kattskit {:a (wrap 9)
                                        :b (wrap 10)})
          (get-local-struct! :kattskit))
         {:a 9 :b 10}))
  (is (= (demo-embed
          (set-local-struct! :kattskit {:a (wrap 11)
                                        :b (wrap 20)})
          (set-local-struct! :kattskit (get-local-struct! :kattskit))
          (get-local-struct! :kattskit))
         {:a 11 :b 20}))
  (is (= (demo-embed 
             (set-local-struct! :kattskit [(wrap 9) (wrap 10)])
             (set-local-struct!
              :kattskit (reverse (get-local-struct! :kattskit)))
             (get-local-struct! :kattskit))
         [10 9]))
  (is (thrown? Exception
               (generate-and-eval
                (set-local-struct! :kattskit [(wrap 9) (wrap 10)])
                (set-local-struct! :kattskit [(wrap 9) 10])))))


(deftest if-test
  (is (= 3.0 (demo-embed (If true (wrap 3.0) (wrap 4.0)))))
  (is (= 4.0 (demo-embed (If false (wrap 3.0) (wrap 4.0)))))
  (is (= {:a 1 :b 1 :d 1}
         (let [s (atom {})]
              (demo-embed
               (demo-step-counter 's :a)
               (If true
                   (do (demo-step-counter 's :b))
                   (do (demo-step-counter 's :c)))
               (demo-step-counter 's :d)))))
  (is (= {:a 1 :c 1 :d 1}
         (let [s (atom {})]
              (demo-embed
               (demo-step-counter 's :a)
               (If false
                   (do (demo-step-counter 's :b))
                   (do (demo-step-counter 's :c)))
               (demo-step-counter 's :d)))))
  (is (= {:a 1 :b 1 :d 1 :e 1}
         (let [s (atom {})]
              (demo-embed
               (demo-step-counter 's :a)
               (If true
                   (do (demo-step-counter 's :b)
                       (if true
                         (demo-step-counter 's :e)
                         (demo-step-counter 's :f)))
                   (do (demo-step-counter 's :c)
                       (demo-step-counter 's :g)))
               (demo-step-counter 's :d)))))
  (is (= {:a 1 :b 1 :d 1 :f 1}
         (let [s (atom {})]
              (demo-embed
               (demo-step-counter 's :a)
               (If true
                   (do (demo-step-counter 's :b)
                       (if false
                         (demo-step-counter 's :e)
                         (demo-step-counter 's :f)))
                   (do (demo-step-counter 's :c)
                       (demo-step-counter 's :g)))
               (demo-step-counter 's :d)))))
  (is (= {:a 1 :b 1 :d 1 :f 1 :k 1}
         (let [s (atom {})]
              (demo-embed
               (demo-step-counter 's :a)
               (If true
                   (do (demo-step-counter 's :b)
                       (if false
                         (demo-step-counter 's :e)
                         (demo-step-counter 's :f))
                       (demo-step-counter 's :k))
                   (do (demo-step-counter 's :c)
                       (demo-step-counter 's :g)))
               (demo-step-counter 's :d)))))
  (is (= {:a 1 :c 1 :g 1 :d 1}
         (let [s (atom {})]
              (demo-embed
               (demo-step-counter 's :a)
               (If false
                   (do (demo-step-counter 's :b)
                       (if false
                         (demo-step-counter 's :e)
                         (demo-step-counter 's :f))
                       (demo-step-counter 's :k))
                   (do (demo-step-counter 's :c)
                       (demo-step-counter 's :g)))
               (demo-step-counter 's :d)))))
  (is (= 0 (demo-embed
            (loop0 (seed/datatype (wrap 9) nil)
                   identity
                   #(demo-call-fn :pure 'not= [0 %])
                   #(demo-call-fn :pure 'dec [%]))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Tests copied from previous core module
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-mini-if [mjao]
  (demo-embed
   (If 'mjao
       (to-seed 3)
       (to-seed 4))))

(deftest test-nothing
  (is (= nil (demo-embed ::defs/nothing))))
