(ns geex.core-test
  (:import [geex SeedParameters Mode])
  (:require [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.core :refer :all :as jcore]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as datatypes]
            [bluebell.utils.wip.check :refer [checked-defn]]
            [bluebell.utils.wip.java :as jutils :refer [set-field]])
  (:refer-clojure :exclude [cast]))

(deftest op-tests
  (eval-body
   clojure-state-settings
   (is (= (type-signature [9 9 (to-seed 10)])
          [9 9 (typed-seed Long/TYPE)]))

   (is (= (type-signature {:a (to-seed 10.0)})
          {:a (typed-seed Double/TYPE)}))
   (is (not (= (to-seed 10.0) (to-seed 10))))

   (type-signature [:a {:b 'k}])
   ))


(deftest with-state-test
  (let [s (with-state-fn clojure-state-settings
            (fn [] defs/global-state))]
    (is (state? s)))
  (is (nil? defs/global-state))
  (is (thrown? Exception (#'jcore/get-state)))
  (is (state? (with-state-fn clojure-state-settings
                #(#'jcore/get-state))))
  (let [s (with-state clojure-state-settings
            (to-seed ::defs/nothing))]
    (is (state? s)))
  (let [s (with-state clojure-state-settings
            (wrap 1)
            (wrap 2)
            (wrap 3))]
    (is (= 3 (.getSeedCount s))))
  (let [s (eval-body clojure-state-settings
                     (wrap 1) (wrap 2) (wrap 3))]
    (is (state? s)))

  (is (= 1 (demo-embed (wrap 1))))
  (is (= 119 (demo-embed (wrap 119))))
  (is (= [1 2] (demo-embed (wrap [1 2]))))
  (is (= (demo-embed (let [x [1 2]] (wrap [x x])))
         [[1 2] [1 2]]))
  (is (= (demo-embed (wrap :a))
         :a))
  (is (= (demo-embed (wrap "Kattskit"))
         "Kattskit"))
  (is (= [[1 2] [1 2]]
         (demo-embed (let [x (wrap [1 2])] (wrap [x x])))))
  (is (nil? (demo-embed (wrap nil))))
  (is (= (demo-embed (wrap [:a :b {:c 3}]))
         [:a :b {:c 3}]))
  (is (nil? (demo-embed (open-scope!) (close-scope!))))
  (is (= 9 (demo-embed (open-scope!)
                       (wrap 9)
                       (close-scope!))))
  (is (nil? (demo-embed (open-scope!)
                        9
                        (close-scope!)))))

(defn demo-compile-call-fn [state seed]
  (let [compiled-deps (seed/access-compiled-indexed-deps seed)]
    `(~(.getData seed) ~@compiled-deps)))

(checked-defn demo-call-fn [:when check-debug
                            
                            _ mode
                            symbol? f
                            sequential? args

                            :post ::defs/seed]
  (make-seed!
   (doto (SeedParameters.)
     (set-field data f)
     (set-field description (str "call " f))
     (set-field mode mode)
     (set-field rawDeps (seed/access-indexed-map {} args))
     (set-field type nil)
     (set-field compiler demo-compile-call-fn))))

(defmacro demo-make-fn [mode f]
  `(fn [& args#]
     (demo-call-fn ~mode (quote ~f) args#)))

(defn demo-sub-step-counter [dst counter-key]
  (swap! dst #(update % counter-key (fn [x] (inc (or x 0))))))

(def demo-pure-add (demo-make-fn Mode/Pure +))

(def demo-step-counter (demo-make-fn
                        Mode/SideEffectful
                        demo-sub-step-counter))

(deftest pure-add-test
  (is (= 6 (demo-embed (demo-pure-add 1 2 3))))
  (is (= (demo-embed
          (let [k (demo-pure-add 1 2 3)
                j (demo-pure-add k k)]
            (wrap [k j k])))
         [6 12 6])))

(deftest side-effect-test
  (is (=  (let [s (atom {})]
            #_(demo-sub-step-counter s :kattskit)
            (demo-embed (demo-step-counter 's :kattskit)))
          {:kattskit 1}))
  (is (= [{:katt 3} {:katt 2} {:katt 1}]
         (let [s (atom {})]
           (demo-embed 
            (wrap (vec (reverse
                        [(demo-step-counter 's :katt)
                         (demo-step-counter 's :katt)
                         (demo-step-counter 's :katt)]))))))))

#_(deftest seq-coll-test
  (is (= '(1 2 3) (demo-embed '(1 2 3)))))


#_(deftest side-effects-in-scope-test
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
            #_(flush! nil)
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
  (is (= {:b 1}
         (let [s (atom {}) ]
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
         '([::defs/nothing {:a 2}]
           [::defs/nothing {:a 1}]))))

#_(deftest local-vars-test
  (is (= [0 1]
         (demo-embed
          (with-local-var-section
            [(declare-local-var!)
             (declare-local-var!)]))))
  (is (nil? (demo-embed
             (with-local-var-section
               (let [id (declare-local-var!)]
                 (set-local-var! id 119.0))))))
  (is (thrown? Exception
               (generate-and-eval
                (with-local-var-section
                 (let [id (declare-local-var!)]
                   (set-local-var! id 119.0)
                   (set-local-var! id []))))))
  (is (nil? (generate-and-eval
             (with-local-var-section
               (let [id (declare-local-var!)]
                 (set-local-var! id 119.0)
                 (set-local-var! id 120.0))))))
  (is (= 119.0  (demo-embed
                 (with-local-var-section
                   (let [id (declare-local-var!)]
                     (set-local-var! id 119.0)
                     (get-local-var! id))))))
  (is (= 120.0
         (demo-embed
          (with-local-var-section
            (let [id (declare-local-var!)]
              (set-local-var! id 119.0)
              (set-local-var! id 120.0)
              (get-local-var! id)))))))

#_(deftest local-struct-test
  (is (= 119.0 (demo-embed
                (with-local-var-section
                  (set-local-struct! :kattskit {:a (wrap 9)
                                                :b (wrap 10)})
                  119.0))))
  (is (= (demo-embed
          (with-local-var-section
            (set-local-struct! :kattskit {:a (wrap 9)})
            (get-local-struct! :kattskit)))
         {:a 9}))
  (is (= (demo-embed
          (with-local-var-section
            (set-local-struct! :kattskit {:a (wrap 11)
                                          :b (wrap 20)})
            (set-local-struct! :kattskit {:a (wrap 9)
                                          :b (wrap 10)})
            (get-local-struct! :kattskit)))
         {:a 9 :b 10}))
  (is (= (demo-embed
          (with-local-var-section
            (set-local-struct! :kattskit {:a (wrap 11)
                                          :b (wrap 20)})
            (set-local-struct! :kattskit (get-local-struct! :kattskit))
            (get-local-struct! :kattskit)))
         {:a 11 :b 20}))
  (is (= (demo-embed
          (with-local-var-section
            (set-local-struct! :kattskit [(wrap 9) (wrap 10)])
            (set-local-struct!
             :kattskit (reverse (get-local-struct! :kattskit)))
            (get-local-struct! :kattskit)))
         [10 9]))
  (is (thrown? Exception
               (generate-and-eval
                (with-local-var-section
                  (set-local-struct! :kattskit [(wrap 9) (wrap 10)])
                  (set-local-struct! :kattskit [(wrap 9) 10])))))

  (deftest if-test
    (is (= 3.0 (demo-embed (If true (wrap 3.0) (wrap 4.0)))))
    (is (= 4.0 (demo-embed (If false (wrap 3.0) (wrap 4.0)))))
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
              (demo-step-counter 's :d)))))))

#_(deftest static-if-cond-test
  (is (= 119.0
         (demo-embed
          (If true
              119.0
              (assert false "This code should never get evaluated!")))))
  (is (= 119.0
         (demo-embed
          (If false
              (assert false "This code should never get evaluated!")
              119.0)))))

#_(deftest test-nothing
  (is (= nil (demo-embed ::defs/nothing))))


#_(deftest wrap-recursive-test
  (is (= {:a 119} (wrap-recursive {:a (wrap-quote 119)})))
  (is (= [:a 119] (wrap-recursive [:a (wrap-quote 119)])))
  
  ;; Check that wrapping takes place
  (is (= 9 (demo-embed
            (with-local-var-section
              (If (wrap true)
                  9
                  10))))))


;; (macroexpand '(demo-embed (make-loop [0] (fn [[state]] (demo-pure-add state 1)))))

#_(deftest loop-without-recur
  (is (= 1
         (demo-embed
          (with-local-var-section
            (fn-loop
             [0]
             (fn [[state]]
               (demo-pure-add state 1))))))))

#_(deftest another-mini-loop
  (is (= 2
         (demo-embed
          (with-local-var-section
            (fn-loop [(seed/set-seed-type! (wrap 9) nil)]
                     (fn [[x]]
                       (If (demo-call-fn Mode/Pure 'not= [2 x])
                           (Recur (demo-call-fn Mode/Pure 'dec [x]))
                           x))))))))

(defn wrap-pure-fn [f-sym]
  {:pre [(symbol? f-sym)]}
  (comp (partial demo-call-fn Mode/Pure f-sym) vector))

(def mymod (wrap-pure-fn 'mod))
(def my= (wrap-pure-fn '=))
(def my* (wrap-pure-fn '*))
(def my- (wrap-pure-fn '-))

#_(deftest another-mini-loop-2
  (is (= (* 9 7 5 3 1)
         (demo-embed
          (with-local-var-section
            (fn-loop [(seed/set-seed-type! (wrap 9) nil) ;; counter
                      (seed/set-seed-type! (wrap 1) nil) ;; product
                      ]
                     (fn [[counter product]]
                       (If (my= 0 counter)
                           product
                           ;(Recur (my- counter 1) (my* product counter))
                           (If (my= 0 (mymod counter 2))
                               (Recur (my- counter 1) product)
                               (Recur (my- counter 1) (my* product counter)))))))))))


(deftest modify-state-var-test
  (is (= [119 nil]
         (binding [defs/global-state (make-clojure-state)]
           [(with-modified-state-var "a" (fn [x] 119)
              (get-state-var "a"))
            (get-state-var "a")])))
  (is (= [119 nil]
         (binding [defs/global-state (make-clojure-state)]
           [(with-new-state-var "a" 119
              (get-state-var "a"))
            (get-state-var "a")]))))
