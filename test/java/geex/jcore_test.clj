(ns geex.jcore-test
  (:import [geex SeedParameters Mode])
  (:require [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.jcore :refer :all :as jcore]
            [geex.core.seed :as seed]
            [geex.core.datatypes :as datatypes]
            [bluebell.utils.wip.check :refer [checked-defn]]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]))

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
            (fn [] global-state))]
    (is (state? s)))
  (is (nil? global-state))
  (is (thrown? Exception (#'jcore/get-state)))
  (is (state? (with-state-fn clojure-state-settings
                #(#'jcore/get-state))))
  (let [s (with-state clojure-state-settings
            (to-seed ::defs/nothing))]
    (is (state? s))
    (is (seed? (.getOutput s))))
  (is (seed?
       (.getOutput (with-state clojure-state-settings
                     (to-seed Double/TYPE)))))
  (let [s (with-state clojure-state-settings
            (wrap 1)
            (wrap 2)
            (wrap 3))]
    (is (= 3 (.getSeedCount s))))
  (let [s (eval-body clojure-state-settings
                     (wrap 1) (wrap 2) (wrap 3))]
    (is (state? s)))
  (is (= 1 (demo-embed 1)))
  (is (= 119 (demo-embed 119)))
  (is (= [1 2] (demo-embed [1 2])))
  (is (= (demo-embed (let [x [1 2]] [x x]))
         [[1 2] [1 2]]))
  (is (= (demo-embed :a)
         :a))
  (is (= (demo-embed "Kattskit")
         "Kattskit"))
  (is (= (demo-embed (let [x (wrap [1 2])] [x x]))
         [[1 2] [1 2]]))
  (is (nil? (demo-embed nil)))
  (is (= (demo-embed [:a :b {:c 3}])
         [:a :b {:c 3}]))
  (is (nil? (demo-embed (begin-scope!) (end-scope! nil))))
  (is (nil? (demo-embed (begin-scope! {:depending-scope? true})
                        (end-scope! nil)))))

(defn demo-compile-call-fn [state seed cb]
  (let [compiled-deps (seed/access-compiled-indexed-deps seed)]
    (cb (defs/compilation-result
          state
          `(~(.getData seed) ~@compiled-deps)))))

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
            [k j k]))
         [6 12 6])))

(deftest side-effect-test
  (is (=  (let [s (atom {})]
            (demo-embed (demo-step-counter 's :kattskit)))
          {:kattskit 1}))
  (is (= [{:katt 3} {:katt 2} {:katt 1}]
         (let [s (atom {})]
           (demo-embed 
            (vec (reverse
                  [
                   (demo-step-counter 's :katt)
                   (demo-step-counter 's :katt)
                   (demo-step-counter 's :katt)])))))))

(deftest seq-coll-test
  (is (= '(1 2 3) (demo-embed '(1 2 3)))))


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

(deftest local-vars-test
  (is (= [0 1]
         (demo-embed [(declare-local-var!)
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
  (is (= 119.0  (demo-embed
                 (let [id (declare-local-var!)]
                   (set-local-var! id 119.0)
                   (get-local-var! id)))))
  (is (= 120.0
         (demo-embed
          (let [id (declare-local-var!)]
            (set-local-var! id 119.0)
            (set-local-var! id 120.0)
            (get-local-var! id))))))

(deftest local-struct-test
  (is (= 119.0 (demo-embed
                (set-local-struct! :kattskit {:a (wrap 9)
                                              :b (wrap 10)})
                119.0)))
  (is (= (demo-embed
          (set-local-struct! :kattskit {:a (wrap 9)})
          (get-local-struct! :kattskit))
         {:a 9}))
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
