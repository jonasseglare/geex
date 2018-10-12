(ns geex.core-test
  (:require [geex.core :refer :all :as core]
            [bluebell.utils.wip.check :refer [checked-defn]]
            [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.core.xplatform :as xp]
            [geex.core.seed :as seed]
            [geex.core.datatypes :as datatypes]))


(defn demo-add-compiler [comp-state expr cb]
  (cb [:add]))

(defn demo-add [a b]
  (let [a (wrap a)
        b (wrap b)]
    (make-seed!
     (-> {}
         ;(seed/access-mode :pure)
         (seed/datatype Double/TYPE)
         (seed/access-deps {:a a
                            :b b})
         (seed/compiler demo-add-compiler)))))

(defn demo-compile-call-fn [comp-state expr cb]
  (let [compiled-deps (seed/access-compiled-indexed-deps expr)]
    (cb (defs/compilation-result
          comp-state
          `(~(:f expr) ~@compiled-deps)))))

(checked-defn demo-call-fn [:when check-debug
                            ::seed/mode mode
                            symbol? f
                            sequential? args

                            :post ::defs/seed]
  (make-seed!
   (-> empty-seed
       (assoc :f f)
       (seed/description (str "call " f))
       ;(seed/access-mode mode)
       (seed/access-indexed-deps args)
       (seed/datatype nil)
       (seed/compiler demo-compile-call-fn))))

(defmacro demo-make-fn [mode f]
  `(fn [& args#]
     (demo-call-fn ~mode (quote ~f) args#)))

(defn demo-sub-step-counter [dst counter-key]
  (swap! dst #(update % counter-key (fn [x] (inc (or x 0))))))

(def demo-pure-add (demo-make-fn :pure +))

(def demo-step-counter (demo-make-fn
                        :side-effectful demo-sub-step-counter))








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
                [(demo-step-counter 's :katt)
                 (demo-step-counter 's :katt)
                 (demo-step-counter 's :katt)])))))))

(deftest seq-coll-test
  (is (= '(1 2 3) (demo-embed '(1 2 3)))))

#_(generate-code
             (eval-body empty-state
                       (demo-step-counter 's :katt)))


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


(deftest static-if-cond-test
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
