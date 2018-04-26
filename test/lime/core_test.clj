(ns lime.core-test
  (:require [lime.core :refer :all :as lime] :reload)
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pp]
            [bluebell.utils.debug :as debug]
            [bluebell.utils.core :as utils]
            [clojure.spec.alpha :as spec]
            [lime.visualize :as viz]
            [lime.debug :refer :all]
            [lime.core.exprmap :as exm]
            [lime.core.defs :as defs]
            [lime.core.seed :as sd]))

(set-inspector (fn [x]
                 (println "Type signature:" (type-signature x))
                 (-> x
                     expr-map
                     viz/plot-expr-map)))

(set-expr-map-inspector (fn [x]
                          (viz/plot-expr-map x)))

(deftest a-test
  (with-context []
    (testing "FIXME, I fail."
      (let [x (with-requirements-fn [[:tag0123 :kattskit]]
                #(with-new-seed "katt" identity))]
        (is (defs/seed? x))
        (is (= :kattskit (-> x sd/access-deps first second))))
      (let [x (with-new-seed "x" sd/mark-dirty)
            y (with-new-seed "y" sd/mark-dirty)]
        (is (defs/seed? x))
        (is (number? (defs/dirty-counter x)))
        (is (= (inc (defs/dirty-counter x))
               (defs/dirty-counter y))))
      (is (= (replace-dirty (defs/last-dirty {} 9) 19)
             #:lime.core.defs{:last-dirty 19, :backup-dirty 9}))
      (record-dirties-fn
       :katt (fn []
               (is (= 119
                      (defs/last-dirty (record-dirties-fn
                                        119
                                        #(with-new-seed "katt" identity)))))
               (is (= :katt (-> lime/state deref defs/last-dirty)))))
      (record-dirties-fn
       :mu
       (fn []
         (let [r (inject-pure-code-fn
                  (fn [d]
                    (-> {}
                        (defs/result-value [:dirty d]) ;; What the result should be
                        (defs/last-dirty :braaaa))))]  ;; What the last dirty should be
           (is (= (defs/last-dirty (deref state)) :braaaa))
           (is (= r [:dirty :mu]))))))))

(deftest accessor-test
  (with-context []
    
    (is (= 9 (-> (with-requirements-fn [[:tag 9]]
                   #(sd/seed-deps-accessor (with-new-seed "Kattskit" identity)))
                 first)))
    (is (= (sd/access-indexed-deps (coll-seed {:a 1 :b 2}))
           [:a 1 :b 2]))
    (is (= {:a 119 :b 42}
           (compile-coll (exm/seed-map 
                          defs/empty-comp-state
                          {:a (defs/compilation-result {} :a)
                           :b (defs/compilation-result {} :b)
                           :katt (defs/compilation-result {} 119)
                           :skit (defs/compilation-result {} 42)})
                         (coll-seed {:a :katt :b :skit}) defs/compilation-result)))
    (is (= #{119 :a}
           (compile-coll (exm/seed-map 
                          defs/empty-comp-state
                          {:a (defs/compilation-result {} :a)
                           :b (defs/compilation-result {} :b)
                           :katt (defs/compilation-result {} 119)
                           :skit (defs/compilation-result {} 42)})
                         (coll-seed #{:a :katt}) defs/compilation-result)))
    
    
    (is (= [42 119]
           (compile-coll (exm/seed-map 
                          defs/empty-comp-state
                          {:a (defs/compilation-result {} :a)
                           :b (defs/compilation-result {} :b)
                           :katt (defs/compilation-result {} 119)
                           :skit (defs/compilation-result {} 42)})
                         (coll-seed [:skit :katt]) defs/compilation-result)))
    (is (= 9.0
           (compile-static-value
            defs/empty-comp-state
            (primitive-seed 9.0) defs/compilation-result)))

  (is (defs/seed? (to-seed 9)))
  (is (defs/seed? (to-seed [:a :b :c])))
  (is (defs/seed? (-> 9 to-seed to-seed to-seed)))
  (is (= (type-signature [9 9 (to-seed 10)])
         [9 9 (defs/datatype {} (class 9))]))
  (is (= (flatten-expr {:a 9 :b (to-seed 10)})
         [(to-seed 10)]))
  (is (= (type-signature {:a (to-seed 10.0)})
         {:a #:lime.core.defs{:type java.lang.Double}}))
  (is (not (= (to-seed 10.0) (to-seed 10))))
  (is (= (to-seed 10.0) (to-seed 10.0)))

  (type-signature [:a {:b 'k}])
  (is (= (sd/access-seed-coll {:a 9 :b 10})
         [:a 9 :b 10]))
  (is (= (sd/access-seed-coll (to-seed {:a 4 :b 5}))
         [:a 4 :b 5]))
  (is (= [:katt :skit]
         (sd/access-seed-coll (-> (with-new-seed "kattskit"
                                    (fn [s]
                                      (sd/add-deps s {:a :katt
                                                      :b :skit})))))))
  (is (= 9 (compile-seed defs/empty-comp-state
                         (:a (populate-seeds {:a (to-seed 10)} [(to-seed 9)]))
                         defs/compilation-result)))
  (let [src (-> [9 10]
                (exm/preprocess to-seed)
                exm/build-key-to-expr-map)
        ks (-> src
               :expr2key
               vals)
        rp (exm/replace-deps-by-keys src)
        rp-dep-vals (map sd/access-deps (vals rp))]
    (is (every? keyword? ks))
    (is (= 3 (count ks)))
    (is (keyword (:top-key src)))
    (is (defs/seed? (:top-expr src)))
    (is (map? rp))
    (is (every? map? rp-dep-vals))
    (is (every? keyword? (reduce into #{} (map vals rp-dep-vals)))))
    (is (map? (exm/summarize-expr-map (expr-map {:a 'a}))))))

(deftest basic-graph-test
  (let [em (with-context []
                     (expr-map
                      (dirty+ (dirty+ 1 2) 3)))]
    (is (-> em
            exm/seed-map
            count
            (= 5))))
  (is (= 2 (count
            (filter
             (complement empty?)
             (map sd/referents
                  (-> (with-context []
                        (expr-map (dirty+ 1 2)))
                      exm/seed-map
                      vals))))))
  (let [roots (exm/expr-map-roots (with-context [] (expr-map (dirty+ 1 2))))]
    (is (= 2 (count roots)))
    (is (every? (partial = "primitive-seed")
                (map (comp sd/description second)
                     roots)))))

(deftest basic-compilation-test
  (let [init-state (initialize-compilation-state
                    (with-context [] (expr-map (dirty+ 1 2))))
        [to-cmp popped-state] (exm/pop-key-to-compile init-state)]
    (is (= [] (access-bindings init-state)))
    (is (= 2 (count (exm/access-to-compile init-state))))
    (is (keyword? to-cmp))
    (is (= 1 (count (exm/access-to-compile popped-state)))))
  (is (= 1 (with-context [] 
             (compile-top 1))))
  (let [comp-state (with-context [] 
                     (compile-full 1 identity))]
    (is (= 1 (-> comp-state
                 exm/seed-map
                 first ;; First element in map
                 second ;; the value (not the key)
                 defs/compilation-result)))
    (is (= 1 (-> comp-state
                 defs/compilation-result ;; Last value to be compiled
                 ))))
  (let [compiled-expr (with-context [] 
                        (compile-full
                         (pure+ (pure+ 1 2) (pure+ 1 2))
                         terminate-return-expr))]
    (is (= 6 (eval compiled-expr)))
    (is (= 1 (count (utils/indices-of (str compiled-expr) "(+ 1 2)"))))))




;; EXAMPLE OF IF!!!
#_(utils/with-flags [debug-init-seed]
                  (with-context []
                    (disp-expr-map
                     (expr-map
                      (If true 3 4)))))

(deftest basic-inlining-test
  (is (= 3 (inject [] (pure+ 1 2)))))

(deftest dont-bind-primitives-test
  (let [expr (str (macroexpand `(inject [] (pure+ 1 1 1))))]
    (is (= -1 (.indexOf expr "let")))
    (is (= 3 (count (utils/indices-of expr "1"))))))

(defn test-mini-if [a]
  (inject []
          (if2 'a
              (to-seed 3)
              (to-seed 4))))

(deftest test-the-if
  (is (= 3 (test-mini-if true)))
  (is (= 4 (test-mini-if false))))

;; (with-context [] (pp/pprint (expr-map (dirty (pure+ 1 2)))))

(defn bound-if [a]
  (inject []
          (let [x (if2 'a
                      (to-seed 3)
                      (to-seed 4))]
            [x x])))


(deftest bound-if-test
  (is (= [3 3] (bound-if true)))
  (is (= [4 4] (bound-if false))))


(deftest pack-and-unpack-test
  (is (=  [9 10]
          (inject []
                  (pack {:a (to-seed 9)
                         :b (to-seed 10)}))))
  (is (= {:a 9 :b 10}
         (inject
          []
          (unpack {:a (to-seed 0)
                   :b (to-seed 0)}
                  (pack {:a (to-seed 9)
                         :b (to-seed 10)})))))
  (is (= {:x 119}
         (inject
          []
          (unpack {:x (to-seed 0)}
                  (pack {:x (to-seed 119)}))))))

(defn packed-if-test-fun [a]
  (inject []
          (if2 'a
              {:value (to-seed 3)
               :a 'a}                            
              {:value (to-seed 4)
               :a 'a})))

(deftest more-complex-test-with-packing
  (is (= {:a true :value 3} (packed-if-test-fun true)))
  (is (= {:a false :value 4} (packed-if-test-fun false))))

(defn test-fun-use-wrapped-value [a]
  (inject []
          (pure* 2.0
                 (-> (if2 'a
                         {:result (to-seed 119)}
                         {:result (to-seed 120)})
                     :result))))

(deftest wrapped-if-test
  (is (= 238.0 (test-fun-use-wrapped-value true)))
  (is (= 240.0 (test-fun-use-wrapped-value false))))

(defn test-nested-ifs-fun [value]
  (inject []
          (if2 (pure< 'value 2)
              (if2 (pure= 'value 0)
                  {:result (to-seed 1000)}
                  {:result (to-seed 2000)})
              (if2 (pure= 'value 2)
                  {:result (to-seed 3000)}
                  {:result (to-seed 4000)}))))

(deftest test-nested-ifs-test
  (is (= {:result 1000} (test-nested-ifs-fun 0)))
  (is (= {:result 2000} (test-nested-ifs-fun 1)))
  (is (= {:result 3000} (test-nested-ifs-fun 2)))
  (is (= {:result 4000} (test-nested-ifs-fun 3))))

(deftest side-effect-reverse-test
  (let [x (atom {})]
    (is (= [{:a 9 :b 10} {:a 9}]
           (inject []
                   (vec
                    (reverse
                     [(atom-assoc 'x :a 9)
                      (atom-assoc 'x :b 10)])))))
    )
  (let [x (atom {})]
    (is (= [{:a 9} {:a 9 :b 10}]
           (inject []
                   [(atom-assoc 'x :a 9)
                    (atom-assoc 'x :b 10)]))))
  (let [x (atom [])]
    (is (= [[9 4 120] [9 4] [9]]
           (inject
            [] (vec (reverse [(atom-conj 'x 9)
                              (atom-conj 'x 4)
                              (atom-conj 'x 120)])))))))

(defn add-some-keys-from [x i])

(defn small-stateful-if2 [n]
  (let [x (atom [])]
    (inject []
            (if2 (pure< 'n 3)
                (do (atom-conj 'x 1)
                    :end)
                :end))
    x))

(deftest small-test-stateful-if
  (is (= [1] (deref (small-stateful-if2 0))))
  (is (= [1] (deref (small-stateful-if2 1))))
  (is (= [] (deref (small-stateful-if2 4)))))

(defn more-complex-stateful-if2 [n]
  (let [x (atom [])]
    (inject []
            (do
              (atom-conj 'x 0)
              (atom-conj 'x 1)
              (if2 (pure< 'n 2)
                  (do (atom-conj 'x 3)
                      (atom-conj 'x 4)
                      :end)
                  (do (atom-conj 'x 5)
                      (atom-conj 'x 6)
                      :end))
              (atom-conj 'x 7)
              (atom-conj 'x 8)))
    (deref x)))

(deftest more-complex-if-test
  (is (= [0 1 5 6 7 8]
         (more-complex-stateful-if2 9)))
  (is (= [0 1 3 4 7 8]
         (more-complex-stateful-if2 0))))

(defn bind-outside-if-test-fn [n]
  (inject []
          (let [a {:b (pure+ 1 'n)}]
            (if2 (pure< 'n 4)
                [:k (pure+ 0 (:b a)) (pure+ 3 (:b a))]
                [:k (pure+ 4 (:b a)) (pure+ 300 (:b a))]))))

;; The above expr expands to something like
#_(let* [wrapped-function2239922422 (+ 1 n)]
  (clojure.core/let [if-termination2241122423 (if (< n 4) [(+ 0 wrapped-function2239922422)
                                                           (+ 3 wrapped-function2239922422)]
                                                  [(+ 4 wrapped-function2239922422)
                                                   (+ 300 wrapped-function2239922422)])]
    [:k
     (clojure.core/nth if-termination2241122423 0)
     (clojure.core/nth if-termination2241122423 1)]))

(deftest bind-outside-if-test
  (is (= [:k 1 4]
         (bind-outside-if-test-fn 0))))


(deftest refactored-loop-first-test
  (is (= {:result 9 :twice 18}
         (inject [] (basic-loop2
                     {:init (to-dynamic 0)
                      :eval identity
                      :loop? (fn [state] (pure< state 9))
                      :next (fn [evaled]
                              (pure-inc evaled))
                      :result (fn [x] {:result x
                                       :twice (pure* 2 x)})})))))


;;;;; Loop test
(deftest first-loop-test
  (is (= {:product 24
          :value 0
          :loop? false}
         (inject []
                 (basic-loop2
                  {:init  {:value (to-type defs/dynamic-type (to-seed 4))
                           :product (to-type defs/dynamic-type (to-seed 1))}
                   :eval (fn [x] (merge x {:loop?  (pure< 0 (:value x))}))
                   :loop? :loop?
                   :next (fn [x] {:value (pure-dec (:value x))
                           :product (pure* (:product x)
                                           (:value x))})
                   :result identity})))))

(deftest with-return-value-fn-test
  (is (= 24
         (inject []
                 (basic-loop2
                  {:init {:value (to-type defs/dynamic-type (to-seed 4))
                          :product (to-type defs/dynamic-type (to-seed 1))} 
                   :eval (fn [x] (merge x {:loop?  (pure< 0 (:value x))}))
                   :loop? :loop?
                   :next (fn [x] {:value (pure-dec (:value x))
                                  :product (pure* (:product x)
                                                  (:value x))})
                   :result :product})))))

(deftest loop-test-wrapped
  (is (= 21
         (inject []
                 (let [x (:product
                          (basic-loop2
                           {:init {:value (to-type defs/dynamic-type (to-seed 3))
                                   :product (to-type defs/dynamic-type (to-seed 1))}
                            :eval (fn [x] (merge x {:loop?  (pure< 0 (:value x))}))
                            :loop? :loop?
                            :next (fn [x] {:value (pure-dec (:value x))
                                           :product (pure* (:product x)
                                                           (:value x))})
                            :result identity}))]
                   (pure+
                    9
                    x x))))))

(deftest initialize-seed-out-of-context-test
  (is (defs/seed? (with-new-seed "kattskit" identity))))

#_(deftest reduce-test
  (is (= 15
         (inject
          []
          (my-basic-reduce pure+
                           (to-dynamic 0)
                           (to-dynamic [1 2 3 4 5]))))))

#_(deftest nested-loop-test-sum
  (is (= 28
         (inject
          [{}]
          (my-basic-reduce (fn [sum x]
                             (pure+ sum (my-basic-sum x)))
                           (to-dynamic 0)
                           (to-dynamic [[1 2] [3 4] [5 6 7]]))))))

#_(defn stateful-looper []
  (let [mut (atom {:a 0
                   :b 1})]
    (inject
     []
     (basic-loop
      {:i (to-dynamic 0)}
      (fn [state]
        (assoc state :loop? (pure< (:i state) 10)))
      (fn [state]
        (fibonacci-step 'mut)
        (update state :i pure-inc))))
    (deref mut)))

#_(deftest stateful-looper-test
  (is (= {:a 55, :b 89}
         (stateful-looper))))

(defn disp-test-scope3 []
  (inject []
   (with-context []
     (dirty+ 1 2)
     (scope {:desc "Katsk" :dirtified? true :flush-root? false}
            (dirty+ 3 4)))))

(deftest test-scope-test
  (is (= 7 (disp-test-scope3))))

#_(deftest test-packing-to-local-vars
  (is (= [nil {:a 9 :b 10}]
         (inject [] (let [x {:a (to-seed 9)
                             :b (to-seed 10)}
                          pup (pack-unpack-fn-pair x)]
                      [((:pack pup) x)
                       (:unpacked pup)])))))

(defn try-if-2-test [c a b]
  (inject [] (if2 'c 'a 'b)))

(deftest if-2-test-case
  (is (= 3 (try-if-2-test true 3 4)))
  (is (= 4 (try-if-2-test false 3 4))))











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC

;; If there is an inexplicable error in eval, 
;; it probably means an external variable wasn't quoted properly.
#_(defn test-nested-ifs [value]
             (inject [] (pure< value 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOOPING
;; REMEMBER: Flush local vars!!!

#_(spec/def ::loop-proto-form [initial-state ;; <-- A function
                             loop?      ;; <-- A function
                             next-state ;; <-- A function
                             ])
;; BUT FIRST, WHEN DEALING WITH SPECIAL CONSTRUCTS:
;; Suppose we have advanced the compilation frontier to only have
;; ifs and loops touching it. Now, we want to compile as much as possible
;; outside of loop bodies and outside of conditional branches. To select
;; the next form to compile, do this:
;;    1. Find the form does not depend directly, or indirectly, on
;;       any of the other *frontier* forms (but can depend on forms already compiled).
;;    2. Once found, now figure out all seeds that need to be compiled,
;;       as part of compiling that form.
;;
;; In meta-expression evaluation: Make the loop-root-seed and in a scope
;; evaluate all the rest, so that it depends on the root seed. Makes it easy
;; to track exactly what we need.
;;
;; How we generate the loop:
;; 1. *** A special loop-root-seed. When compiling:
;;    - Make sure we have compiled as much as possible of
;;      everything that does not have a special status of a loop.
;;    - First bind loop invariants and then
;;      flush the loop.
;;    - Then, make the wrapping loop initialization.
;; 2. Loop state expressions, that depend on the loop root seed.
;; 3. The loop condition expression, depends on the loop state expressions.
;; 4. *** A special loop if-seed, that depends on the condition.
;;        When compiling, it will
;;           1. First compile the next form, then
;;           2. Forward control to the loop termination
;; 5. *** A special loop termination seed: It is just there to
;;        track dependencies. But the final loop variable expression
;;        depend on it.
