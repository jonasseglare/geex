(ns lime.core-test
  (:require [lime.core :refer :all :as lime] :reload-all)
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pp]
            [bluebell.utils.debug :as debug]
            [bluebell.utils.core :as utils]
            [clojure.spec.alpha :as spec]
            [lime.visualize :as viz]))

(deftest a-test
  (with-context []
    (testing "FIXME, I fail."
      (let [x (with-requirements-fn [[:tag0123 :kattskit]]
                #(initialize-seed "katt"))]
        (is (seed? x))
        (is (= :kattskit (-> x access-deps first second))))
      (let [x (dirty (initialize-seed "x"))
            y (dirty (initialize-seed "y"))]
        (is (seed? x))
        (is (number? (dirty-counter x)))
        (is (= (inc (dirty-counter x))
               (dirty-counter y))))
      (is (= (replace-dirty (last-dirty {} 9) 19)
             #:lime.core{:last-dirty 19, :backup-dirty 9}))
      (record-dirties-fn
       :katt (fn []
               (is (= 119
                      (last-dirty (record-dirties-fn 119 #(initialize-seed "katt")))))
               (is (= :katt (-> lime/state deref last-dirty)))))
      (record-dirties-fn
       :mu
       (fn []
         (let [r (inject-pure-code-fn
                  (fn [d]
                    (-> {}
                        (result-value [:dirty d]) ;; What the result should be
                        (last-dirty :braaaa))))]  ;; What the last dirty should be
           (is (= (last-dirty (deref state)) :braaaa))
           (is (= r [:dirty :mu]))))))))

(deftest accessor-test
  (with-context []
    
    (is (= 9 (-> (with-requirements-fn [[:tag 9]]
                   #(seed-deps-accessor (initialize-seed "Kattskit")))
                 first)))
    (is (= (access-indexed-deps (coll-seed {:a 1 :b 2}))
           [:a 1 :b 2]))
    (is (= {:a 119 :b 42}
           (compile-coll (seed-map 
                          empty-comp-state
                          {:a (compilation-result {} :a)
                           :b (compilation-result {} :b)
                           :katt (compilation-result {} 119)
                           :skit (compilation-result {} 42)})
                         (coll-seed {:a :katt :b :skit}) compilation-result)))
    (is (= #{119 :a}
           (compile-coll (seed-map 
                          empty-comp-state
                          {:a (compilation-result {} :a)
                           :b (compilation-result {} :b)
                           :katt (compilation-result {} 119)
                           :skit (compilation-result {} 42)})
                         (coll-seed #{:a :katt}) compilation-result)))
    
    
    (is (= [42 119]
           (compile-coll (seed-map 
                          empty-comp-state
                          {:a (compilation-result {} :a)
                           :b (compilation-result {} :b)
                           :katt (compilation-result {} 119)
                           :skit (compilation-result {} 42)})
                         (coll-seed [:skit :katt]) compilation-result)))
    (is (= 9.0
           (compile-primitive-value {} (primitive-seed 9.0) compilation-result)))

  (is (seed? (to-seed 9)))
  (is (seed? (to-seed [:a :b :c])))
  (is (seed? (-> 9 to-seed to-seed to-seed)))
  (is (= (type-signature [9 9 (to-seed 10)])
         [9 9 (datatype {} (class 9))]))
  (is (= (flatten-expr {:a 9 :b (to-seed 10)})
         [(to-seed 10)]))
  (is (= (type-signature {:a (to-seed 10.0)})
         {:a #:lime.core{:type java.lang.Double}}))
  (is (not (= (to-seed 10.0) (to-seed 10))))
  (is (= (to-seed 10.0) (to-seed 10.0)))

  (type-signature [:a {:b 'k}])
  (is (= (access-seed-coll {:a 9 :b 10})
         [:a 9 :b 10]))
  (is (= (access-seed-coll (to-seed {:a 4 :b 5}))
         [:a 4 :b 5]))
  (is (= [:katt :skit]
         (access-seed-coll (-> (initialize-seed "kattskit")
                               (add-deps {:a :katt
                                          :b :skit})))))
  (is (= 9 (compile-seed empty-comp-state
                         (:a (populate-seeds {:a (to-seed 10)} [(to-seed 9)]))
                         compilation-result)))
  (let [src (-> [9 10]
               preprocess
               build-key-to-expr-map)
        ks (-> src
               :expr2key
               vals)
        rp (replace-deps-by-keys src)
        rp-dep-vals (map access-deps (vals rp))]
    (is (every? keyword? ks))
    (is (= 3 (count ks)))
    (is (keyword (:top-key src)))
    (is (seed? (:top-expr src)))
    (is (map? rp))
    (is (every? map? rp-dep-vals))
    (is (every? keyword? (reduce into #{} (map vals rp-dep-vals)))))
    (is (map? (summarize-expr-map (expr-map {:a 'a}))))))

;; To demonstrate a hack that can be used
;; to efficiently return composite types
;; in if-forms.
;;
;; Use it only if there are at least two values to return.
(defn sum-of [square? a b]
  (loop [done? false
         x 0.0
         y 0.0]
    (if done?

      ;; Here the code that depends on the branch is expanded.
      (+ x y)
      
      (if square?
        (recur true (* a a) (* b b))
        (recur true a b)))))

(deftest if-with-multiple-branch-values
  (is (= 5 (sum-of false 2 3)))
  (is (= 13 (sum-of true 2 3))))

;; For testing it
(def pure+ (wrapfn-pure +))
(def pure- (wrapfn-pure -))
(def pure* (wrapfn-pure *))
(def purediv (wrapfn-pure /))
(def pure< (wrapfn-pure <))
(def pure<= (wrapfn-pure <=))
(def pure= (wrapfn-pure =))
(def pure-not (wrapfn-pure not))
(def dirty+ (wrapfn +))
(def dirty- (wrapfn -))
(def dirty* (wrapfn *))
(def dirtydiv (wrapfn /))
(def dirty< (wrapfn <))
(def dirty<= (wrapfn <=))
(def dirty= (wrapfn =))
(def dirty-not (wrapfn not))

(deftest basic-graph-test
  (let [em (with-context []
                     (expr-map
                      (dirty+ (dirty+ 1 2) 3)))]
    (is (-> em
            seed-map
            count
            (= 5))))
  (is (= 2 (count
            (filter
             (complement empty?)
             (map referents
                  (-> (with-context []
                        (expr-map (dirty (pure+ 1 2))))
                      seed-map
                      vals))))))
  (let [roots (expr-map-roots (with-context [] (expr-map (dirty (pure+ 1 2)))))]
    (is (= 2 (count roots)))
    (is (every? (partial = "primitive-seed")
                (map (comp description second)
                     roots)))))

(deftest basic-compilation-test
  (let [init-state (initialize-compilation-state
                    (with-context [] (expr-map (dirty (pure+ 1 2)))))
        [to-cmp popped-state] (pop-key-to-compile init-state)]
    (is (= [] (access-bindings init-state)))
    (is (= 2 (count (access-to-compile init-state))))
    (is (keyword? to-cmp))
    (is (= 1 (count (access-to-compile popped-state)))))
  (is (= 1 (with-context [] 
             (compile-top 1))))
  (let [comp-state (with-context [] 
                     (compile-full 1 identity))]
    (is (= 1 (-> comp-state
                 seed-map
                 first ;; First element in map
                 second ;; the value (not the key)
                 compilation-result)))
    (is (= 1 (-> comp-state
                 compilation-result ;; Last value to be compiled
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

(deftest basic-if-test-structure

  ;; 1. condition
  ;; 2. bifurcation
  ;; 3. true
  ;; 4. Indirection
  ;; 5. false
  ;; 6. Indirection
  ;; 7. termination
  (is (= 7 (-> (with-context []
                 (expr-map
                  (If 'a
                      (to-seed 3)
                      (to-seed 4))))
               seed-map
               count))))

(defn test-mini-if [a]
  (inject []
          (If 'a
              (to-seed 3)
              (to-seed 4))))

(deftest test-the-if
  (is (= 3 (test-mini-if true)))
  (is (= 4 (test-mini-if false))))

(defn sample-graph-001 []
  (viz/plot-expr-map
   (with-context []
     (expr-map
      (If 'a 3 4)))))

(def s002 (with-context []
            (let [k (pure+ 3 4)]
              (expr-map
               (pure+ k (If 'a k (to-seed 5)))))))

(def test-key (->> s002 seed-map
                   keys
                   (filter (fn [k]
                             (= 0 (.indexOf (name k) "indir"))))
                   first))

(def s002-removed (select-sub-tree s002 test-key))

(deftest test-remove-key
  (is (< (-> s002-removed
             seed-map
             count)
         (-> s002
             seed-map
             count))))

(defn sample-graph-002 []
  (viz/plot-expr-map
   s002))
;; (with-context [] (pp/pprint (expr-map (dirty (pure+ 1 2)))))

(defn bound-if [a]
  (inject []
          (let [x (If 'a
                      (to-seed 3)
                      (to-seed 4))]
            [x x])))


(deftest bound-if-test
  (is (= [3 3] (bound-if true)))
  (is (= [4 4] (bound-if false))))

(println "Fix this"
         (macroexpand '(inject []
                               (let [x (If 'a
                                           (to-seed 3)
                                           (to-seed 4))]
                                 [x x]))))


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
                         :b (to-seed 10)}))))))


;; TODO:
;; 2. Ensure that, whenever a node X depends on a
;;    bifurcation B and another node Y (that does not depend on B), then B depends on Y.



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
