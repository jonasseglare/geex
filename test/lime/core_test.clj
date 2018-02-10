(ns lime.core-test
  (:require [clojure.test :refer :all]
            [lime.core :refer :all :as lime]
            [clojure.spec.alpha :as spec]))

(deftest a-test
  (testing "FIXME, I fail."
    (let [x (with-requirements [:kattskit]
              #(initialize-seed "katt"))]
      (is (seed? x))
      (is (= :kattskit (-> x deps first second))))
    (let [x (dirty (initialize-seed "x"))
          y (dirty (initialize-seed "y"))]
      (is (seed? x))
      (is (number? (dirty-counter x)))
      (is (= (inc (dirty-counter x))
             (dirty-counter y))))
    (is (= (replace-dirty (last-dirty {} 9) 19)
           #:lime.core{:last-dirty 19, :backup-dirty 9}))
    (record-dirties
     :katt (fn []
           (is (= 119
                  (last-dirty (record-dirties 119 #(initialize-seed "katt")))))
             (is (= :katt (-> lime/state deref last-dirty)))))
    (record-dirties
     :mu
     (fn []
       (let [r (inject-pure-code
                 (fn [d]
                   (-> {}
                       (result-value [:dirty d])
                       (last-dirty :braaaa))))]
         (is (= (last-dirty (deref state)) :braaaa))
         (is (= r [:dirty :mu])))))))

(deftest accessor-test
  (is (= 9 (-> (with-requirements [9] #(seed-deps-accessor (initialize-seed "Kattskit")))
               first)))
  (is (= (access-indexed-deps (coll-seed {:a 1 :b 2}))
         [:a 1 :b 2]))
  (is (= {:a 119 :b 42}
         (compile-coll (node-map 
                        empty-comp-state
                        {:a (compilation-result {} :a)
                         :b (compilation-result {} :b)
                         :katt (compilation-result {} 119)
                         :skit (compilation-result {} 42)})
                       (coll-seed {:a :katt :b :skit}) compilation-result)))
  (is (= #{119 :a}
         (compile-coll (node-map 
                        empty-comp-state
                        {:a (compilation-result {} :a)
                         :b (compilation-result {} :b)
                         :katt (compilation-result {} 119)
                         :skit (compilation-result {} 42)})
                       (coll-seed #{:a :katt}) compilation-result)))
  (is (= [42 119]
         (compile-coll (node-map 
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
                               (deps {:a :katt
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
        rp-dep-vals (map deps (vals rp))]
    (is (every? keyword? ks))
    (is (= 3 (count ks)))
    (is (keyword (:top-key src)))
    (is (seed? (:top-expr src)))
    (is (map? rp))
    (is (every? map? rp-dep-vals))
    (is (every? keyword? (reduce into #{} (map vals rp-dep-vals)))))
  (is (map? (summarize-expr-map (expr-map {:a 'a})))))

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

(def pure+ (wrapfn-pure +))
(def pure- (wrapfn-pure -))
(def pure* (wrapfn-pure *))
(def purediv (wrapfn-pure /))


;; TODO:



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
;;    2. Once found, now figure out all nodes that need to be compiled,
;;       as part of compiling that form.
;;
;; In meta-expression evaluation: Make the loop-root-node and in a scope
;; evaluate all the rest, so that it depends on the root node. Makes it easy
;; to track exactly what we need.
;;
;; How we generate the loop:
;; 1. *** A special loop-root-node. When compiling:
;;    - Make sure we have compiled as much as possible of
;;      everything that does not have a special status of a loop.
;;    - First bind loop invariants and then
;;      flush the loop.
;;    - Then, make the wrapping loop initialization.
;; 2. Loop state expressions, that depend on the loop root node.
;; 3. The loop condition expression, depends on the loop state expressions.
;; 4. *** A special loop if-node, that depends on the condition.
;;        When compiling, it will
;;           1. First compile the next form, then
;;           2. Forward control to the loop termination
;; 5. *** A special loop termination node: It is just there to
;;        track dependencies. But the final loop variable expression
;;        depend on it.
