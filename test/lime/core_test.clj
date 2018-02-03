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
         (compile-coll (comp-state-node-map 
                        {} {:a (compilation-result {} :a)
                            :b (compilation-result {} :b)
                            :katt (compilation-result {} 119)
                            :skit (compilation-result {} 42)})
                       (coll-seed {:a :katt :b :skit}) compilation-result)))
  (is (= #{119 :a}
         (compile-coll (comp-state-node-map 
                        {} {:a (compilation-result {} :a)
                            :b (compilation-result {} :b)
                            :katt (compilation-result {} 119)
                            :skit (compilation-result {} 42)})
                       (coll-seed #{:a :katt}) compilation-result)))
  (is (= [42 119]
         (compile-coll (comp-state-node-map 
                        {} {:a (compilation-result {} :a)
                            :b (compilation-result {} :b)
                            :katt (compilation-result {} 119)
                            :skit (compilation-result {} 42)})
                       (coll-seed [:skit :katt]) compilation-result))))
