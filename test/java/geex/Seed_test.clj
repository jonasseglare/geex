(ns geex.Seed-test
  (:import [geex Seed DynamicSeed
            SeedParameters Mode TypedSeed
            SeedUtils])
  (:require [clojure.test :refer :all]
            [bluebell.utils.wip.java :as java :refer [set-field]]))

(defn params-with-type [t]
  (doto (SeedParameters.)
    (set-field type t)
    (set-field description "Default")
    (set-field mode Mode/Pure)
    (set-field compiler identity)))

(deftest mode-test
  (let [src [-1 0 1 2]]
    (is (= src
           (mapv (fn [x]
                   (SeedUtils/intFromMode
                    (SeedUtils/modeFromInt x)))
                 src)))
    (is (= Mode/Ordered (SeedUtils/max
                         Mode/Pure
                         Mode/Ordered)))))

(deftest seed-test
  (let [p0 (params-with-type Double/TYPE)
        p1 (params-with-type Double/TYPE)

        s0 (doto (DynamicSeed. p0) (.setId 119))
        s1 (doto (DynamicSeed. p1) (.setId 119))

        q0 (doto (DynamicSeed.
                  (params-with-type Integer/TYPE))
             (.setId 119))
        r0 (doto (DynamicSeed.
                  (params-with-type Double/TYPE))
             (.setId 120))]
    (is (.equals s0 s1))
    (is (= s0 s1))
    (is (= s1 s0))
    (is (not= p0 q0))
    (is (not= p0 r0))
    (is (not= r0 q0))
    (is (= (TypedSeed. Character/TYPE)
           (DynamicSeed. (params-with-type Character/TYPE))))
    (is (not= (TypedSeed. Character/TYPE)
              (DynamicSeed. (params-with-type Boolean/TYPE))))
    (is (not= s0 nil))
    (is (= {s0 2 r0 3}
           {s1 2 r0 3}))
    (is (not= {s0 3 r0 3}
              {s1 2 r0 3}))
    (is (= 1 (count (conj #{s0} s1))))
    (is (= 2 (count (conj #{s0} q0))))

    (do
      (.addDep (.deps s0) :kattskit r0)
      (.setData s0 :kattskit)
      (is (= :kattskit (.getData s0))))

    
    #_(is (= (DynamicSeed. p0)
           (DynamicSeed. p0)
           
           ))))
