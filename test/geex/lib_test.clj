(ns geex.lib-test
  (:require [geex.java :as java :refer [typed-defn]]
            [geex.core :as core]
            [geex.lib :as lib]
            [clojure.test :refer :all]))

(typed-defn add-3 [Double/TYPE a
                   Double/TYPE b
                   Double/TYPE c]
            (lib/+ a b c))

(deftest add-3-test
  (is (= (add-3 9.0 4.1 1.15)
         14.25)))

(typed-defn  and-3
            [Boolean/TYPE a
             Boolean/TYPE b
             Boolean/TYPE c
             ]
            (lib/and a b c))

(defn gen-combos [n]
  (if (= n 1)
    [[true] [false]]
    (transduce
     (comp (map (fn [combo]
                  [(conj combo false) (conj combo true)]))
           cat)
     conj
     []
     (gen-combos (dec n)))))

(defn true-and-3 [a b c]
  (and a b c))

(deftest and-3-test
  (doseq [combo (gen-combos 3)]
    (let [actual (apply and-3 combo)
          expected (apply true-and-3 combo)]
      (is (= actual expected))
      (when (not= actual expected)
        (println "Combo" combo)
        (println "Actual" actual)
        (println "Expected" expected)))))


(typed-defn or-3
            :debug
            [Boolean/TYPE a
             Boolean/TYPE b
             Boolean/TYPE c]
            (lib/or a b c))

(defn true-or-3 [a b c]
  (or a b c))

(deftest test-or-3
  (doseq [combo (gen-combos 3)]
    (is (= (apply or-3 combo)
           (apply true-or-3 combo)))))
