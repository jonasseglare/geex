(ns geex.jcore-test
  (:require [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.jcore :refer :all :as jcore]))

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
         [:a :b {:c 3}])))
