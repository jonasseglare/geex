(ns geex.jcore-test
  (:require [clojure.test :refer :all]
            [geex.core.defs :as defs]
            [geex.jcore :refer :all :as jcore]))

(deftest with-state-test
  (let [s (with-state-fn nil
            (fn [] global-state))]
    (is (state? s)))
  (is (nil? global-state))
  (is (thrown? Exception (#'jcore/get-state)))
  (is (state? (with-state-fn nil #(#'jcore/get-state))))
  (let [s (with-state nil (to-seed ::defs/nothing))]
    (is (state? s))
    (is (seed? (.getOutput s))))
  (is (seed?
       (.getOutput (with-state nil (to-seed Double/TYPE)))))
  (let [s (with-state nil
            (wrap 1)
            (wrap 2)
            (wrap 3))]
    (is (= 3 (.getSeedCount s))))
  (let [s (eval-body nil (wrap 1) (wrap 2) (wrap 3))]
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
  #_(is (= (demo-embed (let [x (wrap [1 2])] [x x]))
         [[1 2] [1 2]])))
