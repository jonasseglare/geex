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
    (is (= 3 (.getSeedCount s)))))