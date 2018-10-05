(ns geex.jcore-test
  (:require [clojure.test :refer :all]
            [geex.jcore :refer :all :as jcore]))

(deftest with-state-test
  (let [s (with-state-fn nil
            (fn [] global-state))]
    (is (state? s)))
  (is (nil? global-state))
  (is (thrown? Exception (#'jcore/get-state)))
  (is (state? (with-state-fn nil #(#'jcore/get-state))))
  )
