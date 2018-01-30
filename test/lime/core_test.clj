(ns lime.core-test
  (:require [clojure.test :refer :all]
            [lime.core :refer :all :as lime]
            [clojure.spec.alpha :as spec]))

(deftest a-test
  (testing "FIXME, I fail."
    (let [x (with-requirements [:kattskit]
              #(initialize-seed))]
      (is (seed? x))
      (is (= :kattskit (-> x deps first second))))
    (let [x (dirty (initialize-seed))
          y (dirty (initialize-seed))]
      (is (seed? x))
      (is (number? (dirty-counter x)))
      (is (= (inc (dirty-counter x))
             (dirty-counter y))))))
