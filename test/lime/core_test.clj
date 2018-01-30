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
             (dirty-counter y))))
    (is (= (replace-dirty (last-dirty {} 9) 19)
           #:lime.core{:last-dirty 19, :backup-dirty 9}))
    (is (= 119
           (last-dirty (record-dirties 119 #(initialize-seed)))))))
