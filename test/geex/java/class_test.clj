(ns geex.java.class-test
  (:require [geex.java.class :refer :all :as jc]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(deftest class-def-test
  (let [x ((variable Double/TYPE a) (init-class-def 'Kattskit))]
    (is (= 1 (count (:vars x))))
    (let [y (-> x :vars first)]
      (is (= y {:symbol 'a, :type Double/TYPE, :static? false, :visibility :public}))))
  (is (= {:symbol 'a, :type Double/TYPE, :static? false, :visibility :private}
         (-> ((private (variable Double/TYPE a)) (init-class-def 'Katt))
             :vars
             first)))
  (is (= {:symbol 'a, :type Double/TYPE, :static? true, :visibility :private}
         (-> ((private (static (variable Double/TYPE a))) (init-class-def 'Katt))
             :vars
             first)))
  (is (= (-> (defclass Kattskit (private (variable Double/TYPE a)))
             :vars
             first
             :visibility)
         :private))) 
