(ns lime.core.typesystem-test
  (:require [clojure.test :refer :all]
            [lime.core.typesystem :refer :all]
            [lime.core.seed :as seed]
            [bluebell.utils.setdispatch :as sd]))

(deftest basic-tests
  (let [ss (-> 9
               class
               seed/typed-seed
               basic-indicator
               first
               seed-supersets)]
    (is (not (empty? ss)))
    (is (every? tagged-as-seed? ss)))
  (let [ss (-> 9
               class
               class-supersets)]
    (is (not (empty? ss)))
    (is (every? class? ss))))

(sd/def-dispatch my-fun system feature)

(sd/def-set-method my-fun "For instance, [:katt 119]"
  [
   [[:prefix :katt] a]
   ]
  [:prefixed-with-katt a])

(sd/def-set-method my-fun "Any tagged value ending with :kise, e.g. [119 :kise]"
  [
   [[:suffix :kise] x]
   ]
  [:this-is-a-kise (first x)])

(sd/def-set-method my-fun "Any vector"
  [
   [:vector x]
   ]
  [:this-is-a-vector x])


(deftest my-fun-test
  
  (is (= [:this-is-a-vector [:katt]]
         (my-fun [:katt])))
  
  (is (= (my-fun [:katt 119])
         [:prefixed-with-katt [:katt 119]]))

  (is (= [:this-is-a-kise 12]
         (my-fun [12 :kise]))))
