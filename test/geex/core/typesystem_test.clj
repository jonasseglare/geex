(ns geex.core.typesystem-test
  (:require [clojure.test :refer :all]
            [geex.core.typesystem :refer :all]
            [geex.core.seed :as seed]
            [bluebell.utils.wip.symset :as ss]
            [bluebell.utils.wip.setdispatch :as sd]))

(deftest basic-tests
  (let [ss (->> 9
                class
                seed/typed-seed
                basic-indicator
                first
                (seed-supersets ss/empty-set-registry))]
    (is (not (empty? ss))))
  (let [ss (->> 9
                class
                (class-supersets ss/empty-set-registry))]
    (is (not (empty? ss)))
    ))

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

(sd/def-set-method my-fun "Adding numbers"
  [
   [[:map-type :add] a]
   [[:map-type :add] b]]
  {:type :add
   :value (+ (:value a) (:value b))})

(sd/def-set-method my-fun "Numeric seed"
  [
   [[:seed java.lang.Number] a]
   ]

  [:numeric-seed (seed/datatype a)])

(sd/def-set-method my-fun "Add integers"
  [
   [[:array :integer] values]
   ]
  [:int-sum (apply + values)])

(sd/def-set-method my-fun "Convert primitive array to vector"
  [
   [:array x]
   ]
  (vec x))

(sd/def-set-method my-fun "Merging maps"
  [
   [:map a]
   [:map b]
   ]
  (merge a b))


(deftest my-fun-test
  
  (is (= [:this-is-a-vector [:katt]]
         (my-fun [:katt])))
  
  (is (= (my-fun [:katt 119])
         [:prefixed-with-katt [:katt 119]]))

  (is (= [:this-is-a-kise 12]
         (my-fun [12 :kise])))

  (is (= [:numeric-seed java.lang.Double]
         (my-fun (seed/typed-seed java.lang.Double))))

  (is (= [:int-sum 6]
         (my-fun (int-array [1 2 3]))))

  (is (= [true false true]
         (my-fun (boolean-array [true false true]))))

  (is (= {:type :add :value 19}
         (my-fun {:type :add :value 9}
                 {:type :add :value 10}))))

(sd/def-dispatch get-type-sig system feature)

(sd/def-set-method get-type-sig [[[:platform :clojure] p]
                                 [:any arg]]
  (keyword arg))

(sd/def-set-method get-type-sig [[[:platform :java] p]
                                 [:keyword arg]]
  (str (name arg)))

(sd/def-set-method get-type-sig [[[:platform :java] p]
                                 [:string arg]]
  (str "'" arg "'"))

(deftest test-get-type-sig
  (is (= "kattskit" (get-type-sig [:platform :java] :kattskit)))
  (is (= "'kattskit'" (get-type-sig [:platform :java] "kattskit")))
  (is (= :kattskit (get-type-sig [:platform :clojure] "kattskit"))))

(sd/def-dispatch type-string system feature)

(sd/def-set-method type-string [
                                [:any p]
                                ]
  "Anything")

(sd/def-set-method type-string [[:nil x]]
  "nil")

(deftest nil-test
  (is (= "Anything" (type-string 9)))
  (is (= "nil" (type-string nil))))
