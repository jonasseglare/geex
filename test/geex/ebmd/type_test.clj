(ns geex.ebmd.type-test
  (:require [geex.ebmd.type :as type]
            [bluebell.utils.ebmd :as ebmd]
            [clojure.test :refer :all]
            [geex.core.seed :as seed]))

(deftest primitive-test
  (is (ebmd/matches-arg-spec? ::type/double-value 3.0))
  (is (ebmd/matches-arg-spec? ::type/float-value (float 3.0)))
  (is (ebmd/matches-arg-spec? ::type/int-value (int 3)))
  (is (ebmd/matches-arg-spec? ::type/long-value (long 3)))
  (is (ebmd/matches-arg-spec? ::type/short-value (short 3)))
  (is (ebmd/matches-arg-spec? ::type/byte-value (byte 3)))
  (is (ebmd/matches-arg-spec? ::type/char-value (char 3)))
  (is (ebmd/matches-arg-spec? ::type/boolean-value (boolean false))))


(ebmd/declare-poly add-0)

(ebmd/def-poly add-0 [::type/boolean-value a
                      ::type/boolean-value b]
  (or a b))


(ebmd/def-poly add-0 [::type/double-value a
                      ::type/double-value b]
  [:double (+ a b)])

(ebmd/def-poly add-0 [::type/float-value a
                      ::type/float-value b]
  [:float (+ a b)])

(deftest add-0-test
  (is (= true (add-0 false true)))
  (is (= [:double 3.7] (add-0 2 1.7)))
  (is (= [:float 3.0] (add-0 2 (float 1.0)))))


(ebmd/declare-poly add-1)

(ebmd/def-poly add-1 [::type/real-value a
                      ::type/real-value b]
  [:real (+ a b)])

(ebmd/def-poly add-1 [::type/integer-value a
                      ::type/integer-value b]
  [:integer (+ a b)])

(ebmd/def-poly add-1 [::type/coll-value a
                      ::type/coll-value b]
  (into a b))


(ebmd/def-poly add-1 [::type/real-array-value a
                      ::type/real-array-value b]
  (double-array (map + (vec a) (vec b))))

(ebmd/def-poly add-1 [::type/real a
                      ::type/real b]
  [:common-real a b])

(deftest add-1-test
  (is (= [:real 3.4] (add-1 1.4 2)))
  (is (= [:integer 3] (add-1 1 2)))
  (is (= #{1 2 3} (add-1 #{1 2} [3])))
  (is (= [11.0 22.0 33.0]
         (vec (add-1 (double-array [1 2 3])
                     (int-array [10 20 30])))))
  (is (= (first (add-1 (seed/typed-seed Double/TYPE)
                       3))
         :common-real)))



(deftest resolve-type-test
  (is (nil? (type/resolve-type :kattskit)))
  (is (= :kattskit
         (type/resolve-type
          (seed/typed-seed :kattskit))))
  (is (= java.lang.AbstractMethodError
         (type/resolve-type
          java.lang.AbstractMethodError)))
  (is (= Long/TYPE
         (type/resolve-type ::type/long))))
