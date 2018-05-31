(ns lime.core.datatypes-test
  (:require [lime.core.datatypes :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as cljset]
            [bluebell.utils.core :as utils]))

(deftest subset-test
  (is (cljset/subset? (utils/keyset primitive-types)
                      (conj (utils/keyset sample-type-map)
                            java.lang.Void)))
  (is (primitive-type? java.lang.Double))
  )


