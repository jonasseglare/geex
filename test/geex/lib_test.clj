(ns geex.lib-test
  (:require [geex.java :as java :refer [typed-defn]]
            [geex.core :as core]
            [geex.lib :as lib]
            [clojure.test :refer :all]))

(typed-defn add-3 [Double/TYPE a
                   Double/TYPE b
                   Double/TYPE c]
            (lib/+ a b c))

(deftest add-3-test
  (is (= (add-3 9.0 4.1 1.15)
         14.25)))
