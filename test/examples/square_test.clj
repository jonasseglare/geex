(ns examples.square-test
  (:require [geex.common :as common]
            [geex.java :as java]))

(java/typed-defn
   geex-square [Double/TYPE x]
   (common/* x x))
