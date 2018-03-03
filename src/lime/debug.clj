(ns lime.debug
  (:require [bluebell.utils.debug :as debug]))

(defn basic-inspect [x]
  (debug/limited-pprint x))

(def inspector (atom basic-inspect))

(defn inspect [x]
  ((deref inspector) x)
  x)

(defn set-inspector [x]
  (reset! inspector x))
