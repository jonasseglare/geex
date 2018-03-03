(ns lime.debug
  (:require [bluebell.utils.debug :as debug]))

(defn basic-inspect [x]
  (debug/limited-pprint x))

(def inspector (atom basic-inspect))
(def em-inspector (atom basic-inspect))

(defn inspect [x]
  ((deref inspector) x)
  x)

(defn inspect-expr-map [em]
  ((deref em-inspector) em)
  em)

(defn set-inspector [x]
  (reset! inspector x))

(defn set-expr-map-inspector [x]
  (reset! em-inspector x))
