(ns geex.core.seed
  (:import [geex ISeed TypedSeed])
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.party :as party]
            [bluebell.utils.wip.core :as utils]
            [bluebell.utils.wip.tag.core :as tg]
            [bluebell.utils.wip.java :refer [set-field]]))


(def seed? (partial instance? ISeed))

;; The dependencies of a seed
(defn access-deps [x]
  {:pre [(seed? x)]}
  (.deps x))

(defn deps-map [^ISeed x]
  (.getMap (.deps x)))

(defn access-mode [^ISeed x]
  (.getMode x))

(defn typed-seed? [x]
  (instance? TypedSeed x))

(defn compilable-seed?
  "A seed that can be compiled"
  [x]
  (and (seed? x)
       (not (typed-seed? x))))

(defn only-numeric-keys [m]
  (filter (fn [[k v]] (number? k)) m))

(def access-indexed-map
  (party/wrap-accessor
   {:desc "access-indexed-map"
    :getter (fn [x] (mapv second (sort-by first (only-numeric-keys x))))
    :setter (fn [x y] (merge x (zipmap (range (count y)) y)))}))

(defn access-compiled-indexed-deps
  ([^ISeed seed]
   {:pre [(instance? ISeed seed)]}
   (.compilationResultsToArray (.deps seed))))

(defn datatype [^ISeed x]
  {:pre [(seed? x)]}
  (.getType x))

(defn description [x]
  (.getDescription x))

(defn typed-seed [tp]
  (TypedSeed. tp))

(defn strip-seed [seed]
  (typed-seed (datatype seed)))

(defn access-compiled-deps [sd]
  {:pre [(seed? sd)]}
  (.getCompilationResults (.deps sd)))

(defn access-seed-data [x]
  {:pre [(seed? x)]}
  (.getData x))

(defn compilation-result [^ISeed seed]
  (.getCompilationResult (.getState seed)))

(defn set-seed-type! [seed new-type]
  (let [p (.getParams seed)]
    (set-field p type new-type)
    seed))
