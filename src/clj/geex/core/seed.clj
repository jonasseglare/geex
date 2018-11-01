(ns geex.core.seed
  (:import [geex Seed TypedSeed])
  (:require [clojure.spec.alpha :as spec]
            [geex.core.defs :as defs]
            [bluebell.utils.wip.party :as party]
            [bluebell.utils.wip.party.coll :as partycoll]
            [bluebell.utils.wip.core :as utils]
            [bluebell.utils.wip.tag.core :as tg]))


(def seed? (partial instance? Seed))

;; The dependencies of a seed
(defn access-deps [x]
  {:pre [(seed? x)]}
  (.deps x))

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
  ([seed]
   {:pre [(instance? Seed seed)]}
   (.compilationResultsToArray (.deps seed))))

(defn access-indexed-deps [seed-params]
  (assert false))

#_(def compiled-seed? defs/compiled-seed?)
(def referents defs/referents)

(defn keep-keys-in-refs [seed ks]
  (party/update seed referents (fn [r] (filter (fn [[k v]] (contains? ks v)) r))))

(defn filter-referents-of-seed [seed pred]
  (set
   (filter
    identity
    (map (fn [[k v]]
           (if (pred k) v)) ;;
         (referents seed)))))

(defn referents-with-key [seed key]
  (filter-referents-of-seed seed (partial = key)))

(defn referent-with-key [seed key]
  (first (referents-with-key seed key)))

(defn filter-deps [seed pred]
  (->> seed
       access-deps
       (map (fn [[k v]]
              (if (pred k)
                v)))
       (filter (complement nil?))))

;; Helper for filter-referents-of-seed
(defn dep-tagged? [x]
  (fn [y]
    (and (vector? y)
         (= (first y) x))))

(defn find-dep [seed pred]
  (first (filter-deps seed pred)))

(def static-value (party/key-accessor :static-value))

(def compiler defs/compiler)

(def access-pretweak defs/access-pretweak)

(def pretweak? defs/pretweak?)

(defn datatype [x]
  {:pre [(seed? x)]}
  (.getType x))

(defn description [x]
  (.getDescription x))

(defn referent-neighbours
  "Get the referent neighbours"
  [seed]
  (->> seed
       referents
       (map second)
       set))

(defn dep-neighbours
  "Get the dependent neighbours"
  [seed]
  (->> seed
       access-deps
       vals
       set))

(defn all-seed-neighbours [seed]
  (clojure.set/union
   (referent-neighbours seed)
   (dep-neighbours seed)))

(def access-bind? defs/access-bind?)

(defn access-deps-or-empty
  ([] (access-deps))
  ([x] (if (contains? x ::defs/deps)
         (access-deps x)
         {}))
  ([x y] (access-deps x y)))

#_(def flat-deps (party/chain access-deps-or-empty partycoll/map-vals-accessor))

#_(def access-seed-coll-sub
  "Special function used to access the collection over which to recur when there are nested expressions"
  (party/wrap-accessor
   {:desc "access-seed-coll"
    :getter (fn [x]
              (cond
                (compilable-seed? x) (flat-deps x)
                (coll? x) x
                :default []))
    :setter (fn [x new-value]
              (cond
                (compilable-seed? x) (flat-deps x new-value)
                (coll? x) new-value
                :default x))}))

#_(def access-seed-coll
  (party/chain
   access-seed-coll-sub
   partycoll/normalized-coll-accessor))

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

(defn disp-deps [x]
  (println "DEPS:" (-> x access-deps keys))
  x)

