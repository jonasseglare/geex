(ns lime.core.seed
  (:require [clojure.spec.alpha :as spec]
            [lime.core.defs :as defs]
            [bluebell.utils.party :as party]
            [bluebell.utils.core :as utils]))

;; The dependencies of a seed
(def access-deps (party/key-accessor ::defs/deps))

(def access-compiled-deps (party/key-accessor ::defs/compiled-deps))

(defn add-deps [dst extra-deps]
  (party/update dst
                access-deps
                #(merge % extra-deps)))

(defn disp-deps [x]
  (println "DEPS:" (-> x access-deps keys))
  x)

(defn only-numeric-keys [m]
  (filter (fn [[k v]] (number? k)) m))

(defn access-indexed-map
  ([] {:desc "access-indexed-map"})
  ([x] (mapv second (sort-by first (only-numeric-keys x))))
  ([x y] (merge x (zipmap (range (count y)) y))))

(def seed-deps-accessor (party/conditional-accessor

                         ;; Extract the dependency map, then the values
                         ;; for ordered keys
                         (party/chain access-deps utils/map-vals-accessor)

                         ;; Let anything else than a seed? fall through.
                         defs/seed?))

(def access-indexed-deps (party/chain access-deps access-indexed-map))

(def seed? defs/seed?)
(def compiled-seed? defs/compiled-seed?)
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

(def datatype defs/datatype)

(def description defs/description)
