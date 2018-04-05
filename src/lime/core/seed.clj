(ns lime.core.seed
  (:require [clojure.spec.alpha :as spec]
            [lime.core.defs :as defs]
            [bluebell.utils.party :as party]))

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

#_(def seed-deps-accessor (party/conditional-accessor

                         ;; Extract the dependency map, then the values
                         ;; for ordered keys
                         (party/chain defs/access-deps utils/map-vals-accessor)

                         ;; Let anything else than a seed? fall through.
                         defs/seed?))

(def access-indexed-deps (party/chain access-deps access-indexed-map))
