(ns geex.core.seed
  (:require [clojure.spec.alpha :as spec]
            [geex.core.defs :as defs]
            [bluebell.utils.party :as party]
            [bluebell.utils.party.coll :as partycoll]
            [bluebell.utils.core :as utils]
            [bluebell.tag.core :as tg]))

;; The dependencies of a seed
(def access-deps (party/key-accessor ::defs/deps))

(def access-seed-data (party/key-accessor ::defs/seed-data))

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

(def access-indexed-map
  (party/wrap-accessor
   {:desc "access-indexed-map"
    :getter (fn [x] (mapv second (sort-by first (only-numeric-keys x))))
    :setter (fn [x y] (merge x (zipmap (range (count y)) y)))}))

(def access-compiled-indexed-deps (party/chain access-compiled-deps access-indexed-map))

(def seed-deps-accessor (party/conditional-accessor

                         ;; Extract the dependency map, then the values
                         ;; for ordered keys
                         (party/chain access-deps partycoll/map-vals-accessor)

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

(def flat-deps (party/chain access-deps partycoll/map-vals-accessor))


(def access-seed-coll-sub
  "Special function used to access the collection over which to recur when there are nested expressions"
  (party/wrap-accessor
   {:desc "access-seed-coll"
    :getter (fn [x]
              (cond
                (seed? x) (flat-deps x)
                (coll? x) x
                :default []))
    :setter (fn [x new-value]
              (cond
                (seed? x) (flat-deps x new-value)
                (coll? x) new-value
                :default x))}))

(def access-seed-coll
  (party/chain
   access-seed-coll-sub
   partycoll/normalized-coll-accessor))

(def access-tags defs/access-tags)

(defn add-tag [seed x]
  (party/update seed access-tags #(conj % x)))

(defn has-tag? [seed x]
  (contains? (access-tags seed) x))


(defn gen-dirty-key []
  [::defs/dirty (defs/contextual-gensym)])

(defn depend-on-dirty [dst x]
  (add-deps dst {(gen-dirty-key) x}))

(defn set-dirty-dep [dst x]
  (if (seed? x)
    (depend-on-dirty dst x)
    dst))




(defn mark-dirty
  ([seed value]
   (assoc seed :marked-dirty? value))
  ([seed]
   (mark-dirty seed true)))

(defn marked-dirty? [seed]
  (:marked-dirty? seed))

(defn mark-scope-root [seed] (add-tag seed :scope-root))
(defn scope-root? [seed] (has-tag? seed :scope-root))
(defn mark-scope-termination [seed] (add-tag seed :scope-termination))
(defn scope-termination? [seed] (has-tag? seed :scope-termination))

(defn typed-seed [tp]
  (datatype {} tp))
