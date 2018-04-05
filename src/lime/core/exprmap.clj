(ns lime.core.exprmap
  (:require [clojure.spec.alpha :as spec]
            [lime.core.defs :as defs]
            [lime.core.seed :as sd]
            [bluebell.utils.core :as utils]
            [clojure.pprint :as pp]
            [bluebell.utils.party :as party]))

(def seed-map (party/chain (party/key-accessor ::defs/seed-map)))

(defn seed-at-key [comp-state seed-key]
  (assert (map? comp-state))
  (-> comp-state
      seed-map
      (get seed-key)))

(defn traverse-expr-map-sub [dst expr-map at settings]
  (let [seed (seed-at-key expr-map at)]
    (if (or (contains? dst at)
            (not ((:visit?-fn settings) [at seed])))
      dst
      (reduce
       (fn [dst neigh]
         (traverse-expr-map-sub dst expr-map neigh settings))
       (conj dst at)
       ((:neigh settings) seed)))))

(defn traverse-expr-map
  "Given an expr-map, a starting position, a function that returns the neighbours of a seed and function that returns true if a seed can be visited, traverse the graph and return a set of visited seeds. It will not visit the neighbours of a seed if the seed itself is not visited."
  [expr-map
   start
   get-neighbours-of-seed-fn
   visit?-fn]
  (utils/data-assert (keyword? start)
                     "The start has to be a keyword"
                     {:start start})
  (assert (fn? get-neighbours-of-seed-fn))
  (assert (fn? visit?-fn))
  (traverse-expr-map-sub
   #{}
   expr-map
   start
   {:neigh get-neighbours-of-seed-fn
    :visit?-fn visit?-fn}))

(def always-visit (constantly true))

(defn deep-seed-deps
  ([expr-map seed-key] (deep-seed-deps expr-map seed-key always-visit))
  ([expr-map seed-key visit?]
   (utils/data-assert (keyword? seed-key)
                      "Seed key must be a keyword"
                      {:seed-key seed-key})
   (traverse-expr-map
    expr-map
    seed-key
    sd/dep-neighbours
    visit?)))

(def access-top (party/key-accessor ::top))

(defn top-seed [comp-state]
  (get (seed-map comp-state)
       (access-top comp-state)))



(def default-access-omit-for-summary #{::defs/omit-for-summary ::compiler})

;; Just for debugging, to understand how the expression got parsed.
(defn summarize-expr-map [expr-map]
  (party/update
   expr-map
   seed-map
   (fn [m]
     (into
      {}
      (map (fn [[k v]]
             (assert (sd/seed? v))
             (let [all-keys (set (keys v))
                   keys-to-keep (clojure.set/difference
                                 all-keys
                                 (clojure.set/union
                                  default-access-omit-for-summary
                                  (set (defs/access-omit-for-summary v))))]
               [k (select-keys v keys-to-keep)]))
           m)))))

(defn disp-expr-map [m]
  (-> m
      summarize-expr-map
      pp/pprint))

(defn disp-and-return-expr-map [m]
  (disp-expr-map m)
  m)



(defn seed-map-roots
  "Get the root seeds of the exm/seed-map, which is where we start."
  [m]
  (filter
   (fn [[k v]]
     (empty? (sd/access-deps v)))
   m))

(defn expr-map-roots [m]
  (-> m
      seed-map
      seed-map-roots))

(defn compiled-seed-key? [comp-state seed-key]
  (sd/compiled-seed?
   (-> comp-state
       seed-map
       seed-key)))

(defn compilation-roots [m]
  (filter
   (fn [[k v]]
     (and (not (sd/compiled-seed? v))
          (every? (partial compiled-seed-key? m)
                  (-> v sd/access-deps vals))))
   (seed-map m)))

(defn initial-set-to-compile [m]
  (set (map (fn [[k v]]
              k)
            (compilation-roots m))))

(def access-to-compile defs/access-to-compile)

(defn initialize-compilation-roots [m]
  (access-to-compile m (initial-set-to-compile m)))

(defn keep-keys-and-referents [m ks]
  (transduce
   (comp (filter (fn [[k v]] (contains? ks k)))
         (map (fn [[k v]] [k (sd/keep-keys-in-refs v ks)])))
   conj
   {}
   m))

(defn select-sub-tree [comp-state k]
  (utils/data-assert (keyword? k) "The provided sub tree key must be a keyword"
                     {:key k})
  (let [dd (deep-seed-deps comp-state k)]
    (-> comp-state
        (party/update seed-map #(keep-keys-and-referents % dd))
        (access-top k)
        initialize-compilation-roots)))


(defn add-to-compile [dst seed-key]
  (party/update
   dst
   access-to-compile
   #(conj % seed-key)))

(defn try-add-to-compile [comp-state seed-key]
  (assert (keyword? seed-key))

  
  (let [seed (-> comp-state ;; <-- The seed that we are considering compiling
                      seed-map
                      seed-key)
        deps-vals (-> seed  ;; <-- What the seed depends on
                      sd/access-deps
                      vals)]

    ;; Is every dependency of the seed compiled?
    (if (and (not (sd/compiled-seed? seed))
             (every? (partial compiled-seed-key? comp-state) deps-vals))

      ;; If yes, we add it...
      (add-to-compile comp-state
                      seed-key) ;; <-- ...with the order.
      comp-state)))


(def access-seed-key defs/access-seed-key)

(defn scan-referents-to-compile [comp-state]
  (let [seed-key (access-seed-key comp-state)
        refs (->> comp-state
                  seed-map
                  seed-key
                  sd/referents
                  (map second) ;; <-- Keyword of the seeds
                  )]
    (reduce try-add-to-compile comp-state refs)))

(defn access-seed-to-compile
  ([] {:desc "access-seed-to-compile"})
  ([comp-state] (let [k (access-seed-key comp-state)]
                  (-> comp-state
                      seed-map
                      k)))
  ([comp-state s] (let [k (access-seed-key comp-state)]
                    (party/update
                     comp-state
                     seed-map
                     (fn [dst] (assoc dst k s))))))

(defn update-comp-state-seed [comp-state seed-key f]
  (party/update
   comp-state
   seed-map
   (fn [m] (update m seed-key f))))



(defn put-result-in-seed [comp-state]
  (update-comp-state-seed
   comp-state
   (access-seed-key comp-state) 
   #(defs/compilation-result % (defs/compilation-result comp-state))))

(defn initialize-seed-compilation [comp-state seed-key]
  (-> comp-state
      defs/clear-compilation-result
      (access-seed-key seed-key)
      scan-referents-to-compile))

(defn pop-key-to-compile
  "Returns the first key to compile, and the comp-state with
that key removed"
  [comp-state]
  (let [to-comp (access-to-compile comp-state)
        f (first to-comp)
        r (disj to-comp f)]
    (when defs/debug-seed-order
      (println "Popped" f))
    [f (access-to-compile comp-state r)]))

;; Access indexed dependencies
(defn lookup-compiled-results
  "Replace every arg by its compiled result"
  [state arg-map]
  (assert (map? arg-map))
  (let [m (seed-map state)]
    (into {} (map (fn [[k v]]
                    [k (defs/compilation-result (get m v))]) arg-map))))

(defn lookup-compiled-indexed-results [comp-state expr]
  (sd/access-indexed-map
   (lookup-compiled-results
    comp-state (sd/access-deps expr))))

(defn get-seed [expr-map key]
  (utils/data-assert (contains? (seed-map expr-map) key)
                     "No such seed with that key"
                     {:seed-key key})
  (-> expr-map
      seed-map
      key))
