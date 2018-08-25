(ns geex.core.exprmap
  (:require [clojure.spec.alpha :as spec]
            [geex.core.defs :as defs]
            [geex.core.seed :as sd]
            [bluebell.utils.core :as utils]
            [bluebell.utils.traverse :as traverse]
            [clojure.pprint :as pp]
            [bluebell.utils.specutils :as specutils]
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

(defn get-seed [expr-map k]
  (utils/data-assert (contains? (seed-map expr-map) k)
                     "No such seed with that key"
                     {:seed-key k})
  (-> expr-map
      seed-map
      k))

(defn update-seed [expr-map key f]
  (assert (keyword? key))
  (assert (fn? f))
  (party/update
   expr-map
   seed-map
   (fn [sm]
     (assert (contains? sm key))
     (update sm key f))))

(defn replace-deps-by-keys
  "Replace deps by keys"
  [src]
  (let [expr2key (:expr2key src)]
    (into
     {}
     (map (fn [[expr key]]
            (assert (sd/seed? expr))
            [key
             (party/update
              expr
              sd/flat-deps
              (fn [fd]
                (map (partial get expr2key) fd)))])
          expr2key))))

(defn add-referent [referent dst-map [ref-key dep-key]]
  (update dst-map
          dep-key
          (fn [dst-seed]
            (party/update
             dst-seed
             sd/referents
             (fn [dst-deps-map]
               (conj (specutils/validate
                      ::defs/referents dst-deps-map)
                     [ref-key referent]))))))

(defn accumulate-referents [dst-map [k seed]]
  (assert (keyword? k))
  (assert (sd/seed? seed))
  (assert (map? dst-map))
  (reduce (partial add-referent k) dst-map (sd/access-deps seed)))

(defn compute-referents [m]
  (assert (map? m))
  (reduce accumulate-referents m m))

(defn labeled-dep [label]
  [(keyword label)
   (keyword (defs/contextual-gensym label))])

(defn add-expr-map-deps [expr-map label seed-key extra-deps]
  (assert (string? label))
  (assert (keyword? seed-key))
  (assert (set? extra-deps))
  (assert (every? keyword? extra-deps))
  (update-seed
   expr-map
   seed-key
   (fn [seed]
     (let [existing-deps (-> seed
                             sd/access-deps
                             vals
                             set)
           to-add (clojure.set/difference
                   extra-deps existing-deps)]
       (sd/add-deps
        seed
        (into {}
              (map (fn [d] [(labeled-dep label) d])
                   to-add)))))))


(defn generate-seed-key [seed]
  (-> (or (::defs/description seed) "nodesc")
      str
      defs/contextual-gensym
      keyword))

(defn postprocess-generated-keys
  "Helper of ubild-key-to-expr-map"
  [[m top]]
  (let [x  {:expr2key (into {} (map (fn [[k v]] [k (:mapped v)]) m))
            :key2expr (into {} (map (fn [[k v]] [(:mapped v) k]) m))
            :top-key top}]
    (assoc x :top-expr (get (:key2expr x) top))))

;; Build a key to expr map
(defn build-key-to-expr-map [expr]
  (postprocess-generated-keys
   (traverse/traverse-postorder-cached
    {}
    expr
    {:visit generate-seed-key
     :access-coll sd/access-seed-coll})))




;; Preprocess every seed inside
;; But don't assign keys
(defn preprocess [expr subexpr-visitor]
  (second
   (traverse/traverse-postorder-cached
    {}
    expr
    {:visit subexpr-visitor
     :access-coll sd/access-seed-coll})))


(defn expr-map-sub
  "The main function analyzing the expression graph"
  [raw-expr subexpr-visitor]
  (let [lookups (-> raw-expr

                    ;(utils/first-arg (begin :preprocess))
                    (preprocess subexpr-visitor)
                    ;(utils/first-arg (end :preprocess))

                    ;(utils/first-arg (begin :key-to-expr-map))
                    build-key-to-expr-map
                    ;(utils/first-arg (end :key-to-expr-map))
                    
                    )
        top-key (:top-key lookups)
        ]
    (seed-map             ;; Access the exm/seed-map key
     (access-top {} top-key) ;; Initial map
     (-> lookups

         ;(utils/first-arg (begin :replace-deps-by-keys))
         replace-deps-by-keys
         ;(utils/first-arg (end :replace-deps-by-keys))

         ;(utils/first-arg (begin :compute-referents))
         compute-referents
         ;(utils/first-arg (end :compute-referents))
         
         ))

    ;; Post computations on the full map
    ))

(defn perform-pretweak [expr-map [k seed]]
  (if (sd/pretweak? seed)
    ((sd/access-pretweak seed) expr-map k seed)
    expr-map))

(defn perform-pretweaks [expr-map]
  (reduce
   perform-pretweak
   expr-map
   (-> expr-map seed-map)))

(defn expr-map [raw-expr subexpr-visitor]
  
  (-> raw-expr

      ;(utils/first-arg (begin :expr-map-sub))
      ;; Build the expr-map
      (expr-map-sub subexpr-visitor)
      ;(utils/first-arg (end :expr-map-sub))

      ;(utils/first-arg (begin :pretweaks))
      ;; Every seed can make adjustments to the graph
      perform-pretweaks
      ;(utils/first-arg (end :pretweaks))
            
      ;; After every seed has made adjustments, there may
      ;; be more referents to add.
      (party/update seed-map compute-referents)
      ))


(defn get-compiled-deps [comp-state expr]
  (lookup-compiled-results
   comp-state (sd/access-deps expr)))

(defn initialize-seed-to-compile [comp-state seed-key]
  (let [expr (seed-at-key comp-state seed-key)]
    (-> expr
        (sd/access-compiled-deps (get-compiled-deps comp-state expr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn platform-tag
  "Returns a tagged value that can be used to dispatch on."
  [comp-state]
  [:platform (defs/access-platform comp-state)])



;;;; Static code
(def access-static-code (party/default-value (party/key-accessor :static-code
                                                                 {:req-on-get false})
                                             []))

(defn add-static-code [comp-state added-code]
  (party/update comp-state access-static-code
                (fn [static-code]
                  (conj (or static-code []) added-code))))

(defn get-static-code [comp-state]
  (access-static-code comp-state))
