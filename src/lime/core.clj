(ns lime.core
  (:require [bluebell.utils.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]
            [clojure.pprint :as pp]
            [clojure.string :as cljstr]
            [bluebell.utils.debug :as debug]
            [bluebell.utils.specutils :as specutils]))

;; Phases:
;;
;;  - The user builds a nested datastructure, where some values are seeds
;;      NOTE:
;;        - Symbols represent unknown values
;;  - We traverse the datastructure, and every seed becomes a seed
;;  - We remap the datastructure, assigning a symbol to every seed.
;;  - We build a graph
;;  - We traverse the graph from the bottom, compiling everything.


;; Whenever we evaluate something, it may
;; access this for type information, etc.
(def ^:dynamic evaluation-context nil)

(def ^:dynamic state nil)


;;; Pass these as arguments to utils/with-flags, e.g.
;; (with-context []
;;  (utils/with-flags [debug-seed-names debug-seed-order]
;;    (compile-full
;;     (pure+ (pure+ 1 2) (pure+ 1 2))
;;     terminate-return-expr)))

(def ^:dynamic debug-seed-names false)
(def ^:dynamic debug-seed-order false)
(def ^:dynamic debug-init-seed false)
(def ^:dynamic debug-check-bifurcate false)

;; Special type that we use when we don't know the type
(def dynamic-type ::dynamic)

(def omit-for-summary (party/key-accessor ::omit-for-summary))

;;;;;;;;;;;;;;;;;;
(spec/def ::comp-state (spec/keys :req [::result
                                        ::seed-map]))

(def compilation-result (party/key-accessor ::compilation-result))

(defn clear-compilation-result [comp-state]
  (dissoc comp-state ::compilation-result))

(def seed-map (party/chain
               (party/key-accessor ::seed-map)))

(def empty-comp-state {::seed-map {}})

;;;;;;;;;;;;;;;;;;;,
;; State used during meta-evaluation

(def compile-everything (constantly true))

(defn initialize-state []
  (atom {::last-dirty nil ;; <-- last dirty generator
         ::requirements [] ;; <-- Requirements that all seeds should depend on
         ::dirty-counter 0 ;; <-- Used to generate a unique id for every dirty
         }))

(defmacro with-context [[eval-ctxt]& args]
  `(binding [evaluation-context ~eval-ctxt
             state (initialize-state)]
     ~@args))

(spec/def ::seed (spec/keys :req [::type
                                  ::compiler
                                  ::deps]))

(spec/def ::snapshot (spec/keys :req [::result-value
                                      ::last-dirty]))

(def recording? (party/key-accessor ::recording?))

;; Test if something is a seed
(defn seed? [x]
  (and (map? x)
       (contains? x ::type)))

;; Access the last dirty
(def last-dirty (party/key-accessor ::last-dirty))

;; Access the requirements
(def requirements (party/key-accessor ::requirements))

;; Access the dirty-counter
(def dirty-counter (party/key-accessor ::dirty-counter))
(defn dirty? [x]
  (and (seed? x)
       (contains? x ::dirty-counter)))

;; Increase the counter of the state map
(def inc-counter #(party/update % dirty-counter inc))

;; Helper for with-requirements
(defn append-requirements [r s]
  (party/update s requirements #(into % r)))

(defn with-requirements-fn [r f]
  (assert (fn? f))
  (let [initial-reqs (-> state deref requirements)
        new-reqs (swap! state (partial append-requirements r))
        result (f)
        old-reqs (swap! state #(requirements % initial-reqs))]
    result))

(defmacro with-requirements [r & body]
  `(with-requirements-fn
     ~r
     (fn [] ~@body)))

(spec/def ::requirement (spec/cat :tag keyword?
                                  :data (constantly true)))

(def requirement-tag (party/index-accessor 0))
(def requirement-data (party/index-accessor 1))

;; Associate the requirements with random keywords in a map,
;; so that we can merge it in deps.
(defn make-req-map []
  (into {} (map (fn [x]
                  (specutils/validate ::requirement x)
                  [[(requirement-tag x)
                    (keyword (gensym "req"))]
                   (requirement-data x)])
                (-> state deref requirements))))


;; Special access to a dirty, if any
(def dirty (party/key-accessor ::dirty))


;; The dependencies of a seed
(def access-deps (party/key-accessor ::deps))

;; The opposite of deps
(def referents (party/key-accessor ::referents))

(spec/def ::key-seedref-pair (spec/cat :key (constantly true)
                                       :seedref keyword?))

(spec/def ::referents (spec/coll-of ::key-seedref-pair))

;; The compiler of a seed
(def compiler (party/key-accessor ::compiler))

(def access-pretweak (party/key-accessor ::pretweak))
(defn pretweak? [x]
  (contains? x ::pretweak))

;; Access the datatype of the seed
(def datatype (party/key-accessor ::type))

(def description (party/key-accessor ::description))

(def access-bind? (party/key-accessor ::bind? {:req-on-get false}))

(def access-tags (party/key-accessor ::tags))

(defn add-tag [seed x]
  (party/update seed access-tags #(conj % x)))

(defn has-tag? [seed x]
  (contains? (access-tags seed) x))


;; Create a new seed, with actual requirements
(defn initialize-seed [desc]
  (when debug-init-seed
    (println (str  "Initialize seed with desc '" desc "'")))
  (assert (string? desc))
  (-> {}
      (access-deps (make-req-map))
      (access-tags #{})
      (referents #{})
      (compiler nil)
      (datatype nil)
      (omit-for-summary [])
      (description desc)))

;; Extend the deps map
(defn add-deps [dst extra-deps]
  (party/update dst
                access-deps
                #(merge % extra-deps)))

(defn gen-dirty-key []
  [::dirty (gensym)])

(defn depend-on-dirty [dst x]
  (add-deps dst {(gen-dirty-key) x}))

(defn set-dirty-dep [dst x]
  (if (dirty? x)
    (depend-on-dirty dst x)
    dst))

;; Call this function when a seed has been constructed,
;; but is side-effectful
(defn dirty [x]
  (last-dirty
   (swap! state
          (fn [s]
            (inc-counter
             (last-dirty
              s
              (-> x
                  (dirty-counter (dirty-counter s))
                  (set-dirty-dep (last-dirty s)))))))))


;; Access a backup place for the dirty, when using record-dirties
(def backup-dirty (party/key-accessor ::backup-dirty))

;; Access result value, of a snapshot type
(def result-value (party/key-accessor ::result-value))

(def snapshot? (partial spec/valid? ::snapshot))

(defn replace-dirty [s new-dirty]
  (-> s
      (backup-dirty (last-dirty s))
      (last-dirty new-dirty)))

;; Given an initial dirty, initialize the state
;; with that dirty, call (f) without any arguments,
;; and then return the result of f along with the final dirty
(defn record-dirties-fn [initial-dirty f]
  (assert (fn? f))
  (let [start-state (swap! state #(replace-dirty % initial-dirty))
        out (f)
        restored-state (swap! state #(replace-dirty % (backup-dirty start-state)))]
    (-> {}
        (result-value out)
        (last-dirty (backup-dirty restored-state)))))

(defmacro record-dirties [init & body]
  `(record-dirties-fn ~init (fn [] ~@body)))

;; The opposite of the above: f gets as input the last dirty,
;; and then it returns a snapshot with the result and
;; the new dirty that we'd like to use after this.
(defn inject-pure-code-fn [f]
  (let [current-state (deref state)
        snapshot (f (last-dirty current-state))]
    (assert (snapshot? snapshot))
    (swap! state #(last-dirty % (last-dirty snapshot)))
    (result-value snapshot)))

(defmacro inject-pure-code [[d] & body]
  `(inject-pure-code-fn (fn [~d] ~@body)))

;; TODO: Analyze all collections
;;       Build a map from keyword to expr
;;       Traverse expr and replace all exprs by their keys
;;       Start write compiler
;;       Later on: Ability to delay propagation (e.g. when evaluating the if)

;;; Accessors

;; Access the deps of a seed
(def seed-deps-accessor (party/conditional-accessor

                         ;; Extract the dependency map, then the values
                         ;; for ordered keys
                         (party/chain access-deps utils/map-vals-accessor)

                         ;; Let anything else than a seed? fall through.
                         seed?))

;; Access the original-coll
(def access-original-coll (party/key-accessor :original-coll))

;; Access a collection as indexed elements in a map
(defn access-indexed-map
  ([] {:desc "access-indexed-map"})
  ([x] (mapv second (sort-by first x)))
  ([x y] (merge x (zipmap (range (count y)) y))))


;; Access indexed dependencies
(def access-indexed-deps (party/chain access-deps access-indexed-map))

(defn lookup-compiled-results
  "Replace every arg by its compiled result"
  [state arg-map]
  (assert (map? arg-map))
  (let [m (seed-map state)]
    (into {} (map (fn [[k v]]
                    [k (compilation-result (get m v))]) arg-map))))

(defn lookup-compiled-indexed-results [comp-state expr]
  (access-indexed-map
   (lookup-compiled-results
    comp-state (access-deps expr))))

;; Compiler for the coll-seed type
(defn compile-coll [comp-state expr cb]
  (cb (compilation-result
       comp-state
       (utils/normalized-coll-accessor
        (access-original-coll expr)
        (lookup-compiled-indexed-results comp-state expr)))))

(defn coll-seed [x]
  (-> (initialize-seed "coll-seed")
      (access-indexed-deps (utils/normalized-coll-accessor x))
      (access-original-coll x)
      (omit-for-summary #{:original-coll})
      (compiler compile-coll)))

(def primitive-value (party/key-accessor :primitive-value))

(defn value-literal-type [x]
  (if (symbol? x)
    dynamic-type
    (class x)))

(defn compile-primitive-value [state expr cb]
  (cb (compilation-result state (primitive-value expr))))

(defn primitive-seed [x]
  (assert (not (coll? x)))
  (-> (initialize-seed "primitive-seed")
      (access-bind? false)
      (primitive-value x)
      (datatype (value-literal-type x))
      (compiler compile-primitive-value)))

;; Given a seed in the evaluated datastructure of a meta expression,
;; turn it into a seed.
(defn to-seed [x]
  (cond
    (seed? x) x
    (coll? x) (coll-seed x)
    :default (primitive-seed x)))


;;;;;; Analyzing an expression 
(defn access-no-deeper-than-seeds
  ([] {:desc "access-no-deeper-than-seeds"})
  ([x] (if (seed? x)
         []
         (utils/coll-accessor x)))
  ([x y] (if (seed? x)
           x
           (utils/coll-accessor x y))))

(def top-seeds-accessor
  (party/chain
   access-no-deeper-than-seeds
   utils/normalized-coll-accessor))


;;;;; Used by the flat-seeds-accessor.
(defn symbol-to-seed [x]
  (if (symbol? x)
    (to-seed x)
    x))

;;; Helper for flat-seeds-traverse
(defn seed-conj-mapping-visitor [f]
  (fn [state x0]
    (let [x (symbol-to-seed x0)]
      (if (seed? x)
        [(conj state x) (f x)]
        [state x]))))

(defn flat-seeds-traverse
  "Returns a vector with first element being a list of 
  all original expr, the second being the expression
  with mapped seeds"
  [expr f]
  (utils/traverse-postorder-with-state
   [] expr
   {:visit (seed-conj-mapping-visitor f)
    :access-coll top-seeds-accessor
    }))

;; Get a datastructure that represents this type.
(defn type-signature [x]
  (second
   (flat-seeds-traverse
    x (fn [x] (datatype {} (datatype x))))))

;; Get only the seeds, in a vector, in the order they appear
;; when traversing
(defn flatten-expr [x]
  (first
   (flat-seeds-traverse x identity)))


(def flat-deps (party/chain access-deps utils/map-vals-accessor))

(defn access-seed-coll-sub
  "Special function used to access the collection over which to recur when there are nested expressions"
  ([] {:desc "access-seed-coll"})
  ([x]
   (cond
     (seed? x) (flat-deps x)
     (coll? x) x
     :default []))
  ([x new-value]
   (cond
     (seed? x) (flat-deps x new-value)
     (coll? x) new-value
     :default x)))

(def access-seed-coll
  (party/chain
   access-seed-coll-sub
   utils/normalized-coll-accessor))

(defn populate-seeds-visitor
  [state x]
  (if (seed? x)
    [(rest state) (first state)]
    [state x]))

(defn populate-seeds
  "Replace the seeds in dst by the provided list"
  ([dst seeds]
   (second
    (utils/traverse-postorder-with-state
     seeds dst
     {:visit populate-seeds-visitor
      :access-coll top-seeds-accessor}))))

(defn compile-seed [state seed cb]
  ((compiler seed) state seed cb))

(defn seed-at-key [comp-state seed-key]
  (assert (map? comp-state))
  (-> comp-state
      seed-map
      (get seed-key)))

(defn update-comp-state-seed [comp-state seed-key f]
  (party/update
   comp-state
   seed-map
   (fn [m] (update m seed-key f))))

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

(defn put-result-in-seed [comp-state]
  (update-comp-state-seed
   comp-state
   (access-seed-key comp-state) 
   #(compilation-result % (compilation-result comp-state))))


(declare scan-referents-to-compile)


(defn initialize-seed-compilation [comp-state seed-key]
  (-> comp-state
      clear-compilation-result
      (access-seed-key seed-key)
      scan-referents-to-compile))

(defn typehint [seed-type sym]
  (assert (symbol? sym))
  sym)


(spec/def ::dirty-key (spec/cat :prefix (partial = ::dirty)
                                :sym symbol?))

(defn dirty-key? [x]
  (spec/valid? ::dirty-key x))

(defn dirty-referents [refs]
  (assert (set? refs))
  (map first (filter (fn [[k v]] (dirty-key? k)) refs)))

(defn bind-seed?
  "Determinate if a seed should be bound to a local variable"
  [seed]
  (let [refs (referents seed)]
    (and
     (not= false (access-bind? seed))
     (or (dirty? seed)
         (< 1 (count refs))))))

(def access-bindings (party/key-accessor ::bindings))

(defn add-binding [comp-state sym-expr-pair]
  (party/update
   comp-state
   access-bindings
   #(conj % sym-expr-pair)))

(defn flush-bindings [comp-state cb]
  (let [bds (access-bindings comp-state)]
    (if (empty? bds)
      (cb comp-state)
      `(let ~(reduce into [] bds)
         ~(cb (access-bindings comp-state []))))))

;;;;;;;;;;;;; TODO
(defn maybe-bind-result [comp-state]
  (let [seed-key (access-seed-key comp-state)
        seed (-> comp-state
                 seed-map
                 seed-key)]
    (if (bind-seed? seed)
      (let [raw-sym (gensym (name seed-key))
            hinted-sym (typehint (datatype seed) raw-sym)
            result (compilation-result seed)]
        (-> comp-state
            (compilation-result hinted-sym) ;; The last compilation result is a symbol
            (add-binding [hinted-sym result]) ;; Add it as a binding
            (update-comp-state-seed ;; Update the seed so that it has the symbol as result.
             seed-key #(compilation-result % hinted-sym))))
      
      ;; Do nothing
      comp-state)))

;;;;;;;;;;;;; TODO

(defn compiled-seed-key? [comp-state seed-key]
  (contains?
   (-> comp-state
       seed-map
       seed-key)
   ::compilation-result))

(def access-to-compile (party/key-accessor ::to-compile))

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
                      access-deps
                      vals)]

    ;; Is every dependency of the seed compiled?
    (if (every? (partial compiled-seed-key? comp-state) deps-vals)

      ;; If yes, we add it...
      (add-to-compile comp-state
                      seed-key) ;; <-- ...with the order.
      comp-state))) ;; Otherwise we do nothing.

(defn scan-referents-to-compile [comp-state]
  (let [seed-key (access-seed-key comp-state)
        refs (->> comp-state
                  seed-map
                  seed-key
                  referents
                  (map second) ;; <-- Keyword of the seeds
                  )]
    (reduce try-add-to-compile comp-state refs)))

(defn compile-seed-at-key [comp-state seed-key cb]
  (when debug-seed-names
    (println "compile-seed-at-key" seed-key))
  (let [comp-state (initialize-seed-compilation
                    comp-state seed-key)]
    (compile-seed
     comp-state
     (seed-at-key comp-state seed-key)
     (fn [comp-state]
       (-> comp-state
           put-result-in-seed
           maybe-bind-result
           scan-referents-to-compile
           cb)))))

;; The typesignature of the underlying exprssion
(def seed-typesig (party/key-accessor ::seed-typesig))

(defn preprocess-subexpr [expr]
  (-> expr
      to-seed))

;; Preprocess every seed inside
;; But don't assign keys
(defn preprocess [expr]
  (second
   (utils/traverse-postorder-cached
    {}
    expr
    {:visit preprocess-subexpr
     :access-coll access-seed-coll})))

(defn generate-seed-key [seed]
  (keyword (gensym (description seed))))

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
   (utils/traverse-postorder-cached
    {}
    expr
    {:visit generate-seed-key
     :access-coll access-seed-coll})))

(defn replace-deps-by-keys
  [src]
  "Replace deps by keys"
  (let [expr2key (:expr2key src)]
    (into
     {}
     (map (fn [[expr key]]
            [key
             (party/update
              expr
              flat-deps
              (fn [fd]
                (map (partial get expr2key) fd)))])
          expr2key))))

(defn add-referent [referent dst-map [ref-key dep-key]]
  (update dst-map
          dep-key
          (fn [dst-seed]
            (party/update
             dst-seed
             referents
             (fn [dst-deps-map]
               (conj (specutils/validate
                      ::referents dst-deps-map)
                     [ref-key referent]))))))

(defn accumulate-referents [dst-map [k seed]]
  (assert (keyword? k))
  (assert (seed? seed))
  (assert (map? dst-map))
  (reduce (partial add-referent k) dst-map (access-deps seed)))

(defn compute-referents [m]
  (assert (map? m))
  (reduce accumulate-referents m m))

(def access-top (party/key-accessor ::top))


(defn top-seed [comp-state]
  (get (seed-map comp-state)
       (access-top comp-state)))

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

(defn traverse-expr-map-sub [dst expr-map at settings]
  (let [seed (seed-at-key expr-map at)]
    (if (or (contains? dst at)
            (not ((:visit?-fn settings) seed)))
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
  (assert (keyword? start))
  (assert (fn? get-neighbours-of-seed-fn))
  (assert (fn? visit?-fn))
  (traverse-expr-map-sub
   #{}
   expr-map
   start
   {:neigh get-neighbours-of-seed-fn
    :visit?-fn visit?-fn}))

(def always-visit (constantly true))

(defn deep-seed-deps [expr-map seed-key]
  (traverse-expr-map
   expr-map
   seed-key
   dep-neighbours
   always-visit))

(defn update-seed [expr-map key f]
  (assert (keyword? key))
  (assert (fn? f))
  (party/update
   expr-map
   seed-map
   (fn [sm]
     (assert (contains? sm key))
     (update sm key f))))

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
                             access-deps
                             vals
                             set)
           to-add (clojure.set/difference
                   extra-deps existing-deps)]
       (add-deps
        seed
        (into {}
              (map (fn [d] [(keyword (gensym label)) d])
                   to-add)))))))

(defn expr-map-sub
  "The main function analyzing the expression graph"
  [raw-expr]
  (let [lookups (-> raw-expr
                    preprocess
                    build-key-to-expr-map)
        top-key (:top-key lookups)
        ]
    (seed-map             ;; Access the seed-map key
     (access-top {} top-key) ;; Initial map
     (-> lookups
         replace-deps-by-keys
         compute-referents))

    ;; Post computations on the full map
    ))

(defn perform-pretweak [expr-map [k seed]]
  (if (pretweak? seed)
    ((access-pretweak seed) expr-map k seed)
    expr-map))

(defn perform-pretweaks [expr-map]
  (reduce
   perform-pretweak
   expr-map
   (-> expr-map seed-map)))

(defn expr-map [raw-expr]
  (-> (expr-map-sub raw-expr)
      perform-pretweaks))

(def default-omit-for-summary #{::omit-for-summary ::compiler})

;; Just for debugging, to understand how the expression got parsed.
(defn summarize-expr-map [expr-map]
  (party/update
   expr-map
   seed-map
   (fn [m]
     (into
      {}
      (map (fn [[k v]]
             (assert (seed? v))
             (let [all-keys (set (keys v))
                   keys-to-keep (clojure.set/difference
                                 all-keys
                                 (clojure.set/union
                                  default-omit-for-summary
                                  (set (omit-for-summary v))))]
               [k (select-keys v keys-to-keep)]))
           m)))))

(defn disp-expr-map [m]
  (-> m
      summarize-expr-map
      pp/pprint))

(defn seed-map-roots
  "Get the root seeds of the seed-map, which is where we start."
  [m]
  (filter
   (fn [[k v]]
     (empty? (access-deps v)))
   m))

(defn expr-map-roots [m]
  (-> m
      seed-map
      seed-map-roots))

(defn initial-set-to-compile [m]
  (set (map (fn [[k v]]
              k)
            (expr-map-roots m))))

(defn initialize-compilation-state [m]

  ;; Decorate the expr-map with a few extra things
  (-> m

      ;; Initialize a list of things to compile: All nodes that don't have dependencies
      (access-to-compile (initial-set-to-compile m))

      ;; Initialize the bindings, empty.
      (access-bindings [])))

(defn keep-keys-in-refs [seed ks]
  (party/update seed referents (fn [r] (debug/dout (filter (fn [[k v]] (contains? ks v)) r)))))

(defn keep-keys-and-referents [m ks]
  (transduce
   (comp (filter (fn [[k v]] (contains? ks k)))
         (map (fn [[k v]] [k (keep-keys-in-refs v ks)])))
   conj
   {}
   m))

(defn select-sub-tree [comp-state k]
  (let [dd (deep-seed-deps comp-state k)]
    (-> comp-state
        (party/update seed-map #(keep-keys-and-referents % dd))
        (access-top k))))

(defn pop-key-to-compile
  "Returns the first key to compile, and the comp-state with
that key removed"
  [comp-state]
  (let [to-comp (access-to-compile comp-state)
        f (first to-comp)
        r (disj to-comp f)]
    (when debug-seed-order
      (println "Popped" f))
    [f (access-to-compile comp-state r)]))


(defn compile-until [pred? comp-state cb]
  (if (pred? comp-state)
    (cb comp-state)

    ;; Otherwise, continue recursively
    (let [[seed-key comp-state] (pop-key-to-compile comp-state)]

      ;; Compile the seed at this key.
      ;; Bind result if needed.
      (compile-seed-at-key
       comp-state
       seed-key

       ;; Recursive callback.
       #(compile-until pred? % cb)))))

(defn compile-graph-sub
  "Loop over the state"
  [comp-state cb]
  (compile-until
   (comp empty? access-to-compile)
   comp-state
   cb))

(defn terminate-return-expr
  "Return the compilation result of the top node"
  [comp-state]
  (flush-bindings
   comp-state
   #(-> %
        top-seed
        compilation-result)))

(defn compile-graph [m terminate]
  (compile-graph-sub
   (initialize-compilation-state m)
   terminate))

(defn compile-full
  "Main compilation function. Takes a program datastructure and returns the generated code."
  [expr terminate]
  (-> expr
      expr-map
      (compile-graph terminate)))

(defn compile-top [expr]
  (compile-full expr terminate-return-expr))

(defn compile-terminate-snapshot [comp-state expr cb]
  (debug/TODO)
  (cb comp-state))

(defn terminate-snapshot [ref-dirty snapshot]
  (if (= (last-dirty snapshot)
         ref-dirty)
    (result-value snapshot)

    ;; Create a new seed that depends on both the result value
    ;; and the dirty, and compile to the result value.
    (let [x (result-value snapshot)]
      (-> (initialize-seed "terminate-snapshot")
          (add-deps {:value x})
          (set-dirty-dep (last-dirty snapshot))
          (compiler compile-terminate-snapshot)
          (datatype (datatype x))))))









(defmacro inject
  "Inject lime code, given some context."
  [[context] & expr]
  (with-context [(eval context)]
    ;; 1. Evaluate the type system, we need its value during compilation.
    

    ;; 3. Given the expression tree, analyze and compile it to code,
    ;; returned from this macro.
    (compile-top

     (terminate-snapshot
      nil
      (record-dirties-fn nil ;; Capture all effects
                         
                         ;; 2. Evaluate the expression: It is just code
                         ;; and the result is an expression tree
                         #(eval `(do ~@expr)))))))


;;;;;;;;;;;;;;;;;;;;;;;;; most common types
(defn compile-forward [comp-state expr cb]
  (let [k (-> expr
              access-deps
              :main)]
    (compilation-result
     comp-state
     (-> comp-state
         seed-map
         k
         compilation-result))))

(defn disp-deps [x]
  (println "DEPS:" (-> x access-deps keys))
  x)

(defn indirect
  "Every problem can be solved with an extra level of indirection, or something like that, it says, right?"
  [x]
  (-> (initialize-seed "indirect")
      (add-deps {:indirect x})
      (compiler compile-forward)
      (datatype (-> x
                    to-seed
                    datatype))
      ;(disp-deps)
      ))




(def wrapped-function (party/key-accessor :wrapped-function))

(defn compile-wrapfn [comp-state expr cb]
  (cb
   (compilation-result
    comp-state
    `(~(wrapped-function expr)
      ~@(lookup-compiled-indexed-results comp-state expr)))))

(def default-wrapfn-settings {:pure? false})

(defn wrapfn-sub [f settings0] ;; f is a quoted symbol
  (let [settings (merge default-wrapfn-settings settings0)
        dirtify (if (:pure? settings) identity dirty)]
    (fn [& args]
      (-> (initialize-seed "wrapped-function")
          (access-indexed-deps args)
          (wrapped-function f)
          (datatype dynamic-type)
          (compiler compile-wrapfn)
          dirtify))))

(defmacro wrapfn ;; Macro, because we want the symbol (or expr) of the function.
  "Make a wrapper around a function so that we can call it in lime"
  ([fsym settings0]
   `(wrapfn-sub (quote ~fsym) ~settings0))
  ([fsym] `(wrapfn ~fsym {})))

(defmacro wrapfn-pure [f]
  `(wrapfn ~f {:pure? true}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form

(defn identify-this-req [tag refs]
  (first
   (filter (fn [[dep-key seed-key]]
             (and (spec/valid? ::requirement dep-key)
                  (= tag (requirement-tag dep-key))))
           refs)))


(defn dep-tag-and-key [[k v]]
  (if (spec/valid? ::requirement k)
    [requirement-tag v]))

(defn depends-on-bifurcation? [seed tag bif-key]
  (assert (seed? seed))
  (assert (contains? #{:true-branch :false-branch} tag))
  (assert (keyword? bif-key))
  (let [look-for [tag bif-key]]
    (->> seed
         access-deps
         (filter #(= (dep-tag-and-key %) look-for))
         first
         empty?)))

(defn depends-on-this-bifurcation?
  "JUST FOR DEBUGGING"
  [bif]
  (fn [seed]
    (or (depends-on-bifurcation? seed :true-branch bif)
        (depends-on-bifurcation? seed :false-branch bif))))

(defn compile-bifurcate [comp-state expr cb]
  (let [refs (-> comp-state
                 access-seed-to-compile
                 referents)
        this-key (access-seed-key comp-state)]

    ;; Optional sanity checks.
    (if debug-check-bifurcate
      (let [all-seeds (map #(seed-at-key comp-state (second %)) refs)]

        ;; There should be seeds depending on the bifurcation,
        ;; at least one per branch (true and false)
        (assert (<= 2 (count all-seeds)))

        ;; All referents depend on this bifurcation
        (assert (every? (depends-on-this-bifurcation? this-key)
                        all-seeds))

        ;; We should always start with the true branch
        (let [key (pop-key-to-compile comp-state)
              first-seed-to-compile (seed-at-key
                                     comp-state
                                     key)]
          (println "key=" key)
          (pp/pprint first-seed-to-compile)
          (assert (depends-on-bifurcation? first-seed-to-compile
                                           :true-branch
                                           this-key)))))
    
    (debug/TODO "Compile the two bifurcations until branch termination. Generate code")
    ;; compile-until the termination node is reachable.
    (cb comp-state)))



;;;;;; NOTE: A bifurcation should depend on all seeds that:
;;  (i) Are always compiled
;;  (ii) Used by any of the branches
;;
;;  When it compiles a branch, it should limit the scope to the seeds
;;  under the indirection for every branch.

(defn filter-referents-of-seed [seed pred]
  (set
   (filter
    identity
    (map (fn [[k v]]
           (if (pred k) v)) ;;
         (referents seed)))))

(defn dep-tagged? [x]
  (fn [y]
    (and (vector? y)
         (= (first y) x))))

(defn tweak-bifurcation [expr-map key seed]
  (let [refs (referents seed)
        term (first (filter-referents-of-seed seed (partial = :bifurcation)))

        true-refs (filter-referents-of-seed seed (dep-tagged? :true-branch))
        false-refs (filter-referents-of-seed seed (dep-tagged? :false-branch))

        ;; All deep dependencies of the if-termination
        sub-keys (set (deep-seed-deps expr-map term))

        ;; All referent keys of the referents
        ref-keys (->> refs
                      (map second)
                      set)

        ;; All nodes that the if-terminator depends on
        ;; and that were not generated as part of the if.
        what-bif-should-depend-on (clojure.set/difference
                                   sub-keys (clojure.set/union
                                             ref-keys
                                             #{term key}))]
    
    (assert (not (empty? true-refs)))
    (assert (not (empty? false-refs)))
    
    ;; At least the two branches and the bifurcation
    (assert (not (empty? sub-keys)))

    ;; At least the two branches and the bifurcation
    (assert (not (empty? ref-keys)))
    (assert term)

    (-> expr-map
        (update-seed key (fn [x] (assoc x :seed-sets {:true-refs true-refs
                                                      :false-refs false-refs
                                                      :term term})))
        (add-expr-map-deps "eval-outside-if"
                           key
                           what-bif-should-depend-on))))

(defn bifurcate-on [condition]
  (-> (initialize-seed "if-bifurcation")
      (add-deps {:condition condition})
      (add-tag :bifurcation)
      (access-pretweak tweak-bifurcation)
      (compiler compile-bifurcate)))

(defn compile-if-termination [comp-state expr cb]
  (cb comp-state))

(defn if-sub [bif
              input-dirty
              on-true-snapshot
              on-false-snapshot]
  (assert (snapshot? on-true-snapshot))
  (assert (snapshot? on-false-snapshot))

  ;; The if-termination is mainly just an artificial construct
  ;; in the code graph. It is needed for the sake of structure. But
  ;; it does not result in any code. It's the bifurcation that takes
  ;; care of code generation
  (let [termination (-> (initialize-seed "if-termination")
                        (compiler compile-if-termination)
                        (add-tag :if-termination)
                        (add-deps {

                               :bifurcation bif
                                   
                               ;; We terminate each snapshot so that we
                               ;; have a single seed to deal with.
                               :true-branch
                               (terminate-snapshot input-dirty on-true-snapshot)
                               :false-branch
                               (terminate-snapshot input-dirty on-false-snapshot)}))

        ;; Wire the correct return dirty: If any of the branches produced a new dirty,
        ;; it means that this termination node is dirty.
        ;;
        ;; Otherwise, just return the input dirty
        output-dirty (if (= #{input-dirty}
                            (set [(last-dirty on-true-snapshot)
                                  (last-dirty on-false-snapshot)]))
                       termination
                       input-dirty)]

    ;(debug/TODO "If complex return value, generate equivalent datastructure of symbols")

    (-> {}
        (result-value termination)
        (last-dirty output-dirty))))

(defn indirect-if-branch [x]
  (-> x
      indirect ;; An extra, top-level-node for the branch
      ))

(defmacro If [condition true-branch false-branch]

  ;; We wrap it inside ordered, so that we compile things
  ;; in the same order as they were generated. This is to
  ;; avoid having code compiled inside an if-form when
  ;; it should not.
  `(let [bif# (bifurcate-on ~condition)]
     
     (inject-pure-code
      
      [d#] ;; <-- This is the last dirty, that we will feed to every branch to depend on.
      
      (if-sub ;; Returns the snapshot of a terminator.

       bif#
       
       d#     ;; We compare against this dirty.

       ;; For every branch, all its seed should depend on the bifurcation

       (with-requirements [[:true-branch bif#]]
         (record-dirties d# (indirect-if-branch ~true-branch)))

       (with-requirements [[:false-branch bif#]]
         (record-dirties d# (indirect-if-branch ~false-branch)))))))
