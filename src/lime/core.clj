(ns lime.core
  (:require [bluebell.utils.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]
            [clojure.pprint :as pp]
            [clojure.string :as cljstr]
            [bluebell.utils.debug :as debug]
            [clojure.spec.test.alpha :as stest]
            [lime.debug :refer [set-inspector inspect inspect-expr-map]]
            [bluebell.utils.specutils :as specutils]
            [bluebell.utils.trace :as trace]
            [lime.core.defs :as defs]
            [lime.platform.core :as cg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Definitions and specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Phases:
;;
;;  - The user builds a nested datastructure, where some values are seeds
;;      NOTE:
;;        - Symbols represent unknown values
;;  - We traverse the datastructure, and every seed becomes a seed
;;  - We remap the datastructure, assigning a symbol to every seed.
;;  - We build a graph
;;  - We traverse the graph from the bottom, compiling everything.


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
(def ^:dynamic debug-full-graph false)
(def ^:dynamic with-trace true)

;;;;;;;;;;;;; Tracing
(def trace-map (atom {}))

(defn deref-if-not-nil [x]
  (if (nil? x) x (deref x)))

(defn the-trace [] (-> state deref-if-not-nil :trace))

(defn begin [value]
  (trace/begin (the-trace) value))

(defn end [value]
  (trace/end (the-trace) value))

(defn record [value]
  (trace/record (the-trace) value))

;;;;;;;;;;;;;;;;;;


(def compilation-result (party/key-accessor ::compilation-result))

(defn clear-compilation-result [comp-state]
  (dissoc comp-state ::compilation-result))

(def seed-map (party/chain
               (party/key-accessor ::seed-map)))

(def empty-comp-state {:platform :clojure
                       ::seed-map {}})

;;;;;;;;;;;;;;;;;;;,
;; State used during meta-evaluation

(def compile-everything (constantly true))

(spec/def ::platform any?)

;; record a trace?
(spec/def ::trace-key keyword?)
(spec/def ::base-init (spec/keys :opt-un [::trace-key ::platform]))

(defn add-trace-if-requested [state]
  (if (contains? state :trace-key)
    (assoc state :trace (trace/trace-fn))
    state))

(defn post-init-state [state]
  (-> state
      add-trace-if-requested))

(def state-defaults {:platform :clojure
                     :disp-total-time? false})

(defn initialize-state [base-init]
  (atom (post-init-state
         (merge
          state-defaults
          base-init
          {::last-dirty nil ;; <-- last dirty generator
           ::requirements [] ;; <-- Requirements that all seeds should depend on
           ::dirty-counter 0 ;; <-- Used to generate a unique id for every dirty
           }))))

(defmacro with-context [[eval-ctxt]& args]
  `(binding [state (initialize-state ~eval-ctxt)]
     ~@args))

(spec/def ::seed (spec/keys :req [::type
                                  ::compiler
                                  ::deps]))

(spec/def ::basic-seed (spec/keys :req [::type]))

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

(def access-platform (party/key-accessor :platform))

;; Associate the requirements with random keywords in a map,
;; so that we can merge it in deps.
(defn make-req-map []
  (if (nil? state)
    {}
    (into {} (map (fn [x]
                    (specutils/validate ::requirement x)
                    [[(requirement-tag x)
                      (keyword (gensym "req"))]
                     (requirement-data x)])
                  (-> state deref requirements)))))

(defn get-platform []
  (if (nil? state)
    :clojure
    (access-platform (deref state))))

;; Special access to a dirty, if any
(def dirty (party/key-accessor ::dirty))


;; The dependencies of a seed
(def access-deps (party/key-accessor ::deps))

(def access-compiled-deps (party/key-accessor ::compiled-deps))

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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-tag [seed x]
  (party/update seed access-tags #(conj % x)))

(defn has-tag? [seed x]
  (contains? (access-tags seed) x))

(defn only-non-whitespace? [x]
  (->> x
      vec
      (map str)
      (not-any? cljstr/blank?)))

;; Create a new seed, with actual requirements
(defn initialize-seed [desc]
  (assert (only-non-whitespace? desc))
  (when debug-init-seed
    (println (str  "Initialize seed with desc '" desc "'")))
  (assert (string? desc))
  (-> {}
      (access-platform (get-platform))
      (access-deps (make-req-map))
      (access-tags #{})
      (referents #{})
      (compiler nil)
      (datatype nil)
      (defs/access-omit-for-summary [])
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
  (if (seed? x)
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

(defn only-numeric-keys [m]
  (filter (fn [[k v]] (number? k)) m))

;; Access a collection as indexed elements in a map
(defn access-indexed-map
  ([] {:desc "access-indexed-map"})
  ([x] (mapv second (sort-by first (only-numeric-keys x))))
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
      (defs/access-omit-for-summary #{:original-coll})
      (compiler compile-coll)))

(def static-value (party/key-accessor :static-value))

(defn value-literal-type [x]
  (if (symbol? x)
    defs/dynamic-type
    (class x)))

(defn compile-static-value [state expr cb]
  (cb (compilation-result state (cg/compile-static-value
                                 (access-platform state)
                                 (static-value expr)))))

(defn primitive-seed [x]
  (assert (not (coll? x)))
  (-> (initialize-seed "primitive-seed")
      (access-bind? false)
      (static-value x)
      (datatype (value-literal-type x))
      (compiler compile-static-value)))

;; Given a seed in the evaluated datastructure of a meta expression,
;; turn it into a seed.
(defn to-seed [x]
  (cond
    (seed? x) x
    (coll? x) (coll-seed x)
    :default (primitive-seed x)))



(defn to-type [dst-type x]
  (-> x
      to-seed
      (datatype dst-type)))

(defn to-dynamic [x]
  (to-type defs/dynamic-type x))


;;;;;; Analyzing an expression 
(defn access-no-deeper-than-seeds
  ([] {:desc "access-no-deeper-than-seeds"})
  ([x] (if (seed? x)
         []
         x))
  ([x y] (if (seed? x)
           x
           y)))

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
;; when traversing. Opposite of populate-seeds
(defn flatten-expr
  "Convert a nested expression to a vector of seeds"
  [x]
  (let [p (flat-seeds-traverse x identity)]
    (first p)))

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


(defn compiled-seed? [x]
  (contains? x ::compilation-result))

(defn compile-seed [state seed cb]
  (if (compiled-seed? seed)
    (cb (compilation-result state (compilation-result seed)))
    ((compiler seed) state seed cb)))

(defn seed-at-key [comp-state seed-key]
  (assert (map? comp-state))
  (-> comp-state
      seed-map
      (get seed-key)))

(def access-seed-key (party/key-accessor ::seed-key))

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


(spec/def ::invisible-tag (spec/or :eval-outside-if (spec/cat
                                                     :prefix (partial = :eval-outside-if)
                                                     :sym any?)))

(spec/def ::invisible-ref (spec/cat :tag ::invisible-tag
                                    :value any?))

(defn relevant-ref-for-bind? [r]
  (not (spec/valid? ::invisible-ref r)))

(defn bind-seed?
  "Determinate if a seed should be bound to a local variable"
  [seed]
  (let [refs0 (referents seed)
        explicit-bind (let [v (access-bind? seed)]
                        (if (fn? v)
                          (v seed)
                          v))
        refs (filter relevant-ref-for-bind? refs0)]
    (or
     (= true explicit-bind)
     (and
      (not= false explicit-bind)
      (or (dirty? seed)
          (< 1 (count refs)))))))

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
(def access-bind-symbol (party/key-accessor :bind-symbol))

(defn get-or-generate-hinted
  ([seed]
   (get-or-generate-hinted seed "sym"))
  ([seed name-prefix]
   (if (contains? seed :bind-symbol)
     (access-bind-symbol seed)
     (let [raw-sym (gensym name-prefix)
           hinted-sym (typehint (datatype seed) raw-sym)]
       hinted-sym))))

(defn maybe-bind-result
  ([comp-state]
   (maybe-bind-result comp-state (access-seed-key comp-state)))
  ([comp-state seed-key]
   (let [seed (-> comp-state
                  seed-map
                  seed-key)]
     (if-let [bind-suffix (bind-seed? seed)]
       (let [hinted-sym (get-or-generate-hinted seed (name seed-key))
             result (compilation-result seed)]
         (-> comp-state
             (compilation-result hinted-sym) ;; The last compilation result is a symbol
             (add-binding [hinted-sym result]) ;; Add it as a binding
             (update-comp-state-seed ;; Update the seed so that it has the symbol as result.
              seed-key #(compilation-result % hinted-sym))))
       
       ;; Do nothing
       comp-state))))

;;;;;;;;;;;;; TODO



(defn compiled-seed-key? [comp-state seed-key]
  (compiled-seed?
   (-> comp-state
       seed-map
       seed-key)))

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
    (if (and (not (compiled-seed? seed))
             (every? (partial compiled-seed-key? comp-state) deps-vals))

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

(defn get-compiled-deps [comp-state expr]
  (lookup-compiled-results
   comp-state (access-deps expr)))

(defn initialize-seed-to-compile [comp-state seed-key]
  (let [expr (seed-at-key comp-state seed-key)]
    (-> expr
        (access-compiled-deps (get-compiled-deps comp-state expr)))))

(defn compile-seed-at-key [comp-state seed-key cb]
  (when debug-seed-names
    (println "compile-seed-at-key" seed-key))
  (let [comp-state (initialize-seed-compilation
                    comp-state seed-key)]
    (compile-seed
     
     comp-state
     
     (initialize-seed-to-compile
      comp-state seed-key)
     
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
    dep-neighbours
    visit?)))

(defn update-seed [expr-map key f]
  (assert (keyword? key))
  (assert (fn? f))
  (party/update
   expr-map
   seed-map
   (fn [sm]
     (assert (contains? sm key))
     (update sm key f))))

(defn get-seed [expr-map key]
  (utils/data-assert (contains? (seed-map expr-map) key)
                     "No such seed with that key"
                     {:seed-key key})
  (-> expr-map
      seed-map
      key))

(defn labeled-dep [label]
  [(keyword label)
   (keyword (gensym label))])

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
              (map (fn [d] [(labeled-dep label) d])
                   to-add)))))))

(defn expr-map-sub
  "The main function analyzing the expression graph"
  [raw-expr]
  (let [lookups (-> raw-expr

                    (utils/first-arg (begin :preprocess))
                    preprocess
                    (utils/first-arg (end :preprocess))

                    (utils/first-arg (begin :key-to-expr-map))
                    build-key-to-expr-map
                    (utils/first-arg (end :key-to-expr-map))
                    
                    )
        top-key (:top-key lookups)
        ]
    (seed-map             ;; Access the seed-map key
     (access-top {} top-key) ;; Initial map
     (-> lookups

         (utils/first-arg (begin :replace-deps-by-keys))
         replace-deps-by-keys
         (utils/first-arg (end :replace-deps-by-keys))

         (utils/first-arg (begin :compute-referents))
         compute-referents
         (utils/first-arg (end :compute-referents))
         
         ))

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
  
  (-> raw-expr

      (utils/first-arg (begin :expr-map-sub))
      ;; Build the expr-map
      expr-map-sub
      (utils/first-arg (end :expr-map-sub))

      (utils/first-arg (begin :pretweaks))
      ;; Every seed can make adjustments to the graph
      perform-pretweaks
      (utils/first-arg (end :pretweaks))
            
      ;; After every seed has made adjustments, there may
      ;; be more referents to add.
      (party/update seed-map compute-referents)
      ))

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
             (assert (seed? v))
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

(def basic-inspect-expr (comp pp/pprint
                              summarize-expr-map
                              expr-map))

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

(defn compilation-roots [m]
  (filter
   (fn [[k v]]
     (and (not (compiled-seed? v))
          (every? (partial compiled-seed-key? m)
                  (-> v access-deps vals))))
   (seed-map m)))

(defn initial-set-to-compile [m]
  (set (map (fn [[k v]]
              k)
            (compilation-roots m))))

(defn initialize-compilation-roots [m]
  (access-to-compile m (initial-set-to-compile m)))

(defn keep-keys-in-refs [seed ks]
  (party/update seed referents (fn [r] (filter (fn [[k v]] (contains? ks v)) r))))

(defn keep-keys-and-referents [m ks]
  (transduce
   (comp (filter (fn [[k v]] (contains? ks k)))
         (map (fn [[k v]] [k (keep-keys-in-refs v ks)])))
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

(defn initialize-compilation-state [m]

  ;; Decorate the expr-map with a few extra things
  (-> m

      (access-platform (get-platform))

      (utils/first-arg (begin :initialize-compilation-state))

      ;; Initialize a list of things to compile: All nodes that don't have dependencies
      initialize-compilation-roots

      ;; Initialize the bindings, empty.
      (access-bindings [])

      (utils/first-arg (end :initialize-compilation-state))
      
      ))


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


(def ^:dynamic debug-compile-until false)

(defn compile-until [pred? comp-state cb]
  (if (pred? comp-state)
    (do debug-compile-until
        (cb comp-state)) 

    ;; Otherwise, continue recursively
    (let [[seed-key comp-state] (pop-key-to-compile comp-state)]
      (begin [:compile-until seed-key])
      ;; Compile the seed at this key.
      ;; Bind result if needed.
      (when debug-compile-until
        (println "To compile" (access-to-compile comp-state))
        (println "Compile seed with key" seed-key))
      (let [flag (atom false)
            result (compile-seed-at-key
                    comp-state
                    seed-key

                    ;; Recursive callback.
                    (fn [comp-state]
                      (end [:compile-until seed-key])
                      (reset! flag true)
                      (compile-until pred? comp-state cb)))]
        (utils/data-assert (deref flag)
                           "Callback not called"
                           {:seed-key seed-key})
        result))))

(spec/fdef compile-until :args (spec/cat :pred fn?
                                         :comp-state ::comp-state
                                         :cb fn?))

(defn compile-initialized-graph
  "Loop over the state"
  ([comp-state cb]
   (compile-until
    (comp empty? access-to-compile)
    comp-state
    cb)))

(defn terminate-return-expr
  "Return the compilation result of the top node"
  [comp-state]
  (flush-bindings
   comp-state
   #(-> %
        top-seed
        compilation-result)))

(defn terminate-last-result
  [comp-state]
  (flush-bindings
   comp-state
   #(compilation-result %)))

(defn check-all-compiled [comp-state]
  (doseq [[k v] (->> comp-state
                     seed-map)]
    (utils/data-assert (compiled-seed? v)
                       "There are seeds that have not been compiled. Cyclic deps?"
                       {:seed k}))
  comp-state)

(defn set-flag [flag]
  (fn [x]
    (reset! flag true)
    x))

(defn terminate-all-compiled-last-result [flag]
  (comp terminate-last-result
        check-all-compiled
        ;disp-and-return-expr-map
        (set-flag flag)))

(defn compile-graph [m terminate]
  (begin :inspect-expr-map)
  (if debug-full-graph
    (inspect-expr-map m))
  (end :inspect-expr-map)
  (compile-initialized-graph
   (initialize-compilation-state m)
   terminate))

(defn compile-full
  "Main compilation function. Takes a program datastructure and returns the generated code."
  [expr terminate]
  (-> expr
      expr-map
      (compile-graph terminate)))

(defn disp-trace [k]
  (let [tr-map (deref trace-map)]
    (if-let [tr (get tr-map k)]
      (trace/disp-trace tr)
      (println "No trace at key" k))))

(defn finalize-state []
  (let [value (deref state)]
    (when (contains? value :trace-key)
      (swap! trace-map #(assoc % (:trace-key value)
                               ((:trace value))))
      (println "You can inspect the trace with (disp-trace" (:trace-key value) ")"))))

(defn compile-top [expr]
  (let [terminated? (atom false)
        start (System/currentTimeMillis)
        _ (begin :compile-full)
        result (compile-full expr
                             (terminate-all-compiled-last-result
                              terminated?))
        _ (end :compile-full)
        end (System/currentTimeMillis)]
    (when (:disp-total-time? (deref state))
      (println (str "Compiled in " (- end start) " milliseconds")))
    (assert (deref terminated?))
    (finalize-state)
    result))

(defn compile-terminate-snapshot [comp-state expr cb]
  (let [results  (lookup-compiled-results
                  comp-state (access-deps expr))]
    (cb (compilation-result comp-state (:value results)))))

(defn terminate-snapshot [ref-dirty snapshot]
  (if (= (last-dirty snapshot)
         ref-dirty)
    (result-value snapshot)

    ;; Create a new seed that depends on both the result value
    ;; and the dirty, and compile to the result value.
    (let [x (to-seed (result-value snapshot))]
      (-> (initialize-seed "terminate-snapshot")
          (add-deps {:value x})
          (set-dirty-dep (last-dirty snapshot))
          (compiler compile-terminate-snapshot)
          (datatype (datatype x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Used when passing
(defn pack
  "Almost like flatten. Convert a nested expression to something that is easy to construct."
  [x]
  (let [k (flatten-expr x)]
    (cond
      (empty? k) nil
      (= 1 (count k)) (first k)
      :default k)))

(defn compile-unpack-element [comp-state expr cb]
  (let [i (specutils/validate number? (:index expr))]
    (cb (compilation-result
         comp-state
         `(nth ~(-> expr access-compiled-deps :arg)
               ~i)))))

(defn unpack-vector-element
  "Helper to unpack"
  [src-expr dst-type index]
  (-> (initialize-seed "Unpack-vector-element")
      (access-deps {:arg src-expr})
      (assoc :index index)
      (datatype (datatype dst-type))
      (compiler compile-unpack-element)))

(defn inherit-datatype [x from]
  (datatype x (datatype from)))

(defn unpack
  [dst x]
  (populate-seeds
   dst
   (let [flat-dst (flatten-expr dst)
         n (count flat-dst)]
     (cond
       (= 0 n) []
       (= 1 n) [(inherit-datatype x (first flat-dst))]
       :default (map (partial unpack-vector-element x)
                     flat-dst
                     (range n) )))))





;; Replaces 'inline'
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

(defmacro inspect-full
  "Inject lime code, given some context."
  [[context] & expr]
  (with-context [(eval context)]
    ;; 1. Evaluate the type system, we need its value during compilation.
    

    ;; 3. Given the expression tree, analyze and compile it to code,
    ;; returned from this macro.
    (terminate-snapshot
     nil
     (record-dirties-fn nil ;; Capture all effects
                        
                        ;; 2. Evaluate the expression: It is just code
                        ;; and the result is an expression tree
                        #(eval `(do ~@expr))))))

(defmacro get-expr-map
  "Inject lime code, given some context."
  [[context] & expr]
  (with-context [(eval context)]
    ;; 1. Evaluate the type system, we need its value during compilation.
    

    ;; 3. Given the expression tree, analyze and compile it to code,
    ;; returned from this macro.
    (terminate-snapshot
     nil
     (record-dirties-fn nil ;; Capture all effects
                        
                        ;; 2. Evaluate the expression: It is just code
                        ;; and the result is an expression tree
                        #(eval `(do ~@expr))))))


;;;;;;;;;;;;;;;;;;;;;;;;; most common types
(defn compile-forward [comp-state expr cb]
  (let [k (-> expr
              access-deps
              :indirect)]
    (assert (keyword? k))
    (cb
     (compilation-result
      comp-state
      (-> comp-state
          seed-map
          k
          compilation-result)))))

(defn disp-deps [x]
  (println "DEPS:" (-> x access-deps keys))
  x)


;; The reason for indirection is so that we can add dependencies,
;; in case we are not dealing with a seed. 
;; We can also use it to generate a local binding where we need it.
(defn indirect
  "Every problem can be solved with an extra level of indirection, or something like that, it says, right?"
  [x]
  #_(println "Indirect to")
  #_(debug/dout x)
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

(defn disp-deps [x]
  (println "The deps of the object are" (-> x access-deps keys))
  x)

(defn wrapfn-sub [label f settings0] ;; f is a quoted symbol
  (let [settings (merge default-wrapfn-settings settings0)
        dirtify (if (:pure? settings)
                  identity
                  dirty)]
    (fn [& args]
      (-> (initialize-seed (str "wrapped-function" ))
          (access-indexed-deps args)
          (wrapped-function f)
          (datatype defs/dynamic-type)
          (compiler compile-wrapfn)
          dirtify
          ;;disp-deps
          ))))

(defmacro wrapfn ;; Macro, because we want the symbol (or expr) of the function.
  "Make a wrapper around a function so that we can call it in lime"
  ([fsym settings0]
   (assert (symbol? fsym))
   `(wrapfn-sub
     ~(name fsym)
     (quote ~fsym)
     ~settings0))
  ([fsym] `(wrapfn ~fsym {})))

(defmacro wrapfn-pure [f]
  `(wrapfn ~f {:pure? true}))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   If form
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If-form
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

(defn mark-compiled [comp-state key-s]
  (if (keyword? key-s)
    (update-seed
     comp-state key-s
     (fn [s]
       (if (compiled-seed? s)
         s
         (compilation-result s [:marked-as-compiled key-s]))))
    (reduce mark-compiled comp-state key-s)))


(defn compile-to-expr [sub-tree]
  (compile-initialized-graph
   sub-tree
   terminate-last-result))

(defn codegen-if [condition true-branch false-branch]
  `(if ~condition ~true-branch ~false-branch))

;; Hidden result: The result of compiling the seed
;; and stored in the seed. Use this if the seed is
;; being compiled outside of the standard traversal.
;; When the seed is then compiled, it simply uses the hidden result.
(def access-hidden-result (party/key-accessor :hidden-result))
(defn has-hidden-result? [x]
  (contains? x :hidden-result))

(defn compile-bifurcate [comp-state expr cb]
  (flush-bindings
   comp-state
   (fn [comp-state]
     (let [r (lookup-compiled-results comp-state (access-deps expr))
           refs (-> comp-state
                    access-seed-to-compile
                    referents)
           this-key (access-seed-key comp-state)
           seed-sets (:seed-sets expr)
           true-top (:true-top seed-sets)
           false-top (:false-top seed-sets)
           comp-state (mark-compiled comp-state (:bif seed-sets))
           term (:term seed-sets)
           true-comp (compile-to-expr (select-sub-tree comp-state true-top))
           false-comp (compile-to-expr (select-sub-tree comp-state false-top))]
       (cb (-> comp-state
               (compilation-result :compiled-bifurcate)

               ;; Mark everything, except the termination, as compiled.
               (mark-compiled (disj (:term-sub-keys seed-sets) term))

               ;; Put the result in the term node
               (update-seed term #(access-hidden-result
                                   % (codegen-if
                                      (:condition r) true-comp false-comp)))
               
               initialize-compilation-roots))))))



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

(defn find-dep [seed pred]
  (first (filter-deps seed pred)))


(defn tweak-bifurcation [expr-map key seed]
  (let [;; All seeds referring to bifurcation
        refs (referents seed)

        ;; The termination seed key
        term (referent-with-key seed :bifurcation)

        ;; The termination seed
        term-seed (-> expr-map seed-map term)

        ;; The dependencies of the termination seed
        term-seed-deps (-> term-seed access-deps)

        ;; The top of the true/false branches
        true-top (-> term-seed-deps :true-branch)
        false-top (-> term-seed-deps :false-branch)

        ;; The sets of seed referring to the bifurcation from either branch
        true-refs (filter-referents-of-seed seed (dep-tagged? :true-branch))
        false-refs (filter-referents-of-seed seed (dep-tagged? :false-branch))

        ;; All deep dependencies of the if-termination
        term-sub-keys (set (deep-seed-deps expr-map term))

        bif-refs (traverse-expr-map
                  expr-map
                  key
                  referent-neighbours
                  (fn [[k _]] (contains? term-sub-keys k)))

        ;; All referent keys of the referents
        ref-keys (->> refs
                      (map second)
                      set)

        ;; All nodes that the if-terminator depends on
        ;; and that were not generated as part of the if.
        what-bif-should-depend-on (clojure.set/difference
                                   term-sub-keys (clojure.set/union
                                                  ref-keys
                                                  #{term key}
                                                  bif-refs))]

    (assert (keyword? true-top))
    (assert (keyword? false-top))
    
    (assert (not (empty? true-refs)))
    (assert (not (empty? false-refs)))
    
    ;; At least the two branches and the bifurcation
    (assert (not (empty? term-sub-keys)))

    ;; At least the two branches and the bifurcation
    (assert (not (empty? ref-keys)))
    (assert term)

    (-> expr-map
        (update-seed
         key
         (fn [x]
           (assoc x :seed-sets
                  {:bif key
                   :true-top true-top
                   :false-top false-top
                   :true-refs true-refs
                   :false-refs false-refs
                   :term-sub-keys term-sub-keys
                   :term term})))
        (add-expr-map-deps "eval-outside-if"
                           key
                           what-bif-should-depend-on))))

(defn bifurcate-on [condition]
  (-> (initialize-seed "if-bifurcation")
      (add-deps {:condition condition})
      (add-tag :bifurcation)
      (access-bind? false)
      (access-pretweak tweak-bifurcation)
      (compiler compile-bifurcate)))

(defn compile-if-termination [comp-state expr cb]
  (cb (compilation-result comp-state (access-hidden-result expr))))

(def access-original-type (party/key-accessor :original-type))

(def original-branch-type (comp access-original-type result-value))

(defn mark-dont-bind [x]
  (access-bind? x false))

(defn if-sub [settings
              bif
              input-dirty
              on-true-snapshot
              on-false-snapshot]
  (assert (snapshot? on-true-snapshot))
  (assert (snapshot? on-false-snapshot))

  ;; The if-termination is mainly just an artificial construct
  ;; in the code graph. It is needed for the sake of structure. But
  ;; it does not result in any code. It's the bifurcation that takes
  ;; care of code generation
  (let [unpacker (if (:pack? settings) unpack (fn [_ value] value))
        true-type (original-branch-type on-true-snapshot)
        false-type (original-branch-type on-false-snapshot)
        true-branch (terminate-snapshot input-dirty on-true-snapshot)
        false-branch (terminate-snapshot input-dirty on-false-snapshot)]
    (utils/data-assert (utils/implies
                        (:check-branch-types? settings)
                        (= true-type false-type)) "Different branch types"
                       {:true-branch true-type
                        :false-branch false-type})
    (let  [ret-type true-type
           
           branch-dirties (set [(last-dirty on-true-snapshot)
                                (last-dirty on-false-snapshot)])

           output-dirty? (not= #{input-dirty}
                               branch-dirties)
           
           termination (-> (initialize-seed "if-termination")
                           (compiler compile-if-termination)
                           (add-tag :if-termination)
                           (utils/cond-call (:dont-bind? settings) mark-dont-bind)
                           (add-deps
                            {

                             :bifurcation bif
                             
                             :true-branch (pack true-branch)
                             
                             :false-branch (pack false-branch)
                             })
                           (utils/cond-call output-dirty? dirty))

           _ (assert (= (boolean output-dirty?)
                        (boolean (dirty? termination))))

           ;; Wire the correct return dirty: If any of the branches produced a new dirty,
           ;; it means that this termination node is dirty.
           ;;
           ;; Otherwise, just return the input dirty


           output-dirty (if output-dirty?
                          termination
                          input-dirty)
           ret (-> {}
                   (result-value (unpacker ret-type termination))
                   (last-dirty output-dirty))]
      #_(debug/dout output-dirty?)
      ret)))

(defn indirect-if-branch [packer x]
  (let [tp (type-signature x)]
    (-> x
        packer
        indirect ;; An extra, top-level-node for the branch
        (access-original-type tp) ;; Decorate it with the type it holds
        )))
(def import-if-settings (utils/default-settings-fn {:pack? true
                                                    :check-branch-types? true
                                                    :dont-bind? false}))

(defn if-with-settings [settings0 condition true-branch false-branch]
  (let [settings (import-if-settings settings0)
        packer (if (:pack? settings) pack identity)]
    ;; We wrap it inside ordered, so that we compile things
    ;; in the same order as they were generated. This is to
    ;; avoid having code compiled inside an if-form when
    ;; it should not.
    `(let [bif# (bifurcate-on ~condition)]
       (inject-pure-code
        
        [d#] ;; <-- This is the last dirty, that we will feed to every branch to depend on.

        (if-sub ;; Returns the snapshot of a terminator.

         ~settings

         bif#
         
         d#     ;; We compare against this dirty.

         ;; For every branch, all its seed should depend on the bifurcation

         (with-requirements [[:true-branch bif#]]
           (record-dirties d# (indirect-if-branch ~packer ~true-branch)))

         (with-requirements [[:false-branch bif#]]
           (record-dirties d# (indirect-if-branch ~packer ~false-branch))))))))

(defmacro If [condition true-branch false-branch]
  (if-with-settings {:pack? true} condition true-branch false-branch))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Loop form
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-mask [mask v]
  (utils/data-assert (= (count mask)
                        (count v))
                     "Incompatible lengths"
                     {:mask (count mask)
                      :v (count v)})
  (map second
       (filter (fn [[m v]] m)
               (map vector mask v))))

(defn compile-bind [comp-state expr cb]
  (cb (compilation-result comp-state (access-bind-symbol expr))))

(defn make-loop-binding [comp-state lvar-key]
  (let [lvar (get-seed comp-state lvar-key)]
    [(access-bind-symbol lvar)
     (:value (get-compiled-deps comp-state lvar))]))



(defn replace-by-local-var [x0]
  (let [x (to-seed x0)]
    (-> (initialize-seed "local-var")
        (access-bind-symbol (get-or-generate-hinted x))
        (add-deps {:value x})
        (access-bind? false)
        (datatype (datatype x))
        (compiler compile-bind))))

(defn bind-if-not-masked [mask value]
  (if mask
    (replace-by-local-var value)        ;; <-- assign a symbol to it, and we are going to use it.
    (access-bind? (to-seed value) true) ;; <-- force it to be bound outside of the loop
    ))

(comment
  (defn tweak-bifurcation [expr-map key seed]
    (let [ ;; All seeds referring to bifurcation
          refs (referents seed)

          ;; The termination seed key
          term (referent-with-key seed :bifurcation)

          ;; The termination seed
          term-seed (-> expr-map seed-map term)

          ;; The dependencies of the termination seed
          term-seed-deps (-> term-seed access-deps)

          ;; The top of the true/false branches
          true-top (-> term-seed-deps :true-branch)
          false-top (-> term-seed-deps :false-branch)

          ;; The sets of seed referring to the bifurcation from either branch
          true-refs (filter-referents-of-seed seed (dep-tagged? :true-branch))
          false-refs (filter-referents-of-seed seed (dep-tagged? :false-branch))

          ;; All deep dependencies of the if-termination
          term-sub-keys (set (deep-seed-deps expr-map term))

          bif-refs (traverse-expr-map
                    expr-map
                    key
                    referent-neighbours
                    (fn [[k _]] (contains? term-sub-keys k)))

          ;; All referent keys of the referents
          ref-keys (->> refs
                        (map second)
                        set)

          ;; All nodes that the if-terminator depends on
          ;; and that were not generated as part of the if.
          what-bif-should-depend-on (clojure.set/difference
                                     term-sub-keys (clojure.set/union
                                                    ref-keys
                                                    #{term key}
                                                    bif-refs))]

      (assert (keyword? true-top))
      (assert (keyword? false-top))
      
      (assert (not (empty? true-refs)))
      (assert (not (empty? false-refs)))
      
      ;; At least the two branches and the bifurcation
      (assert (not (empty? term-sub-keys)))

      ;; At least the two branches and the bifurcation
      (assert (not (empty? ref-keys)))
      (assert term)

      (-> expr-map
          (update-seed
           key
           (fn [x]
             (assoc x :seed-sets
                    {:bif key
                     :true-top true-top
                     :false-top false-top
                     :true-refs true-refs
                     :false-refs false-refs
                     :term-sub-keys term-sub-keys
                     :term term})))
          (add-expr-map-deps "eval-outside-if"
                             key
                             what-bif-should-depend-on)))))

(defn tweak-loop [expr-map seed-key seed]
  (let [loop-binding-key (find-dep seed (partial = :loop-binding))
        term-key (referent-with-key seed :root)
        term-seed (-> expr-map seed-map term-key)
        term-sub-keys (set (deep-seed-deps expr-map term-key))
        root-refs (traverse-expr-map
                  expr-map
                  loop-binding-key
                  referent-neighbours
                  (fn [[k _]] (contains? term-sub-keys k)))


        eval-outside-loop (clojure.set/difference term-sub-keys
                                                  (set root-refs))]
    (-> expr-map
        (update-seed
         seed-key
         (fn [x]
           (assoc x :seed-sets
                  {:term key
                   :term-sub-keys term-sub-keys})))
        (add-expr-map-deps "eval-outside-loop"
                           loop-binding-key
                           eval-outside-loop))))

(comment
  (defn compile-bifurcate [comp-state expr cb]
    (flush-bindings
     comp-state
     (fn [comp-state]
       (let [r (lookup-compiled-results comp-state (access-deps expr))
             refs (-> comp-state
                      access-seed-to-compile
                      referents)
             this-key (access-seed-key comp-state)
             seed-sets (:seed-sets expr)
             true-top (:true-top seed-sets)
             false-top (:false-top seed-sets)
             comp-state (mark-compiled comp-state (:bif seed-sets))
             term (:term seed-sets)
             true-comp (compile-to-expr (select-sub-tree comp-state true-top))
             false-comp (compile-to-expr (select-sub-tree comp-state false-top))]
         (cb (-> comp-state
                 (compilation-result :compiled-bifurcate)

                 ;; Mark everything, except the termination, as compiled.
                 (mark-compiled (disj (:term-sub-keys seed-sets) term))

                 ;; Put the result in the term node
                 (update-seed term #(access-hidden-result
                                     % (codegen-if
                                        (:condition r) true-comp false-comp)))
                 initialize-compilation-roots)))))))

(def access-mask (party/key-accessor :mask))

(defn compile-loop-bindings [comp-state lvars]
  (reduce into [] (map (partial make-loop-binding comp-state) lvars)))

(defn compile-loop [comp-state seed cb]
  (flush-bindings
   comp-state
   (fn [comp-state]
     (let [deps (access-deps seed)
           loop-binding (get-seed comp-state (:loop-binding deps))
           lvars (access-indexed-deps seed)
           mask (access-mask seed)
           this-key (access-seed-key comp-state)
           term (referent-with-key seed :root)
           comp-state (mark-compiled comp-state #{this-key})
           term-subtree (select-sub-tree comp-state term)
           compiled-loop-body (compile-to-expr term-subtree)
           term-sub (set (deep-seed-deps comp-state term))
           this-result `(loop ~(compile-loop-bindings comp-state lvars)
                          ~compiled-loop-body)]
       (cb (-> comp-state
               (compilation-result this-result)
               (update-seed term #(access-hidden-result % this-result))
               (mark-compiled (disj term-sub term))
               initialize-compilation-roots))))))

(defn compile-loop-binding [comp-state expr cb]
  (cb (compilation-result comp-state :loop-binding)))

(defn loop-binding []
  (-> (initialize-seed "loop-binding")
      (compiler compile-loop-binding)
      (access-bind? false)))

(defn loop-root [loop-binding mask initial-state]
  (-> (initialize-seed "loop-root")
                                        ;(add-deps {:state initial-state})
      (access-indexed-deps (flatten-expr initial-state))
      (add-tag :loop-root)
      (add-deps {:loop-binding loop-binding})
      (access-bind? false)
      (access-mask mask)
      (access-pretweak tweak-loop)
      (compiler compile-loop)))

(def access-loop? (party/key-accessor :loop?))

(defn compile-loop-test-condition [comp-state expr cb]
  (cb comp-state))

(defn compile-recur [comp-state expr cb]
  (let [results (lookup-compiled-indexed-results comp-state expr)]
    (cb (compilation-result comp-state
                            `(recur ~@results)))))

(def recur-seed-type ::recur)

(defn recur-seed [x]
  (-> (initialize-seed "recur")
      (access-indexed-deps (flatten-expr x))
      (datatype recur-seed-type)
      (compiler compile-recur)))

(def access-state-type (party/key-accessor :state-type))


(defn compile-loop-termination [comp-state expr cb]
  (if (has-hidden-result? expr)
    (cb
     (compilation-result
      comp-state
      (access-hidden-result expr)))
    (let [rdeps (access-compiled-deps expr)]
      (cb (compilation-result
           comp-state
           (:if rdeps))))))


(defn remove-loop?-key [x]
  (dissoc x :loop?))

(defn prepare-return-value [x]
  (-> x
      remove-loop?-key
      pack
      to-seed))

(defn terminate-loop-snapshot [return-value
                               mask
                               root
                               input-dirty
                               loop-if-snapshot]
  (let [dirty-loop? (not= input-dirty (last-dirty loop-if-snapshot))

        ;; Build the termination node
        term (-> (initialize-seed "loop-termination")
                 (access-state-type (type-signature return-value))
                 (compiler compile-loop-termination)
                 (access-bind? has-hidden-result?) ;; It has a recur inside
                 (add-deps {;; Structural pointer at the beginning of the loop
                            :root root


                            :if (terminate-snapshot
                                 input-dirty
                                 loop-if-snapshot)})
                 (utils/cond-call dirty-loop? dirty))]

    ;; Build a snapshot
    (-> {}
        (result-value term)
        (last-dirty (if dirty-loop? term input-dirty)))))

(defn unpack-loop-result [x]
  (unpack (access-state-type x) x))

(defn active-loop-vars-mask [input-dirty initial-state eval-state-fn next-state-fn]
  (let [next-state (result-value
                    (record-dirties
                     input-dirty
                     (-> initial-state
                         eval-state-fn
                         remove-loop?-key
                         next-state-fn)))

        init-state-type (type-signature initial-state)
        next-state-type (type-signature next-state)]
    (utils/data-assert (= init-state-type next-state-type)
                       "The loop state types must be the same"
                       {:init-state-type init-state-type
                        :next-state-type next-state-type})
    (map not=
         (flatten-expr initial-state)
         (flatten-expr next-state))))

(defmacro if-loop [condition true-branch false-branch]
  (if-with-settings {:pack? false
                     :check-branch-types? false
                     :dont-bind? true}
                    condition
                    true-branch
                    false-branch))

(defn basic-loop
  ([initial-state0 eval-state-fn next-state-fn]
   (basic-loop initial-state0 eval-state-fn next-state-fn identity))
  ([initial-state0 eval-state-fn next-state-fn result-fn]
   (assert (fn? eval-state-fn))
   (assert (fn? next-state-fn))
   (unpack-loop-result
    (inject-pure-code
     [input-dirty]
     (let [
           ;; First compute a mask over active loop vars
           mask (active-loop-vars-mask input-dirty
                                       initial-state0
                                       eval-state-fn
                                       next-state-fn)

           binding (loop-binding)

           ;; Then use this mask to introduce local loop variables
           ;; for the active parts
           initial-state (with-requirements [[:loop-binding binding]]
                           (populate-seeds
                            initial-state0
                            (map bind-if-not-masked
                                 mask
                                 (flatten-expr initial-state0))))

           record-return-value (utils/atom-fn)
           
           ;; Now we can make our root.
           root (loop-root binding mask initial-state)
           loop-if-snapshot (with-requirements [[:if root]]
                              (record-dirties
                               input-dirty
                               (let [evaled (eval-state-fn initial-state)]
                                 
                                 (if-loop
                                  (access-loop? evaled)
                                  (recur-seed
                                   (apply-mask
                                    mask
                                    (-> evaled
                                        remove-loop?-key
                                        next-state-fn
                                        flatten-expr)))
                                  (-> evaled
                                      remove-loop?-key
                                      result-fn
                                      record-return-value
                                      prepare-return-value
                                      )))))]
       (terminate-loop-snapshot (record-return-value)
                                mask
                                root
                                input-dirty
                                loop-if-snapshot))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; TEST CODE WOKR IN PROGRESS
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Functions used to build tests
;;;;;
(def pure+ (wrapfn-pure +))
(def pure- (wrapfn-pure -))
(def pure* (wrapfn-pure *))
(def purediv (wrapfn-pure /))
(def pure< (wrapfn-pure <))
(def pure<= (wrapfn-pure <=))
(def pure= (wrapfn-pure =))
(def pure-inc (wrapfn-pure inc))
(def pure-dec (wrapfn-pure dec))
(def pure-not (wrapfn-pure not))
(def dirty+ (wrapfn +))
(def dirty- (wrapfn -))
(def dirty* (wrapfn *))
(def dirtydiv (wrapfn /))
(def dirty< (wrapfn <))
(def dirty<= (wrapfn <=))
(def dirty= (wrapfn =))
(def dirty-not (wrapfn not))
(def pure-first (wrapfn first))
(def pure-rest (wrapfn rest))
(def pure-empty? (wrapfn empty?))
(def atom-deref (wrapfn deref))

(defn my-basic-reduce [f init collection]
  (:result
   (basic-loop
    {:result init
     :coll collection}
    (fn [state]
      (merge state {:loop? (pure-not (pure-empty? (:coll state)))}))
    (fn [state]
      {:result (f (:result state)
                  (pure-first (:coll state)))
       :coll (pure-rest (:coll state))}))))

(defn my-basic-sum [x]
  (my-basic-reduce pure+
                   (to-dynamic 0)
                   (to-dynamic x)))

(defn atom-assoc-sub [dst key value]
  (swap! dst #(assoc % key value)))
(def atom-assoc (wrapfn atom-assoc-sub))

(defn atom-conj-sub [dst x]
  (swap! dst #(conj % x)))
(def atom-conj (wrapfn atom-conj-sub))


(defmacro acquire-expr-map [])

(defmacro debug-compilation [expr]
  `(with-context []
     (println "\n\n\n\n\n\n\n\n\n\n\n\n")
     (let [em# (expr-map ~expr)]
       (try 
         (compile-graph em# terminate-all-compiled-last-result)
         (catch Throwable e#
           (inspect-expr-map em#))))))

(defmacro debug-expr [expr]
  `(do
     (println "\n\n\n\n\n\n")
     (pp/pprint (macroexpand '(inject [] ~expr)))))

(defmacro inject-debug [& expr]
  `(binding [debug-full-graph true]
     (inject [] ~@expr)))

(defn fibonacci-step-sub [state]
  (swap! state (fn [{a :a b :b}]
                 {:a b
                  :b (+ a b)})))

(def fibonacci-step (wrapfn fibonacci-step-sub))

(defn stateful-looper []
  (let [mut (atom {:a 0
                   :b 1})]
    (inject
     []
     (basic-loop
      {:i (to-dynamic 0)}
      (fn [state]
        (assoc state :loop? (pure< (:i state) 10)))
      (fn [state]
        (fibonacci-step 'mut)
        (update state :i pure-inc))))
    (deref mut)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#_(macroexpand
 ' (inject []
           (basic-loop
            {:value (to-type defs/dynamic-type (to-seed 4))
             :product (to-type defs/dynamic-type (to-seed 1))} 
            (fn [x] (merge x {:loop?  (pure< 0 (:value x))}))
            (fn [x] {:value (pure-dec (:value x))
                     :product (pure* (:product x)
                                     (:value x))}))))
#_(debug/pprint-code
 (macroexpand
  '(inject
    []
    (my-basic-reduce pure+
                     (to-dynamic 0)
                     (to-dynamic [1 2 3 4 5])))))


;;;;; Att göra:
;;; 1. Avlusa my-basic-reduce:
;;;      - Loopen binds inte... OK
;;;      - Returvärdet packas inte. OK
;;; 2. Fixa bra if-form för loopen
;;; 3. Testa med
;;;     - Nästlade loopar (använd reduce för det?) OK
;;;     - Loopar som har sidoeffekter
;;; FIXA ALLA TODOs

;;; Avlusa: Vissa kanter ska ignoreras när vi bestämmer antalet referenser till ett seed.

(debug/TODO :done "Expressions referenced outside of loops should be bound even if they are only referenced once. But that is usually the case, because when we add the explicit dependency of the root on those expressions, they get referenced multiple times.")
(debug/TODO :done "We should use a good if-form in the loop")
(debug/TODO :sort-of-done
            "Certain kinds of dependencies should not change the reference counter "
            "Such as artificial dependies introduced by the structures (with-req...)"
            "Special dependencies from the control structures. "
            "Pay attention to things that *should* be bound outside"

            "Well now we have the mechanism in place...")

(debug/TODO :sort-of-done
            "Test the loop with lots of stateful things..."

            "See stateful-looper-test")

(debug/TODO :done
            "Possibility of applying a function to the state before returning it"

            "See with-return-value-fn-test")

(debug/TODO :done
            "Profile the code to reduce compilation time"

            "The time is pretty spread out. No part that is particularly slow. "
            "Use the {:trace-key ...} context to enable tracing")

(debug/TODO :done "Make it possible to initialize-seed without a state?")
(debug/TODO "Add support for static values")
(debug/TODO "If the condition in an if statement is static, then we can directly pick one branch")
(debug/TODO :ignore
            "If the condition in a loop is static, then we should either not loop, or loop forever. Maybe not so prioritized.")
(debug/TODO "Factor out a spec namespace with all core/specs.clj and convenient accessors.")
(debug/TODO "Factor out a seed namespace with the core/seed.clj related stuff.")
(debug/TODO "Consider factoring out a core/exprmap.clj namespace and related functions.")
