(ns lime.core
  (:require [bluebell.utils.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]
            [clojure.pprint :as pp]
            [bluebell.utils.debug :as debug]))

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

(defn initialize-state []
  (atom {::last-dirty nil
         ::requirements []
         ::dirty-counter 0}))

(def ^:dynamic state nil)

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

(defn with-requirements [r f]
  (assert (fn? f))
  (let [initial-reqs (-> state deref requirements)
        new-reqs (swap! state (partial append-requirements r))
        result (f)
        old-reqs (swap! state #(requirements % initial-reqs))]
    result))

;; Associate the requirements with random keywords in a map,
;; so that we can merge it in deps.
(defn make-req-map []
  (into {} (map (fn [x] [(keyword (gensym "req")) x])
                (-> state deref requirements))))


;; Special access to a dirty, if any
(def dirty (party/key-accessor ::dirty))


;; The dependencies of a seed
(def deps (party/key-accessor ::deps))

;; The opposite of deps
(def referents (party/key-accessor ::referents))

;; The compiler of a seed
(def compiler (party/key-accessor ::compiler))

;; Access the datatype of the seed
(def datatype (party/key-accessor ::type))

(def description (party/key-accessor ::description))

;; Create a new seed, with actual requirements
(defn initialize-seed [desc]
  (assert (string? desc))
  (-> {}
      (deps (make-req-map))
      (referents {})
      (compiler nil)
      (datatype nil)
      (omit-for-summary [])
      (description desc)))

;; Extend the deps map
(defn add-deps [dst extra-deps]
  (party/update dst deps #(merge % extra-deps)))


(def dirty-dep (party/key-accessor ::dirty {:req-on-get false}))

;; Access the last dirty in the deps map
(def last-dirty-dep (party/chain deps dirty-dep))

(defn set-dirty-dep [dst x]
  (if (dirty? x)
    (last-dirty-dep dst x)
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
(defn record-dirties [initial-dirty f]
  (let [start-state (swap! state #(replace-dirty % initial-dirty))
        out (f)
        restored-state (swap! state #(replace-dirty % (backup-dirty start-state)))]
    (-> {}
        (result-value out)
        (last-dirty (backup-dirty restored-state)))))

;; The opposite of the above: f gets as input the last dirty,
;; and then it returns a snapshot with the result and
;; the new dirty that we'd like to use after this.
(defn inject-pure-code [f]
  (let [current-state (deref state)
        snapshot (f (last-dirty current-state))]
    (assert (snapshot? snapshot))
    (swap! state #(last-dirty % (last-dirty snapshot)))
    (result-value snapshot)))

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
                         (party/chain deps utils/map-vals-accessor)

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
(def access-indexed-deps (party/chain deps access-indexed-map))

(defn lookup-compiled-results [state arg-map]
  (assert (map? arg-map))
  (let [m (seed-map state)]
    (into {} (map (fn [[k v]]
                    [k (compilation-result (get m v))]) arg-map))))

;; Compiler for the coll-seed type
(defn compile-coll [state expr cb]
  (cb (compilation-result
       state
       (utils/normalized-coll-accessor
        (access-original-coll expr)
        (access-indexed-map
         (lookup-compiled-results state (deps expr)))))))

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


(def flat-deps (party/chain deps utils/map-vals-accessor))

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

(def access-seed-key (party/key-accessor ::seed-key))

(defn put-result-in-seed [comp-state]
  (update-comp-state-seed
   comp-state
   (access-seed-key comp-state) 
   #(compilation-result % (compilation-result comp-state))))

(defn initialize-seed-compilation [comp-state seed-key]
  (access-seed-key
   (clear-compilation-result comp-state)
   seed-key))

(defn compile-seed-at-key [comp-state seed-key cb]
  (let [comp-state (initialize-seed-compilation
                    comp-state seed-key)]
    (compile-seed
     comp-state
     (seed-at-key comp-state seed-key)
     (fn [comp-state]
       (-> comp-state
           put-result-in-seed
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
               (assoc dst-deps-map
                      ref-key referent))))))

(defn accumulate-referents [dst-map [k seed]]
  (assert (keyword? k))
  (assert (seed? seed))
  (assert (map? dst-map))
  (reduce (partial add-referent k) dst-map (deps seed)))

(defn compute-referents [m]
  (assert (map? m))
  (reduce accumulate-referents m m))

(def access-top (party/key-accessor ::top))

(defn expr-map
  "The main function analyzing the expression graph"
  [raw-expr]
  (let [lookups (-> raw-expr
                    preprocess
                    build-key-to-expr-map)
        top-key (:top-key lookups)
        ]
    (seed-map ;; Access the seed-map key
     (access-top {} top-key) ;; Initial map
     (-> lookups
         replace-deps-by-keys
         compute-referents))))

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
  (set
   (filter
    (fn [[k v]]
      (empty? (deps v)))
    m)))

(defn expr-map-roots [m]
  (-> m
      seed-map
      seed-map-roots))

(def access-to-compile (party/key-accessor ::to-compile))

(defn add-to-compile [dst x]
  (party/update dst access-to-compile #(conj % x)))

(def access-bindings (party/key-accessor ::bindings))

(defn initialize-compilation-state [m]

  ;; Decorate the expr-map with a few extra things
  (-> m
      
      ;; Initialize a list of things to compile: All nodes that don't have dependencies
      (access-to-compile
       (mapv first (expr-map-roots m)))

      ;; Initialize the bindings, empty.
      (access-bindings [])))

(defn pop-key-to-compile
  "Returns the first key to compile, and the comp-state with
that key removed"
  [comp-state]
  (let [to-comp (access-to-compile comp-state)]
    
    [(first to-comp)
     (access-to-compile comp-state (rest to-comp))]))



(defn compile-graph-sub
  "Loop over the state"
  [comp-state cb]
  (if (empty? (access-to-compile comp-state))
    (cb comp-state)

    ;; Otherwise, continue recursively
    (let [[seed-key comp-state] (pop-key-to-compile comp-state)]

      ;; Compile the seed at this key.
      ;; Bind result if needed.
      (compile-seed-at-key
       comp-state
       seed-key

       ;; Recursive callback.
       #(compile-graph-sub % cb)))))

(defn terminate-return-expr
  "Return the compilation result of the top node"
  [comp-state]
  (println "Final compilation state")
  (pp/pprint comp-state)
  (compilation-result
   (seed-map comp-state
             (access-top comp-state))))

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







(defmacro inline
  "Inject lime code, given some context."
  [[context] & expr]
  (binding [evaluation-context (eval context)]
    ;; 1. Evaluate the type system, we need its value during compilation.
    

    ;; 3. Given the expression tree, analyze and compile it to code,
    ;; returned from this macro.
    (compile-top

     (record-dirties nil ;; Capture all effects
                     
                     ;; 2. Evaluate the expression: It is just code
                     ;; and the result is an expression tree
                     (eval `(do ~@expr))))))


;;;;;;;;;;;;;;;;;;;;;;;;; most common types
(defn compile-wrapfn [state expr cb]
  (cb))

(def wrapped-function (party/key-accessor :wrapped-function))

(def default-wrapfn-settings {:pure? false})

(defn wrapfn
  "Make a wrapper around a function so that we can call it in lime"
  ([f settings0]
   (let [settings (merge default-wrapfn-settings settings0)
         dirtify (if (:pure? settings) identity dirty)]
     (fn [& args]
       (-> (initialize-seed "wrapped-function")
           (access-indexed-deps args)
           (wrapped-function f)
           (datatype dynamic-type)
           (compiler compile-wrapfn)
           dirtify))))
  ([f] (wrapfn f {})))

(defn wrapfn-pure [f]
  (wrapfn f {:pure? true}))

