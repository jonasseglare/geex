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
            [lime.core.seed :as sd]
            [lime.platform.core :as cg]
            [lime.core.exprmap :as exm]
            [lime.core.loop :as looputils]))

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

(def ^:dynamic scope-state nil)

(def contextual-gensym defs/contextual-gensym)

(def contextual-genkey (comp keyword contextual-gensym))

(def contextual-genstring (comp str contextual-gensym))


;;; Pass these as arguments to utils/with-flags, e.g.
;; (with-context []
;;  (utils/with-flags [debug-seed-names debug-seed-order]
;;    (compile-full
;;     (pure+ (pure+ 1 2) (pure+ 1 2))
;;     terminate-return-expr)))

(def ^:dynamic debug-seed-names false)
(def ^:dynamic debug-init-seed false)
(def ^:dynamic debug-check-bifurcate false)
(def ^:dynamic debug-full-graph false)
(def ^:dynamic with-trace false)

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


;;;;;;;;;;;;;;;;;;;,
;; State used during meta-evaluation

(def compile-everything (constantly true))

;; record a trace?


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
          {::defs/last-dirty nil ;; <-- last dirty generator
           ::defs/requirements [] ;; <-- Requirements that all seeds should depend on
           ::defs/dirty-counter 0 ;; <-- Used to generate a unique id for every dirty
           ::defs/local-vars {} ;; <-- Map of pack-id and {:type tp :vars v}
           }))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Scope data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def scope-ref-tag :scope-ref)
(def bind-ref-tag :bind-ref)
(def sideeffect-tag :sideeffect)

(defn new-scope-state
  ([]
   {:parents #{}
    :seeds (atom #{})
    :ref-tag scope-ref-tag})
  ([old-state]
   (let [old-seeds (-> old-state
                       :seeds
                       deref)
         parents (if (empty? old-seeds)
                   (:parents old-state)
                   old-seeds)]
     {:parents parents
      :seeds (atom #{})
      :ref-tag scope-ref-tag})))

(def access-scope-ref (party/key-accessor :ref-tag))

(defmacro with-modified-scope-state [f & body]
  `(do
     (utils/data-assert (not (nil? scope-state))
                        "There must be a scope state"
                        {})
     (binding [scope-state (~f scope-state)]
       ~@body)))

(defmacro deeper-scope-state [& body]
  `(with-modified-scope-state
     new-scope-state
     ~@body))

(defmacro deeper-tagged-scope-state [ref-tag & body]
  `(with-modified-scope-state
     (comp #(access-scope-ref % ~ref-tag)
           new-scope-state)
     ~@body))



(defn register-scope-seed [x]
  (if (not (nil? scope-state))
    (swap! (:seeds scope-state) conj x))
  x)

(defn reset-scope-seeds [x]
  (assert (not (nil? scope-state)))
  (reset! (:seeds scope-state) x))

(defmacro with-context [[eval-ctxt]& args]
  `(binding [scope-state (new-scope-state)
             state (initialize-state ~eval-ctxt)
             defs/gensym-counter (atom 0)]
     ~@args))

(def recording? (party/key-accessor ::recording?))


;; Helper for with-requirements
(defn append-requirements [r s]
  (party/update s defs/requirements #(into % r)))

(defn with-requirements-fn [r f]
  (assert (fn? f))
  (let [initial-reqs (-> state deref defs/requirements)
        new-reqs (swap! state (partial append-requirements r))
        result (f)
        old-reqs (swap! state #(defs/requirements % initial-reqs))]
    result))

(defmacro with-requirements [r & body]
  `(with-requirements-fn
     ~r
     (fn [] ~@body)))





(defn make-explicit-req-map [state-value]
  (into {} (map (fn [x]
                  (specutils/validate ::defs/requirement x)
                  [[(defs/requirement-tag x)
                    (contextual-genkey "req")]
                   (defs/requirement-data x)])
                (-> state-value defs/requirements))))

(defn make-scope-req-map [tg parents]
  (zipmap
   (map (fn [p]
          [tg (contextual-genkey "scope")])
        parents)
   parents))

;; Associate the requirements with random keywords in a map,
;; so that we can merge it in deps.
(defn make-req-map [state-value]
  (merge (make-explicit-req-map state-value)
         (if (nil? scope-state)
           {}
           (make-scope-req-map
            (access-scope-ref scope-state)
            (:parents scope-state)))))

(defn get-platform []
  (if (nil? state)
    :clojure
    (defs/access-platform (deref state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn only-non-whitespace? [x]
  (->> x
      vec
      (map str)
      (not-any? cljstr/blank?)))

(defn nothing-compiler [comp-state expr cb]
  (cb (defs/compilation-result comp-state ::this-value-should-not-be-used)))

(defn compile-to-nothing [seed]
  (sd/compiler seed nothing-compiler))

;; Create a new seed, with actual requirements
(defn initialize-seed-sub [desc platform req-map]
  (utils/data-assert (only-non-whitespace? desc) "Bad seed descriptor"
                     {:desc desc})
  (when debug-init-seed
    (println (str  "Initialize seed with desc '" desc "'")))
  (assert (string? desc))
  (-> {}
      (defs/access-platform platform)
      (sd/access-deps req-map)
      (sd/access-tags #{})
      (sd/referents #{})
      (sd/compiler nil)
      (sd/datatype nil)
      (defs/access-omit-for-summary [])
      (sd/description desc)))



;; Extend the deps map

;; Call this function when a seed has been constructed,
;; but is side-effectful
(defn register-dirty-seed [state x]
  (defs/inc-counter
    (defs/last-dirty
      state
      (-> x
          (defs/dirty-counter (defs/dirty-counter state))
          (sd/set-dirty-dep (defs/last-dirty state))))))

(defn with-stateless-new-seed [desc f]
  (let [result-seed (f (initialize-seed-sub desc defs/default-platform {}))]
    (utils/data-assert (sd/seed? result-seed) "Not a valid seed" {:value result-seed})
    (utils/data-assert (not (sd/marked-dirty? result-seed))
                       "Seeds cannot not be dirty in a stateless setting"
                       {:value result-seed})
    result-seed))

(defn with-stateful-new-seed [desc f]
  (let [current-state (deref state)
        result-seed (f (initialize-seed-sub desc
                                            (defs/access-platform current-state)
                                            (make-req-map current-state)))]
    (if (sd/marked-dirty? result-seed)
      (defs/last-dirty (swap! state #(register-dirty-seed % result-seed)))
      result-seed)))

(defn with-new-seed [desc f]
  (register-scope-seed
   (if (nil? state)
     (with-stateless-new-seed desc f)
     (with-stateful-new-seed desc f))))



(defn replace-dirty [s new-dirty]
  (-> s
      (defs/backup-dirty (defs/last-dirty s))
      (defs/last-dirty new-dirty)))

;; Given an initial dirty, initialize the state
;; with that dirty, call (f) without any arguments,
;; and then return the result of f along with the final dirty
(defn record-dirties-fn [initial-dirty f]
  (assert (fn? f))
  (let [start-state (swap! state #(replace-dirty % initial-dirty))
        out (f)
        restored-state (swap! state #(replace-dirty % (defs/backup-dirty start-state)))]
    (-> {}
        (defs/result-value out)
        (defs/last-dirty (defs/backup-dirty restored-state)))))

(defmacro record-dirties [init & body]
  `(record-dirties-fn ~init (fn [] ~@body)))

;; The opposite of the above: f gets as input the last dirty,
;; and then it returns a snapshot with the result and
;; the new dirty that we'd like to use after this.
(defn inject-pure-code-fn [f]
  (let [current-state (deref state)
        snapshot (f (defs/last-dirty current-state))]
    (assert (defs/snapshot? snapshot))
    (swap! state #(defs/last-dirty % (defs/last-dirty snapshot)))
    (defs/result-value snapshot)))

(defmacro inject-pure-code [[d] & body]
  `(inject-pure-code-fn (fn [~d] ~@body)))

;; TODO: Analyze all collections
;;       Build a map from keyword to expr
;;       Traverse expr and replace all exprs by their keys
;;       Start write sd/compiler
;;       Later on: Ability to delay propagation (e.g. when evaluating the if)

;;; Accessors

;; Access the deps of a seed


;; Access the original-coll
(def access-original-coll (party/key-accessor :original-coll))

;; sd/compiler for the coll-seed type
(defn compile-coll [comp-state expr cb]
  (cb (defs/compilation-result
       comp-state
       (utils/normalized-coll-accessor
        (access-original-coll expr)
        (exm/lookup-compiled-indexed-results comp-state expr)))))

(defn coll-seed [x]
  (with-new-seed
    "coll-seed"
    (fn [s]
      (-> s
          (sd/access-indexed-deps (utils/normalized-coll-accessor x))
          (access-original-coll x)
          (defs/access-omit-for-summary #{:original-coll})
          (sd/compiler compile-coll)))))


(defn value-literal-type [x]
  (if (symbol? x)
    defs/dynamic-type
    (class x)))

(defn compile-static-value [state expr cb]
  (cb (defs/compilation-result state (cg/compile-static-value
                                      (defs/access-platform state)
                                      (sd/static-value expr)))))

(defn primitive-seed [x]
  (assert (not (coll? x)))
  (with-new-seed
    "primitive-seed"
    (fn [s]
      (-> s
          (sd/access-bind? false)
          (sd/static-value x)
          (defs/datatype (value-literal-type x))
          (sd/compiler compile-static-value)))))

;; Given a seed in the evaluated datastructure of a meta expression,
;; turn it into a seed.
(defn to-seed [x]
  (cond
    (sd/seed? x) x
    (coll? x) (coll-seed x)
    :default (primitive-seed x)))



(defn to-type [dst-type x]
  (-> x
      to-seed
      (defs/datatype dst-type)))

(defn to-dynamic [x]
  (to-type defs/dynamic-type x))


;;;;;; Analyzing an expression 
(defn access-no-deeper-than-seeds
  ([] {:desc "access-no-deeper-than-seeds"})
  ([x] (if (sd/seed? x)
         []
         x))
  ([x y] (if (sd/seed? x)
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
      (if (sd/seed? x)
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
    x (fn [x] (defs/datatype {} (defs/datatype x))))))

;; Get only the seeds, in a vector, in the order they appear
;; when traversing. Opposite of populate-seeds
(defn flatten-expr
  "Convert a nested expression to a vector of seeds"
  [x]
  (let [p (flat-seeds-traverse x identity)]
    (first p)))

(defn populate-seeds-visitor
  [state x]
  (if (sd/seed? x)
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

(defn map-expr-seeds
  "Apply f to all the seeds of the expression"
  [f expr]
  (let [src (flatten-expr expr)
        dst (map f src)]
    (assert (every? sd/seed? dst))
    (populate-seeds expr dst)))


(defn compile-seed [state seed cb]
  (if (sd/compiled-seed? seed)
    (cb (defs/compilation-result state (defs/compilation-result seed)))
    (if-let [c (sd/compiler seed)]
      ((sd/compiler seed) state seed cb)
      (throw (ex-info "Missing compiler for seed" {:seed seed})))))


(declare scan-referents-to-compile)




(defn typehint [seed-type sym]
  (assert (symbol? sym))
  sym)


(defn dirty-referents [refs]
  (assert (set? refs))
  (map first (filter (fn [[k v]] (defs/dirty-key? k)) refs)))


(def scope-ref-set #{
                     ;; Used by scopes to control the order of compilation
                     scope-ref-tag
                     })
(spec/def ::invisible-tag-key scope-ref-set)

(def bind-ref-set #{ ;; Used by loops to encourage an expression to be bound outside of
                    ;; the loop
                    bind-ref-tag
                    })
(spec/def ::bind-tag-key bind-ref-set)

(spec/def ::invisible-tag (spec/or :eval-composite-tag (spec/cat
                                                        :prefix ::invisible-tag-key
                                                        :sym any?)))

(spec/def ::invisible-ref (spec/cat :tag ::invisible-tag
                                    :value any?))

(defn relevant-ref-for-bind? [r]
  (not (spec/valid? ::invisible-ref r)))

(def sideeffect-set #{sideeffect-tag})
(spec/def ::sideeffect-ref (spec/cat :tag sideeffect-set
                                     :value any?))

(spec/def ::sideeffect-ref-value (spec/cat :ref (spec/spec ::sideeffect-ref)
                                           :value any?))

(defn has-sideeffect? [refs0]
  (some (specutils/pred ::sideeffect-ref-value) refs0))



;; Three kinds of deps: scope-ref, bind-ref, sideffect-ref and any other
;;

(spec/def ::seed-dep-key (spec/or :composite
                                  (spec/cat :key (spec/or :scope-ref scope-ref-set
                                                          :bind-ref bind-ref-set
                                                          :sideeffect-ref sideeffect-set
                                                          )
                                            :value any?)
                                  :simple any?))
(spec/def ::seed-ref (spec/cat :key ::seed-dep-key
                               :value any?))
(spec/def ::seed-refs (spec/coll-of ::seed-ref))

(defn classify-ref-key [[key-type parsed-key]]
  (if (= key-type :simple)
    :simple
    (-> parsed-key
        :key
        first)))

(defn classify-ref [parsed-ref]
  (-> parsed-ref
      :key
      classify-ref-key))

(defn analyze-refs [deps]
  (let [parsed (specutils/force-conform ::seed-refs deps)]
    (println "Analyzed these: " (map classify-ref parsed))))

(defn explicit-bind? [seed]
  (let [v (sd/access-bind? seed)]
    (if (fn? v)
      (v seed)
      v)))

(defn bind-seed?
  "Determinate if a seed should be bound to a local variable"
  [seed]
  (let [refs0 (sd/referents seed)
        _ (analyze-refs refs0)
        explicit-bind (explicit-bind? seed)
        refs (filter relevant-ref-for-bind? refs0)
        has-sideeffect (has-sideeffect? refs0)]
    (or
     (= true explicit-bind)
     (and
      (not= false explicit-bind)
      (or (defs/dirty? seed)
          has-sideeffect
          (< 1 (count refs)))))))

(def access-bindings (party/key-accessor ::bindings))

(defn add-binding [comp-state sym-expr-pair]
  (party/update
   comp-state
   access-bindings
   (fn [v]
     (assert (vector? v))
     (conj v sym-expr-pair))))

(defn remove-binding-marker [comp-state expected-marker]
  (party/update
   comp-state
   access-bindings
   (fn [b]
     (assert (= (last b) expected-marker))
     (vec (butlast b)))))

(defn split-tail [f? v0]
  (let [v (vec v0)
        reversed (reverse v)
        not-f? (complement f?)
        tail (take-while not-f? reversed)
        head (drop-while not-f? reversed)]
    [(vec (reverse head))
     (vec (reverse tail))]))
(comment
  (split-tail keyword? [1 2 :b 3 :a 4 5 6])
  (split-tail keyword? [1 2 3])
  )

(defn flush-bindings-to [f? comp-state cb]
  (let [bds (access-bindings comp-state)
        [head tail] (split-tail f? bds) ;[[] bds]
        comp-state (access-bindings comp-state head)]
    (if (empty? tail)
      (cb comp-state)
      `(let ~(reduce into [] tail)
         ~(cb comp-state)))))

(defn flush-bindings [comp-state cb]
  (flush-bindings-to (-> ::defs/binding
                         specutils/pred
                         complement)
                     comp-state cb))

;;;;;;;;;;;;; TODO
(def access-bind-symbol (party/key-accessor :bind-symbol))

(defn get-or-generate-hinted
  ([seed]
   (get-or-generate-hinted seed "sym"))
  ([seed name-prefix]
   (if (contains? seed :bind-symbol)
     (access-bind-symbol seed)
     (let [raw-sym (contextual-gensym name-prefix)
           hinted-sym (typehint (defs/datatype seed) raw-sym)]
       hinted-sym))))

(defn maybe-bind-result
  ([comp-state]
   (maybe-bind-result comp-state (exm/access-seed-key comp-state)))
  ([comp-state seed-key]
   (let [seed (-> comp-state
                  exm/seed-map
                  seed-key)]
     (if-let [bind-suffix (bind-seed? seed)]
       (let [hinted-sym (get-or-generate-hinted seed (name seed-key))
             result (defs/compilation-result seed)]
         (-> comp-state
             (defs/compilation-result hinted-sym) ;; The last compilation result is a symbol
             (add-binding [hinted-sym result]) ;; Add it as a binding
             (exm/update-comp-state-seed ;; Update the seed so that it has the symbol as result.
              seed-key #(defs/compilation-result % hinted-sym))))
       
       ;; Do nothing
       comp-state))))

;;;;;;;;;;;;; TODO

 ;; Otherwise we do nothing.

(defn post-compile [cb]
  (fn [comp-state]
    (-> comp-state
        exm/put-result-in-seed
        maybe-bind-result
        exm/scan-referents-to-compile
        cb)))

(defn compile-seed-at-key [comp-state seed-key cb]
  (when debug-seed-names
    (println "compile-seed-at-key" seed-key))
  (let [comp-state (exm/initialize-seed-compilation
                    comp-state seed-key)]
    (compile-seed
     
     comp-state
     
     (exm/initialize-seed-to-compile
      comp-state seed-key)
     
     (post-compile cb))))

;; The typesignature of the underlying exprssion
(def seed-typesig (party/key-accessor ::seed-typesig))

(defn expr-map [raw-expr]
  (exm/expr-map raw-expr to-seed))

(def basic-inspect-expr (comp pp/pprint
                              exm/summarize-expr-map
                              expr-map))


(defn initialize-compilation-state [m]
  (let [final-state (or (and (not (nil? state))
                             (deref state))
                        {})]

    ;; Decorate the expr-map with a few extra things
    (-> final-state

        (merge m)

        (utils/first-arg (begin :initialize-compilation-state))

        ;; Initialize a list of things to compile: All nodes that don't have dependencies
        exm/initialize-compilation-roots

        ;; Initialize the bindings, empty.
        (access-bindings [])

        (utils/first-arg (end :initialize-compilation-state))
        
        )))





(def ^:dynamic debug-compile-until false)

(defn compile-until [pred? comp-state cb]
  (if (pred? comp-state)
    (do debug-compile-until
        (cb comp-state)) 

    ;; Otherwise, continue recursively
    (let [[seed-key comp-state] (exm/pop-key-to-compile comp-state)]
      (begin [:compile-until seed-key])
      ;; Compile the seed at this key.
      ;; Bind result if needed.
      (when debug-compile-until
        (println "To compile" (exm/access-to-compile comp-state))
        (println "Compile seed with key" seed-key))
      (let [seed (exm/seed-at-key comp-state seed-key)]
        (if (sd/scope-termination? seed)
          (do
            (compile-seed-at-key
             comp-state
             seed-key
             utils/crash-if-called)) ;; <-- the result will be delivered to an atom.
          (let [flag (atom false)
                next-cb (fn [comp-state]
                          (end [:compile-until seed-key])
                          (reset! flag true)
                          (compile-until pred? comp-state cb))
                result (if (sd/scope-root? seed)
                         (let [scope-result-atom (atom nil)
                               comp-state (assoc comp-state
                                                 (:scope-id seed)
                                                 scope-result-atom)
                               compiled-scope (compile-seed-at-key comp-state seed-key next-cb)]
                           ;; In this call, the promise will eventually be resolved.
                           (if-let [[comp-state] (deref scope-result-atom)]
                             ((post-compile next-cb) (defs/compilation-result
                                                       comp-state compiled-scope))
                             (throw (ex-info "No scope result provided" {:seed-key seed-key}))))
                         (do
                           (compile-seed-at-key comp-state seed-key next-cb)))]
            (utils/data-assert (deref flag)
                               "Callback not called"
                               {:seed-key seed-key})    
            result))))))
(spec/fdef compile-until :args (spec/cat :pred fn?
                                         :comp-state ::comp-state
                                         :cb fn?))

(defn declare-local-vars [comp-state cb]
  (let [vars (::defs/local-vars comp-state)]
    (if (empty? vars)
      (cb comp-state)
      `(let ~(transduce
              (comp (map (comp :vars second))
                    cat
                    (map (fn [x] [(-> x :name symbol) `(atom nil)]))
                    cat)
              conj
              []
              vars)
         ~(cb (assoc comp-state ::defs/local-vars {}))))))

(defn compile-initialized-graph
  "Loop over the state"
  ([comp-state cb]
   (declare-local-vars
    comp-state
    (fn [comp-state]
      (compile-until
       (comp empty? exm/access-to-compile)
       comp-state
       cb)))))

(defn terminate-return-expr
  "Return the compilation result of the top node"
  [comp-state]
  (flush-bindings
   comp-state
   #(-> %
        exm/top-seed
        defs/compilation-result)))

(defn terminate-last-result
  [comp-state]
  (flush-bindings
   comp-state
   #(defs/compilation-result %)))

(defn check-all-compiled [comp-state]
  (doseq [[k v] (->> comp-state
                     exm/seed-map)]
    (utils/data-assert (sd/compiled-seed? v)
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

(defn finalize-state [value]
  (when (contains? value :trace-key)
    (swap! trace-map #(assoc % (:trace-key value)
                             ((:trace value))))
    (println "You can inspect the trace with (disp-trace" (:trace-key value) ")")))

(defn compile-top [expr]
  (let [final-state (deref state)
        terminated? (atom false)
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
    (finalize-state final-state)
    result))

(defn compile-terminate-snapshot [comp-state expr cb]
  (flush-bindings ;; Is this good?
   comp-state
   (fn [comp-state]
     (let [results  (exm/lookup-compiled-results
                     comp-state (sd/access-deps expr))]
       (cb (defs/compilation-result comp-state (:value results)))))))

(defn terminate-snapshot [ref-dirty snapshot]
  (if (= (defs/last-dirty snapshot)
         ref-dirty)
    (defs/result-value snapshot)

    ;; Create a new seed that depends on both the result value
    ;; and the dirty, and compile to the result value.
    (let [x (to-seed (defs/result-value snapshot))]
      (with-new-seed
        "terminate-snapshot"
        (fn [s]
          (-> s
              (sd/add-deps {:value x})
              (sd/set-dirty-dep (defs/last-dirty snapshot))
              (sd/compiler compile-terminate-snapshot)
              (defs/datatype (defs/datatype x))))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Packing and unpacking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (cb (defs/compilation-result
         comp-state
         `(nth ~(-> expr sd/access-compiled-deps :arg)
               ~i)))))

(defn unpack-vector-element
  "Helper to unpack"
  [src-expr dst-type index]
  (with-new-seed
    "Unpack-vector-element"
    (fn [s]
      (-> s
          (sd/access-deps {:arg src-expr})
          (assoc :index index)
          (defs/datatype (defs/datatype dst-type))
          (sd/compiler compile-unpack-element)))))

(defn inherit-datatype [x from]
  (defs/datatype x (defs/datatype from)))

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









(defn compile-sequentially [comp-state expr cb]
  (let [r (sd/access-compiled-deps expr)
        n (:n expr)]
    (cb
     (defs/compilation-result
       comp-state
       `(do
          ~@(map (partial get r) (range n))
          nil)))))

;; Compiles to a sequence of statements
(defn sequentially [& deps]
  (with-new-seed
    "sequentially"
    (fn [seed]
      (-> seed
          (assoc :n (count deps))
          (sd/access-indexed-deps deps)
          (sd/compiler compile-sequentially)))))

(defn compile-var-decl [comp-state expr cb]
  (cb (defs/compilation-result comp-state `(atom nil))))

(defn gen-var [tp]
  {:name (contextual-genstring "var")
   :type tp})


(defn var-symbol [x]
  (-> x :var :name symbol))

(defn compile-pack-var [comp-state expr cb]
  (let [r (sd/access-compiled-deps expr)]
    (cb (defs/compilation-result
         comp-state
         `(reset! ~(var-symbol expr)
                  ~(:expr r))))))

(defn pack-var [var x]
  (with-new-seed
    "pack-var"
    (fn [seed]
      (-> seed
          (assoc :var var)
          (sd/add-deps {:expr x})
          (sd/compiler compile-pack-var)))))

(defn compile-unpack-var [comp-state expr cb]
  (let [r (sd/access-compiled-deps expr)]
    (cb (defs/compilation-result
          comp-state
          `(deref ~(var-symbol expr))))))

(defn unpack-var [var dependency]
  (with-new-seed
    "unpack-var"
    (fn [seed]
      (-> seed
          (assoc :var var)
          (sd/add-deps {[sideeffect-tag :dep] dependency})
          (sd/compiler compile-unpack-var)))))

(defn allocate-vars [id type]
  (-> (swap! state
             (fn [state]
               (update-in state
                          [::defs/local-vars id]
                          (fn [lvars]
                            (if (nil? lvars)
                              {:type type
                               :vars (map gen-var (flatten-expr type))}
                              (do
                                (let [tp2 (:type lvars)]
                                  (utils/data-assert
                                   (= tp2 type)
                                   "Inconsistent pack type"
                                   {:current tp2
                                    :new type}))
                                lvars))))))
      ::defs/local-vars
      id
      :vars))

(defn pack-at [id expr]
  (let [type (type-signature expr)
        vars (allocate-vars id type)]
    (apply sequentially
           (map pack-var vars (flatten-expr expr)))))


;;; Must be called *after* pack-at
(defn unpack-at [id dependency]
  (let [vars (-> state
                 deref
                 ::defs/local-vars
                 id)]
    (assert (not (nil? vars)))
    (populate-seeds (:type vars)
                    (map #(unpack-var % dependency)
                         (:vars vars)))))

;; Returns a pair of functions that can be used to unpack and pack.
#_(defn pack-unpack-fn-pair [expr]
  (let [tp (type-signature expr)
        f (flatten-expr expr)
        n (count f)
        vars (map prep-var f)]
    {:pack (fn [expr]
             (assert (= (type-signature expr) tp))
             (apply sequentially
                    (for [[var x] (map vector vars (flatten-expr expr))]
                      (pack-var var x))))
     :unpacked (populate-seeds expr (map unpack-var vars))
     }))













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
              sd/access-deps
              :indirect)]
    (assert (keyword? k))
    (cb
     (defs/compilation-result
      comp-state
       (-> comp-state
           exm/seed-map
           k
           defs/compilation-result)))))


;; The reason for indirection is so that we can add dependencies,
;; in case we are not dealing with a seed. 
;; We can also use it to generate a local binding where we need it.
(defn indirect
  "Every problem can be solved with an extra level of indirection, or something like that, it says, right?"
  ([x] (indirect x identity))
  ([x decorations]
   (with-new-seed
     "indirect"
     (fn [s]
       (-> s
           (sd/add-deps {:indirect x})
           (sd/compiler compile-forward)
           (defs/datatype (-> x
                              to-seed
                              defs/datatype))
           decorations)))))

(defn rebind [x]
  (indirect x #(sd/access-bind? % true)))


(def wrapped-function (party/key-accessor :wrapped-function))

(defn compile-wrapfn [comp-state expr cb]
  (cb
   (defs/compilation-result
    comp-state
    `(~(wrapped-function expr)
      ~@(exm/lookup-compiled-indexed-results comp-state expr)))))

(def default-wrapfn-settings {:pure? false})

(defn wrapfn-sub [label f settings0] ;; f is a quoted symbol
  (let [settings (merge default-wrapfn-settings settings0)]
    (fn [& args]
      (with-new-seed
        "wrapped-function"
        (fn [s]
          (-> s
              (sd/access-indexed-deps args)
              (wrapped-function f)
              (defs/datatype defs/dynamic-type)
              (sd/compiler compile-wrapfn)
              (sd/mark-dirty (not (:pure? settings)))
              ;;disp-deps
              ))))))

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









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Scopes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn conditionally-flush-bindings [condition comp-state cb]
  (if condition
    (flush-bindings
     comp-state
     cb)
    (cb comp-state)))

(defn compile-scope-root [state expr cb]
  (let [scope-id (:scope-id expr)]
    (assert (keyword? scope-id))
    (cb (defs/compilation-result
          (add-binding state scope-id)
          ::scope-root))))

(defn scope-root [scope-id scope-spec]
  (let [desc (:desc scope-spec)]
    (with-new-seed
      (str "scope-root-" desc)
      (fn [seed]
        (-> seed
            (merge scope-spec)
            (assoc :scope-id scope-id)
            (sd/compiler compile-scope-root)
            sd/mark-scope-root)))))

(defn make-snapshot [result d]
  (-> {}
      (defs/result-value result)
      (defs/last-dirty d)))


(defn compile-scope-termination [comp-state expr _]
  (flush-bindings
   comp-state
   (fn [comp-state]
     (let [scope-id (:scope-id expr)
           comp-state (remove-binding-marker comp-state scope-id)]
       (let [k (-> expr
                   sd/access-deps
                   :indirect)
             result-expr (-> comp-state
                             exm/seed-map
                             k
                             defs/compilation-result)]
         (assert (keyword? k))
         ;; Instead of calling a callback, provide the next compilation state
         ;; to the :scope-result atom, wrapped in a vector.
         #_(reset! (:scope-result comp-state) [comp-state])
         (swap! (scope-id comp-state)
                (fn [old]
                  (assert (nil? old))
                  [comp-state]))
         result-expr)))))

(defn scope-termination [scope-id desc sr should-be-dirty? x]
  (with-new-seed
    (str "scope-termination-" desc)
    (fn [seed]
      (-> seed
          (assoc :scope-id scope-id)
          (sd/add-deps {:indirect x :scope-root sr})
          (sd/compiler compile-scope-termination)
          (sd/mark-dirty should-be-dirty?)
          sd/mark-scope-termination))))

(defmacro scope [scope-sp0 & body]
  (let [scope-sp (merge {:ref-tag scope-ref-tag} scope-sp0)]
    `(do
       (specutils/validate ::defs/scope-spec ~scope-sp)
       (let [scope-id# (contextual-genkey "scope-id")
             out# (inject-pure-code
                   [input-dirty#]
                   (let [desc# (:desc ~scope-sp)
                         term-snapshot#
                         (deeper-tagged-scope-state ~(:ref-tag scope-sp)
                          (let [sr# (scope-root scope-id# ~scope-sp)]
                            (deeper-scope-state
                             (let [result-snapshot# (record-dirties input-dirty# ~@body)
                                   should-be-dirty?# (and (:dirtified? ~scope-sp)
                                                          (not= input-dirty#
                                                                (defs/last-dirty
                                                                  result-snapshot#)))]
                               (deeper-scope-state
                                (record-dirties
                                 (defs/last-dirty result-snapshot#)
                                 (scope-termination
                                  scope-id#
                                  desc#
                                  sr#
                                  should-be-dirty?#
                                  (terminate-snapshot
                                   input-dirty# result-snapshot#)
                                  )))))))]

                     term-snapshot#))]
         (reset-scope-seeds [out#])
         out#))))
(spec/fdef scope :args (spec/cat :spec ::defs/scope-spec
                                 :body (spec/* any?)))

(declare pure+)
(declare dirty+)

#_(defn disp-test-scope []
  (inject []                            ; pp/pprint
   (with-context []
     (pure+ 1 2)
     (scope {:desc "Katsk" :dirtified? true}
            (pure+ 3 4)))))

(defn compile-if2 [comp-state expr cb]
  (let [rdeps (sd/access-compiled-deps expr)]
    (cb (defs/compilation-result
          comp-state
          `(if ~(:condition rdeps)
             ~(:true-branch rdeps)
             ~(:false-branch rdeps))))))

(defn if2-expr [if-id
                settings
                condition
                true-branch
                false-branch]
  
  (let [true-t (type-signature true-branch)
        false-t (type-signature false-branch)]
    (utils/data-assert (or (= true-t false-t)
                           (not (:check-branch-types? settings)))
                       
                       "Different types for true branch and false branch"
                       {:true-type true-t
                        :false-type false-t})

    ;;:unpacked (unpack-at if-id)
    (with-new-seed
      "if2-seed"
      (fn [seed]
        (-> seed
            (sd/add-deps {:condition condition
                          :true-branch true-branch
                          :false-branch false-branch})
            (sd/compiler compile-if2))))))

(defmacro if2-main-macro [condition true-branch false-branch settings]
  `(let [if-id# (contextual-genkey "if-id")]
     (unpack-at if-id#
                (scope {:desc "if-scope"
                        :dirtified? true
                        }
                       
                       (if2-expr if-id#
                                 ~settings
                                 ~condition
                                 (scope {:desc "true-branch"
                                         :dirtified? false
                                         }
                                        
                                        (pack-at if-id# ~true-branch))
                                 
                                 (scope {:desc "false-branch"
                                         :dirtified? false
                                         }
                                        
                                        (pack-at if-id# ~false-branch)))))))

(defmacro if2 [condition true-branch false-branch]
  `(if2-main-macro ~condition
                   ~true-branch
                   ~false-branch
                   {:check-branch-types? true}))





















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OOLD  Loop form
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
  (cb (defs/compilation-result comp-state (access-bind-symbol expr))))

(defn make-loop-binding [comp-state lvar-key]
  (assert (keyword? lvar-key))
  (let [lvar (exm/get-seed comp-state lvar-key)]
    [(access-bind-symbol lvar)
     (:value (exm/get-compiled-deps comp-state lvar))]))

(defn replace-by-local-var [x0]
  (let [x (to-seed x0)]
    (with-new-seed
      "local-var"
      (fn [s]
        (-> s
            (access-bind-symbol (get-or-generate-hinted x))
            (sd/add-deps {:value x})
            (sd/access-bind? false)
            (defs/datatype (defs/datatype x))
            (sd/compiler compile-bind))))))

(defn replace-by-local-vars [x]
  (map-expr-seeds replace-by-local-var x))

(defn bind-if-not-masked [mask value]
  (if mask
    (replace-by-local-var value)        ;; <-- assign a symbol to it, and we are going to use it.
    (sd/access-bind? (to-seed value) true) ;; <-- force it to be bound outside of the loop
    ))

(def access-mask (party/key-accessor :mask))

(defn compile-recur [comp-state expr cb]
  (let [results (exm/lookup-compiled-indexed-results comp-state expr)]
    (cb (defs/compilation-result comp-state
                            `(recur ~@results)))))

(def recur-seed-type ::recur)

(defn recur-seed [x]
  (with-new-seed
    "recur"
    (fn [s]
      (-> s
          (sd/access-indexed-deps (flatten-expr x))
          (defs/datatype recur-seed-type)
          (sd/compiler compile-recur)))))

(defn remove-loop?-key [x]
  (dissoc x :loop?))

(defn prepare-return-value [x]
  (-> x
      remove-loop?-key
      pack
      to-seed))

(defn active-loop-vars-mask [input-dirty initial-state eval-state-fn next-state-fn]
  (let [next-state (defs/result-value
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  New loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-loop [comp-state expr cb]
  (cb (defs/compilation-result
        comp-state
        (let [cdeps (defs/access-compiled-deps expr)]
          `(if ~(:loop? cdeps)
             ~(:next cdeps)
             ~(:result cdeps))))))

(defn make-loop-seed [args]
  (with-new-seed
    "loop-seed"
    (fn [seed]
      (-> args
          (merge seed)
          (sd/add-deps args)
          (sd/compiler compile-loop)))))

(defn compile-loop-header [comp-state expr cb]
  (let [bindings (sd/access-indexed-deps expr)]
    `(loop ~(reduce into [] (map (partial make-loop-binding comp-state) bindings))
       ~(cb (defs/compilation-result comp-state (-> expr
                                                    defs/access-compiled-deps
                                                    :wrapped))))))

(defn make-loop-header [bindings wrapped-body]
  (with-new-seed
    "loop-header"
    (fn [seed]
      (-> seed
          (assoc :bindings bindings)
          (sd/access-indexed-deps (flatten-expr bindings))
          (sd/add-deps {:wrapped wrapped-body})
          (sd/compiler compile-loop-header)))))

(defn compile-step-loop-state [comp-state expr cb]
  (cb (defs/compilation-result comp-state
        `(recur ~@(exm/lookup-compiled-indexed-results comp-state expr)))))

(defn compute-active-mask [a b]
  (mapv not=
        (flatten-expr a)
        (flatten-expr b)))

(defn step-loop-state [mask-export bindings expr]
  (let [state-type (type-signature bindings)
        expr-type (type-signature expr)]
    (utils/data-assert (= state-type expr-type)
                       "Loop mismatch"
                       {:state-type state-type
                        :expr-type expr-type})
    (let [mask (mask-export (compute-active-mask bindings expr))
          rebound (map-expr-seeds rebind expr)]
      (with-new-seed
        "step-loop-state"
        (fn [seed]
          (-> seed
              (assoc :dst bindings)
              (sd/compiler compile-step-loop-state)
              (sd/access-indexed-deps (flatten-expr rebound))))))))

(defn basic-loop2 [args]
  (specutils/validate ::looputils/args args)
  (let [loop-id (contextual-genkey "basic-loop2")
        loop-bindings (replace-by-local-vars (:init args))
        state-type (type-signature loop-bindings)]

    ;; Top most loop scope
    (unpack-at
     loop-id
     (scope {:desc "Loop-scope"
             :dirtified? true
             :ref-tag :loop-dependency}


            (let [ ;; Evaluate the state
                  evaluated (utils/error-context
                             "Evaluating the loop state"
                             {:type (type-signature loop-bindings)}
                             ((:eval args) loop-bindings))

                  eval-type-info {:evaluated-type (type-signature evaluated)}

                  ;; We always evaluate the loop condition
                  loop? (utils/error-context
                         "Evaluating the loop condition"
                         eval-type-info
                         ((:loop? args) evaluated))

                  ;; And then we take action, based on the outcome of the loop
                  ;; condition

                  ;; This is the value that we return
                  result (scope {:desc "result"
                                 :dirtified? false
                                 }
                                (utils/error-context
                                 "Evaluating the loop result"
                                 eval-type-info
                                 (pack-at loop-id ((:result args) evaluated))))

                  ;; Otherwise, we continue to loop
                  [next [active-mask]] (utils/with-value-export
                                        export-mask
                                        (scope {:desc "next"
                                                :dirtified? false
                                                }
                                               (utils/error-context
                                                "Evaluating the next state"
                                                eval-type-info
                                                (step-loop-state export-mask
                                                                 loop-bindings
                                                                 ((:next args) evaluated)))))

                  next-type (type-signature next)]

              ;; Active mask not used now
              ;(println "Active mask is" active-mask)

              ;; This takes care of generating the code
              (make-loop-header
               loop-bindings
               (make-loop-seed {;:active-mask active-mask
                                :evaluated evaluated
                                :loop? loop?
                                :result result
                                :next next})))))))

(spec/fdef basic-loop2 :args (spec/cat :args ::looputils/args))


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
   (basic-loop2
    {:init {:result init
            :coll collection}
     :eval (fn [state]
             (merge state {:loop? (pure-not (pure-empty? (:coll state)))}))
     :loop? :loop?
     :result identity
     :next (fn [state]
      {:result (f (:result state)
                  (pure-first (:coll state)))
       :coll (pure-rest (:coll state))})})))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro debug-inject [x]
  `(debug/pprint-code (macroexpand (quote (inject [] ~x)))))

#_(debug-inject
 (basic-loop2
  {:init [(to-dynamic 0)    ; <--- Passive!
          (to-dynamic 0)]
   :eval identity
   :loop? (fn [[a b]]
            (pure< b 9))
   :next (fn [[a b]]
           [a (pure-inc b)])
   :result identity}))
#_(debug-inject
 (my-basic-reduce (fn [sum x]
                    (pure+ sum (my-basic-sum x)))
                  (to-dynamic 0)
                  (to-dynamic [[1 2] [3 4] [5 6 7]])))


#_(debug-inject
 (basic-loop2
  {:init  {:value (to-type defs/dynamic-type (to-seed 4))
           :product (to-type defs/dynamic-type (to-seed 1))}
   :eval (fn [x] (merge x {:loop?  (pure< 0 (:value x))}))
   :loop? :loop?
   :next (fn [x] {:value (pure-dec (:value x))
                  :product (pure* (:product x)
                                  (:value x))})
   :result identity}))



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
