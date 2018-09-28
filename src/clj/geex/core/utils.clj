(ns geex.core.utils

  "This is the implementation of the *Polhem* compiler, along with fundamental tools.

  "
  
  (:require [bluebell.utils.wip.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.traverse :as traverse]
            [bluebell.utils.wip.core :as utils]
            [clojure.pprint :as pp]
            [clojure.string :as cljstr]
            [bluebell.utils.wip.debug :as debug]
            [clojure.spec.test.alpha :as stest]
            [bluebell.utils.wip.party.coll :as partycoll]
            [geex.debug :refer [set-inspector inspect inspect-expr-map]]
            [bluebell.utils.wip.specutils :as specutils]
            [bluebell.utils.wip.trace :as trace]
            [geex.core.defs :as defs]
            [geex.core.seed :as sd]
            [geex.core.jvm :as gjvm]
            [geex.core.exprmap :as exm]
            [geex.core.datatypes :as datatypes]
            [geex.core.loop :as looputils]
            [geex.core.xplatform :as xp]
            [clojure.set :as cljset]))

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

(defn wrap-expr-compiler [c]
  {:pre [(fn? c)]}
  (fn [comp-state expr cb]
    (cb (defs/compilation-result comp-state (c expr)))))
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
           ::defs/comp-state nil ;; <-- The last compiltation state
           }))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Scope data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-scope-state
  ([]
   {:parents #{}
    :local-deps (atom #{})
    :seeds (atom #{})
    :ref-tag defs/scope-ref-tag})
  ([old-state]
   (let [old-seeds (-> old-state
                       :seeds
                       deref)
         parents (if (empty? old-seeds)
                   (:parents old-state)
                   old-seeds)]
     {:parents parents
      :local-deps (atom #{})
      :seeds (atom #{})
      :ref-tag defs/scope-ref-tag})))

(defn clear-scope-state [sc]
  (reset! (:local-deps sc) #{})
  (reset! (:seeds sc) #{}))

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
  (swap! (:local-deps scope-state) into x)
  (reset! (:seeds scope-state) x))

(defmacro with-context [[eval-ctxt]& args]
  `(binding [scope-state (new-scope-state)
             defs/state (initialize-state ~eval-ctxt)
             defs/gensym-counter (defs/new-or-existing-gensym-counter)]
     ~@args))

(def recording? (party/key-accessor ::recording?))


;; Helper for with-requirements
(defn append-requirements [r s]
  (party/update s defs/requirements #(into % r)))

(defn with-requirements-fn [r f]
  (assert (fn? f))
  (let [initial-reqs (-> defs/state deref defs/requirements)
        new-reqs (swap! defs/state (partial append-requirements r))
        result (f)
        old-reqs (swap! defs/state #(defs/requirements % initial-reqs))]
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
            (into (:parents scope-state)

                  ;;;;; THIS BREAKS IT!!!!
                  (deref (:local-deps scope-state))

                  
                  ))))) ;;;;; <--- here?

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

(def base-seed (-> {}
                   (sd/access-tags #{})
                   (sd/referents #{})
                   (sd/compiler nil)
                   (sd/datatype nil)
                   (defs/access-omit-for-summary [])))

;; Create a new seed, with actual requirements
(defn initialize-seed-sub [desc req-map]
  (utils/data-assert (only-non-whitespace? desc) "Bad seed descriptor"
                     {:desc desc})
  (when debug-init-seed
    (println (str  "Initialize seed with desc '" desc "'")))
  (assert (string? desc))
  (-> base-seed
      (sd/description desc)
      (assoc ::initial-deps req-map)
      (sd/access-deps req-map)))

(def empty-seed (initialize-seed-sub "empty-seed" {}))



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

(defn finalize-seed [seed]
  (-> seed
      (update ::defs/deps (partial merge (::initial-deps seed)))
      (dissoc ::initial-deps)
      ))

(defn with-stateless-new-seed [desc f]
  (let [result-seed (f (initialize-seed-sub desc {}))]
    (utils/data-assert (sd/seed? result-seed) "Not a valid seed" {:value result-seed})
    (utils/data-assert (not (sd/marked-dirty? result-seed))
                       "Seeds cannot not be dirty in a stateless setting"
                       {:value result-seed})
    result-seed))

(defn with-stateful-new-seed [desc f]
  (let [current-state (deref defs/state)
        result-seed (f (initialize-seed-sub desc
                                            (make-req-map current-state)))]
    (if (sd/marked-dirty? result-seed)
      (defs/last-dirty (swap! defs/state #(register-dirty-seed % result-seed)))
      result-seed)))

(defn validate-seed [seed]
  seed)

(defn with-new-seed [desc f0]
  (let [f (comp finalize-seed validate-seed f0)]
    (register-scope-seed
     (if (nil? defs/state)
       (with-stateless-new-seed desc f)
       (with-stateful-new-seed desc f)))))



(defn replace-dirty [s new-dirty]
  (-> s
      (defs/backup-dirty (defs/last-dirty s))
      (defs/last-dirty new-dirty)))

;; Given an initial dirty, initialize the state
;; with that dirty, call (f) without any arguments,
;; and then return the result of f along with the final dirty
(defn record-dirties-fn [initial-dirty f]
  (assert (fn? f))
  (let [start-state (swap! defs/state #(replace-dirty % initial-dirty))
        out (f)
        restored-state (swap! defs/state #(replace-dirty % (defs/backup-dirty start-state)))]
    (-> {}
        (defs/result-value out)
        (defs/last-dirty (defs/backup-dirty restored-state)))))

(defmacro record-dirties [init & body]
  `(record-dirties-fn ~init (fn [] ~@body)))

;; The opposite of the above: f gets as input the last dirty,
;; and then it returns a snapshot with the result and
;; the new dirty that we'd like to use after this.
(defn inject-pure-code-fn [f]
  (let [current-state (deref defs/state)
        snapshot (f (defs/last-dirty current-state))]
    (assert (defs/snapshot? snapshot))
    (swap! defs/state #(defs/last-dirty % (defs/last-dirty snapshot)))
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



(defn coll-seed [x]
  (with-new-seed
    "coll-seed"
    (fn [s]
      (-> s
          (sd/access-indexed-deps (partycoll/normalized-coll-accessor x))
          (access-original-coll x)
          (sd/datatype (xp/call :get-type-signature x))
          (defs/access-omit-for-summary #{:original-coll})
          (sd/compiler (xp/get :compile-coll))))))


(defn value-literal-type [x]
  (if (symbol? x)
    defs/dynamic-type
    (datatypes/unboxed-class-of x)))

(def keyword-seed (xp/caller :keyword-seed))

(def symbol-seed (xp/caller :symbol-seed))

(def string-seed (xp/caller :string-seed))

(defn complete-typed-seed [x]
  (coll-seed x))



(defn ensure-seed? [x]
  (assert (sd/compilable-seed? x))
  x)


(defn to-dynamic [x]
  (to-type defs/dynamic-type x))


;;;;;; Analyzing an expression 
(def access-no-deeper-than-seeds
  (party/wrap-accessor
   {:desc "access-no-deeper-than-seeds"
    :getter (fn [x] (if (sd/seed? x)
                      []
                      x))
    :setter (fn [x y] (if (sd/seed? x)
                        x
                        y))}))

(def top-seeds-accessor
  (party/chain
   access-no-deeper-than-seeds
   partycoll/normalized-coll-accessor))


;;; Helper for flat-seeds-traverse


(defn selective-conj-mapping-visitor [pred-fn f]
  (fn [state x0]
    (let [x (symbol-to-seed x0)]
      (if (pred-fn x)
        [(conj state x) (f x)]
        [state x]))))

(defn flat-seeds-traverse
  "Returns a vector with first element being a list of 
  all original expr, the second being the expression
  with mapped seeds"
  [pred-fn expr f]
  (traverse/traverse-postorder-with-state
   [] expr
   {:visit (selective-conj-mapping-visitor pred-fn f)
    :access-coll top-seeds-accessor
    }))

;; Get a datastructure that represents this type.
(defn type-signature [x]
  (second
   (flat-seeds-traverse
    sd/seed?
    x
    sd/strip-seed)))

;; Get only the seeds, in a vector, in the order they appear
;; when traversing. Opposite of populate-seeds
(defn flatten-expr
  "Convert a nested expression to a vector of seeds"
  [x]
  (let [p (flat-seeds-traverse sd/seed? x identity)]
    (first p)))

(def size-of (comp count flatten-expr))

(defn populate-seeds-visitor
  [state x]
  (if (sd/seed? x)
    [(rest state) (first state)]
    [state x]))

(defn populate-seeds
  "Replace the seeds in dst by the provided list"
  ([dst seeds]
   (second
    (traverse/traverse-postorder-with-state
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Binding compilation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defn summarize-refs [deps]
  (map classify-ref (specutils/force-conform ::defs/seed-refs deps)))

(defn explicit-bind? [seed]
  (let [v (sd/access-bind? seed)]
    (if (fn? v)
      (v seed)
      v)))


(defn analyze-seed-binding [seed]
  {:explicit-bind? (explicit-bind? seed)
   :dirty? (defs/dirty? seed)
   :ref-summary (-> seed
                    sd/referents
                    summarize-refs
                    utils/count-values)})
(spec/fdef analyze-seed-binding
           :args (spec/cat :seed ::defs/seed)
           :ret ::defs/seed-binding-summary)

(defn side-effecty? [summary]
  (or (:dirty? summary)
      (< 0 (utils/count-or-0 (:ref-summary summary)
                             defs/sideeffect-ref-tag))))

(defn refed-count [summary which]
  (utils/count-or-0 (:ref-summary summary)
                    which))

(defn compute-bind-level
  "Given a summary determine whether 
  (i :bind) we should bind the seed to a variable or
  (ii :list) insert the code as a sideeffectful statement when generating code or
  (iii :dont-bind) Dont bind the seed at all, just let it appear whereever..."
  [summary]
  (cond
    (= true (:explicit-bind? summary)) :bind
    (= false (:explicit-bind? summary)) :dont-bind
    (or (<= 2 (refed-count summary defs/simple-tag)) ;; <-- Value used multiple times
        (and (<= 1 (refed-count summary defs/simple-tag)) ;; <-- Value used once, and...
             (or (<= 1 (refed-count summary defs/bind-ref-tag)) ;; ...should be bound, or
                 (side-effecty? summary)))) :bind ;; ...also has a sideeffect
    (side-effecty? summary) :list ;; Meaning that it is probably a statement.
    :default :dont-bind))

(def compute-seed-bind-level (comp compute-bind-level analyze-seed-binding))


(def access-bindings (party/key-accessor ::bindings))

(spec/def ::xpair (spec/cat :a any?
                            :b any?))

(defn add-binding [comp-state sym-expr-pair]
  (specutils/validate (spec/or :binding ::defs/binding
                               :marker keyword?)
                      sym-expr-pair)
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
      (xp/call
       :render-bindings
       tail
       (cb comp-state)))))

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
     (if (contains? defs/bind-or-list (compute-seed-bind-level seed))
       (let [hinted-sym (xp/call
                         :to-variable-name
                         (get-or-generate-hinted
                          seed (name seed-key)))
             result (defs/compilation-result seed)]
         (-> comp-state
             (defs/compilation-result hinted-sym) ;; The last compilation result is a symbol
             (add-binding {:result result
                           :name hinted-sym
                           :seed seed}) ;; Add it as a binding
             (exm/update-comp-state-seed ;; Update the seed so that it has the symbol as result.
              seed-key #(defs/compilation-result % hinted-sym))))
       
       ;; Do nothing
       comp-state))))

;;;;;;;;;;;;; TODO

 ;; Otherwise we do nothing.





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Compiling a seed
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-compilation-result [comp-state]
  (xp/call :check-compilation-result
           (defs/compilation-result comp-state))
  comp-state)

(defn post-compile [cb]
  (fn [comp-state]
    (-> comp-state
        check-compilation-result
        exm/put-result-in-seed
        maybe-bind-result
        exm/scan-referents-to-compile
        cb)))

(defn compile-seed-at-key [comp-state seed-key cb]
  (when debug-seed-names
    (println "compile-seed-at-key" seed-key))
  (let [debug? (= seed-key :gs-call-static-method-181)
        comp-state (exm/initialize-seed-compilation
                    comp-state seed-key)]
    (compile-seed
     
     comp-state
     
     (let [s (exm/initialize-seed-to-compile
              comp-state seed-key)]
       (if debug?
         (println "This is the seed " s))
       s)
     
     (post-compile cb))))

;; The typesignature of the underlying exprssion
(def seed-typesig (party/key-accessor ::seed-typesig))

(def basic-inspect-expr (comp pp/pprint
                              exm/summarize-expr-map
                              expr-map))


(defn initialize-compilation-state [m]
  (let [final-state (or (and (not (nil? defs/state))
                             (deref defs/state))
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

(defn compile-until
  "Inner compilation loop: Traversing the graph respecting the partial ordering."
  [pred? comp-state cb]
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
                                         :comp-state ::defs/comp-state
                                         :cb fn?))
;;"Variables that need to be visible in the entire scope. Used for returning values from 
;; expressions, etc.
(def declare-local-vars (xp/caller :declare-local-vars))

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
   (fn [comp-state]
     
     ;; Make the last compilation state visible in the comp-state atom
     (swap! defs/state #(defs/access-comp-state % comp-state))
     
     (defs/compilation-result comp-state))))

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
  (when debug-full-graph
    (println "Displaying the graph")
    (inspect-expr-map m)
    (println "Displayed it."))
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

  ;;; Very important
  (clear-scope-state scope-state)
  
  (let [final-state (deref defs/state)
        terminated? (atom false)
        start (System/currentTimeMillis)
        _ (begin :compile-full)
        result (compile-full expr
                             (terminate-all-compiled-last-result
                              terminated?))
        _ (end :compile-full)
        end (System/currentTimeMillis)
        final-comp-state (-> defs/state
                             deref
                             defs/access-comp-state)]
    (assert (-> final-comp-state
                nil?
                not))
    (when (:disp-total-time? (deref defs/state))
      (println (str "Compiled in " (- end start) " milliseconds")))
    (assert (deref terminated?))
    (finalize-state final-state)
    {:comp-state final-comp-state
     :result result
     :expr expr}))

(defn inherit-datatype [x from]
  (defs/datatype x (defs/datatype from)))

(defn var-symbol [x]
  (-> x :var :name symbol))









;; Normalize something to a type such that we get the same type when we call rest on it.

(xp/register
 :clojure
 {:render-bindings
  (fn [tail body]
    `(let ~(reduce into [] (map (fn [x]
                                  [(:name x) (:result x)])
                                tail))
       ~body))

  :to-variable-name symbol

  :get-type-signature gjvm/get-type-signature
  :get-compilable-type-signature
  gjvm/get-compilable-type-signature

  :compile-coll
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state
          (partycoll/normalized-coll-accessor
           (access-original-coll expr)
           (exm/lookup-compiled-indexed-results comp-state expr)))))

  :compile-static-value
  (fn  [state expr cb]
    (cb (defs/compilation-result state (sd/static-value expr))))


  :declare-local-vars
  (fn [comp-state cb]
    (let [vars (::defs/local-vars comp-state)]
      (if (empty? vars)
        (cb comp-state)

        ;; Generate the code for local variables
        `(let ~(transduce
                (comp (map (comp :vars second))
                      cat
                      (map (fn [x] [(-> x :name symbol) `(atom nil)]))
                      cat)
                conj
                []
                vars)
           ~(cb (assoc comp-state ::defs/local-vars {}))))))

  :render-sequential-code
  (fn [code]
    `(do
       ~@code
       nil))

  :compile-bind
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state (access-bind-symbol expr))))


  :compile-bind-name
  (fn [x]
    (throw (ex-info "Not applicable for this platform" {:x x})))

  :compile-loop-header
  (fn [comp-state expr cb]
    (let [bindings (sd/access-indexed-deps expr)]
      `(loop ~(reduce
               into []
               (map (partial make-loop-binding
                             comp-state) bindings))
         ~(cb (defs/compilation-result
                comp-state
                (-> expr
                    defs/access-compiled-deps
                    :wrapped))))))

  :compile-return-value
  (fn [datatype expr]
    (throw (ex-info "Return value not supported on this platform"
                    {:datatype datatype
                     :expr expr})))

  :compile-nil
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state
          nil)))

  :check-compilation-result (constantly nil)
  
  })
