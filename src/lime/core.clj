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
            [lime.core.exprmap :as exm]))

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
           }))))

(defn new-scope-state
  ([]
   {:parents #{}
    :seeds (atom #{})})
  ([old-state]
   {:parents (-> old-state
                 :seeds
                 deref)
    :seeds (atom #{})}))

(defmacro deeper-scope-state [& body]
  `(do
     (utils/data-assert (not (nil? scope-state))
                        "There must be a scope state"
                        {})
     (binding [scope-state (new-scope-state scope-state)]
       ~@body)))

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






;; Associate the requirements with random keywords in a map,
;; so that we can merge it in deps.
(defn make-req-map [state-value]
  (into {} (map (fn [x]
                  (specutils/validate ::defs/requirement x)
                  [[(defs/requirement-tag x)
                    (keyword (contextual-gensym "req"))]
                   (defs/requirement-data x)])
                (-> state-value defs/requirements))))

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

;; Create a new seed, with actual requirements
(defn initialize-seed-sub [desc platform req-map]
  (assert (only-non-whitespace? desc))
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


(defn compile-seed [state seed cb]
  (if (sd/compiled-seed? seed)
    (cb (defs/compilation-result state (defs/compilation-result seed)))
    ((sd/compiler seed) state seed cb)))


(declare scan-referents-to-compile)




(defn typehint [seed-type sym]
  (assert (symbol? sym))
  sym)


(defn dirty-referents [refs]
  (assert (set? refs))
  (map first (filter (fn [[k v]] (defs/dirty-key? k)) refs)))


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
  (let [refs0 (sd/referents seed)
        explicit-bind (let [v (sd/access-bind? seed)]
                        (if (fn? v)
                          (v seed)
                          v))
        refs (filter relevant-ref-for-bind? refs0)]
    (or
     (= true explicit-bind)
     (and
      (not= false explicit-bind)
      (or (defs/dirty? seed)
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

(defn compile-seed-at-key [comp-state seed-key cb]
  (when debug-seed-names
    (println "compile-seed-at-key" seed-key))
  (let [comp-state (exm/initialize-seed-compilation
                    comp-state seed-key)]
    (compile-seed
     
     comp-state
     
     (exm/initialize-seed-to-compile
      comp-state seed-key)
     
     (fn [comp-state]
       (-> comp-state
           exm/put-result-in-seed
           maybe-bind-result
           exm/scan-referents-to-compile
           cb)))))

;; The typesignature of the underlying exprssion
(def seed-typesig (party/key-accessor ::seed-typesig))

(defn expr-map [raw-expr]
  (exm/expr-map raw-expr to-seed))

(def basic-inspect-expr (comp pp/pprint
                              exm/summarize-expr-map
                              expr-map))


(defn initialize-compilation-state [m]

  ;; Decorate the expr-map with a few extra things
  (-> m

      (defs/access-platform (get-platform))

      (utils/first-arg (begin :initialize-compilation-state))

      ;; Initialize a list of things to compile: All nodes that don't have dependencies
      exm/initialize-compilation-roots

      ;; Initialize the bindings, empty.
      (access-bindings [])

      (utils/first-arg (end :initialize-compilation-state))
      
      ))





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
    (comp empty? exm/access-to-compile)
    comp-state
    cb)))

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
  (let [results  (exm/lookup-compiled-results
                  comp-state (sd/access-deps expr))]
    (cb (defs/compilation-result comp-state (:value results)))))

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
  [x]
  #_(println "Indirect to")
  #_(debug/dout x)
  (with-new-seed
    "indirect"
    (fn [s]
      (-> s
          (sd/add-deps {:indirect x})
          (sd/compiler compile-forward)
          (defs/datatype (-> x
                             to-seed
                             defs/datatype))))))




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

(defn scope-root [desc]
  (with-new-seed
    (str "scope-root-" desc)
    identity))

(defn scope-termination [desc]
  (with-new-seed
    (str "scope-termination-" desc)
    identity))

(defmacro scope [desc & body]
  (let [term (deeper-scope-state
              (scope-root desc)
              (deeper-scope-state
               ~@body
               (deeper-scope-state
                (scope-termination desc))))]
    (reset-scope-seeds [term])
    term))





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
             (and (spec/valid? ::defs/requirement dep-key)
                  (= tag (defs/requirement-tag dep-key))))
           refs)))


(defn dep-tag-and-key [[k v]]
  (if (spec/valid? ::defs/requirement k)
    [defs/requirement-tag v]))

(defn depends-on-bifurcation? [seed tag bif-key]
  (assert (sd/seed? seed))
  (assert (contains? #{:true-branch :false-branch} tag))
  (assert (keyword? bif-key))
  (let [look-for [tag bif-key]]
    (->> seed
         sd/access-deps
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
    (exm/update-seed
     comp-state key-s
     (fn [s]
       (if (sd/compiled-seed? s)
         s
         (defs/compilation-result s [:marked-as-compiled key-s]))))
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
     (let [r (exm/lookup-compiled-results comp-state (sd/access-deps expr))
           refs (-> comp-state
                    exm/access-seed-to-compile
                    sd/referents)
           this-key (exm/access-seed-key comp-state)
           seed-sets (:seed-sets expr)
           true-top (:true-top seed-sets)
           false-top (:false-top seed-sets)
           comp-state (mark-compiled comp-state (:bif seed-sets))
           term (:term seed-sets)
           true-comp (compile-to-expr (exm/select-sub-tree comp-state true-top))
           false-comp (compile-to-expr (exm/select-sub-tree comp-state false-top))]
       (cb (-> comp-state
               (defs/compilation-result :compiled-bifurcate)

               ;; Mark everything, except the termination, as compiled.
               (mark-compiled (disj (:term-sub-keys seed-sets) term))

               ;; Put the result in the term node
               (exm/update-seed term #(access-hidden-result
                                   % (codegen-if
                                      (:condition r) true-comp false-comp)))
               
               exm/initialize-compilation-roots))))))



;;;;;; NOTE: A bifurcation should depend on all seeds that:
;;  (i) Are always compiled
;;  (ii) Used by any of the branches
;;
;;  When it compiles a branch, it should limit the scope to the seeds
;;  under the indirection for every branch.





(defn tweak-bifurcation [expr-map key seed]
  (let [;; All seeds referring to bifurcation
        refs (sd/referents seed)

        ;; The termination seed key
        term (sd/referent-with-key seed :bifurcation)

        ;; The termination seed
        term-seed (-> expr-map exm/seed-map term)

        ;; The dependencies of the termination seed
        term-seed-deps (-> term-seed sd/access-deps)

        ;; The top of the true/false branches
        true-top (-> term-seed-deps :true-branch)
        false-top (-> term-seed-deps :false-branch)

        ;; The sets of seed referring to the bifurcation from either branch
        true-refs (sd/filter-referents-of-seed seed (sd/dep-tagged? :true-branch))
        false-refs (sd/filter-referents-of-seed seed (sd/dep-tagged? :false-branch))

        ;; All deep dependencies of the if-termination
        term-sub-keys (set (exm/deep-seed-deps expr-map term))

        bif-refs (exm/traverse-expr-map
                  expr-map
                  key
                  sd/referent-neighbours
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
        (exm/update-seed
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
        (exm/add-expr-map-deps "eval-outside-if"
                               key
                               what-bif-should-depend-on))))

(defn bifurcate-on [condition]
  (with-new-seed
    "if-bifurcation"
    (fn [s]
      (-> s
          (sd/add-deps {:condition condition})
          (sd/add-tag :bifurcation)
          (sd/access-bind? false)
          (sd/access-pretweak tweak-bifurcation)
          (sd/compiler compile-bifurcate)))))

(defn compile-if-termination [comp-state expr cb]
  (cb (defs/compilation-result comp-state (access-hidden-result expr))))

(def access-original-type (party/key-accessor :original-type))

(def original-branch-type (comp access-original-type defs/result-value))

(defn mark-dont-bind [x]
  (sd/access-bind? x false))

(defn if-sub [settings
              bif
              input-dirty
              on-true-snapshot
              on-false-snapshot]
  (assert (defs/snapshot? on-true-snapshot))
  (assert (defs/snapshot? on-false-snapshot))

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
           
           branch-dirties (set [(defs/last-dirty on-true-snapshot)
                                (defs/last-dirty on-false-snapshot)])

           output-dirty? (not= #{input-dirty}
                               branch-dirties)
           
           termination (with-new-seed
                         "if-termination"
                         (fn [s]
                           (-> s
                               (sd/compiler compile-if-termination)
                               (sd/add-tag :if-termination)
                               (utils/cond-call (:dont-bind? settings) mark-dont-bind)
                               (sd/add-deps
                                {

                                 :bifurcation bif
                                 
                                 :true-branch (pack true-branch)
                                 
                                 :false-branch (pack false-branch)
                                 })
                               (sd/mark-dirty output-dirty?))))

           _ (assert (= (boolean output-dirty?)
                        (boolean (defs/dirty? termination))))

           ;; Wire the correct return dirty: If any of the branches produced a new dirty,
           ;; it means that this termination node is dirty.
           ;;
           ;; Otherwise, just return the input dirty


           output-dirty (if output-dirty?
                          termination
                          input-dirty)
           ret (-> {}
                   (defs/result-value (unpacker ret-type termination))
                   (defs/last-dirty output-dirty))]
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
  (cb (defs/compilation-result comp-state (access-bind-symbol expr))))

(defn make-loop-binding [comp-state lvar-key]
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

(defn bind-if-not-masked [mask value]
  (if mask
    (replace-by-local-var value)        ;; <-- assign a symbol to it, and we are going to use it.
    (sd/access-bind? (to-seed value) true) ;; <-- force it to be bound outside of the loop
    ))

(comment
  (defn tweak-bifurcation [expr-map key seed]
    (let [ ;; All seeds referring to bifurcation
          refs (sd/referents seed)

          ;; The termination seed key
          term (sd/referent-with-key seed :bifurcation)

          ;; The termination seed
          term-seed (-> expr-map exm/seed-map term)

          ;; The dependencies of the termination seed
          term-seed-deps (-> term-seed sd/access-deps)

          ;; The top of the true/false branches
          true-top (-> term-seed-deps :true-branch)
          false-top (-> term-seed-deps :false-branch)

          ;; The sets of seed referring to the bifurcation from either branch
          true-refs (sd/filter-referents-of-seed seed (sd/dep-tagged? :true-branch))
          false-refs (sd/filter-referents-of-seed seed (sd/dep-tagged? :false-branch))

          ;; All deep dependencies of the if-termination
          term-sub-keys (set (exm/deep-seed-deps expr-map term))

          bif-refs (exm/traverse-expr-map
                    expr-map
                    key
                    sd/referent-neighbours
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
          (exm/update-seed
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
          (exm/add-expr-map-deps "eval-outside-if"
                                 key
                                 what-bif-should-depend-on)))))

(defn tweak-loop [expr-map seed-key seed]
  (let [loop-binding-key (sd/find-dep seed (partial = :loop-binding))
        term-key (sd/referent-with-key seed :root)
        term-seed (-> expr-map exm/seed-map term-key)
        term-sub-keys (set (exm/deep-seed-deps expr-map term-key))
        root-refs (exm/traverse-expr-map
                  expr-map
                  loop-binding-key
                  sd/referent-neighbours
                  (fn [[k _]] (contains? term-sub-keys k)))


        eval-outside-loop (clojure.set/difference term-sub-keys
                                                  (set root-refs))]
    (-> expr-map
        (exm/update-seed
         seed-key
         (fn [x]
           (assoc x :seed-sets
                  {:term key
                   :term-sub-keys term-sub-keys})))
        (exm/add-expr-map-deps "eval-outside-loop"
                               loop-binding-key
                               eval-outside-loop))))

(comment
  (defn compile-bifurcate [comp-state expr cb]
    (flush-bindings
     comp-state
     (fn [comp-state]
       (let [r (exm/lookup-compiled-results comp-state (sd/access-deps expr))
             refs (-> comp-state
                      exm/access-seed-to-compile
                      sd/referents)
             this-key (exm/access-seed-key comp-state)
             seed-sets (:seed-sets expr)
             true-top (:true-top seed-sets)
             false-top (:false-top seed-sets)
             comp-state (mark-compiled comp-state (:bif seed-sets))
             term (:term seed-sets)
             true-comp (compile-to-expr (exm/select-sub-tree comp-state true-top))
             false-comp (compile-to-expr (exm/select-sub-tree comp-state false-top))]
         (cb (-> comp-state
                 (defs/compilation-result :compiled-bifurcate)

                 ;; Mark everything, except the termination, as compiled.
                 (mark-compiled (disj (:term-sub-keys seed-sets) term))

                 ;; Put the result in the term node
                 (exm/update-seed term #(access-hidden-result
                                     % (codegen-if
                                        (:condition r) true-comp false-comp)))
                 exm/initialize-compilation-roots)))))))

(def access-mask (party/key-accessor :mask))

(defn compile-loop-bindings [comp-state lvars]
  (reduce into [] (map (partial make-loop-binding comp-state) lvars)))

(defn compile-loop [comp-state seed cb]
  (flush-bindings
   comp-state
   (fn [comp-state]
     (let [deps (sd/access-deps seed)
           loop-binding (exm/get-seed comp-state (:loop-binding deps))
           lvars (sd/access-indexed-deps seed)
           mask (access-mask seed)
           this-key (exm/access-seed-key comp-state)
           term (sd/referent-with-key seed :root)
           comp-state (mark-compiled comp-state #{this-key})
           term-subtree (exm/select-sub-tree comp-state term)
           compiled-loop-body (compile-to-expr term-subtree)
           term-sub (set (exm/deep-seed-deps comp-state term))
           this-result `(loop ~(compile-loop-bindings comp-state lvars)
                          ~compiled-loop-body)]
       (cb (-> comp-state
               (defs/compilation-result this-result)
               (exm/update-seed term #(access-hidden-result % this-result))
               (mark-compiled (disj term-sub term))
               exm/initialize-compilation-roots))))))

(defn compile-loop-binding [comp-state expr cb]
  (cb (defs/compilation-result comp-state :loop-binding)))

(defn loop-binding []
  (with-new-seed
    "loop-binding"
    (fn [s]
      (-> s
          (sd/compiler compile-loop-binding)
          (sd/access-bind? false)))))

(defn loop-root [loop-binding mask initial-state]
  (with-new-seed
    "loop-root"
    (fn [s]
      (-> s
                                        ;(add-deps {:state initial-state})
          (sd/access-indexed-deps (flatten-expr initial-state))
          (sd/add-tag :loop-root)
          (sd/add-deps {:loop-binding loop-binding})
          (sd/access-bind? false)
          (access-mask mask)
          (sd/access-pretweak tweak-loop)
          (sd/compiler compile-loop)))))

(def access-loop? (party/key-accessor :loop?))

(defn compile-loop-test-condition [comp-state expr cb]
  (cb comp-state))

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

(def access-state-type (party/key-accessor :state-type))


(defn compile-loop-termination [comp-state expr cb]
  (if (has-hidden-result? expr)
    (cb
     (defs/compilation-result
      comp-state
      (access-hidden-result expr)))
    (let [rdeps (sd/access-compiled-deps expr)]
      (cb (defs/compilation-result
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
  (let [dirty-loop? (not= input-dirty (defs/last-dirty loop-if-snapshot))

        ;; Build the termination node
        term (with-new-seed
               "loop-termination"
               (fn [s]
                 (-> s
                     (access-state-type (type-signature return-value))
                     (sd/compiler compile-loop-termination)
                     (sd/access-bind? has-hidden-result?) ;; It has a recur inside
                     (sd/add-deps { ;; Structural pointer at the beginning of the loop
                                   :root root


                                   :if (terminate-snapshot
                                        input-dirty
                                        loop-if-snapshot)})
                     (sd/mark-dirty dirty-loop?))))]

    ;; Build a snapshot
    (-> {}
        (defs/result-value term)
        (defs/last-dirty (if dirty-loop? term input-dirty)))))

(defn unpack-loop-result [x]
  (unpack (access-state-type x) x))

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
