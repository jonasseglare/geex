(ns geex.jcore
  (:import [geex State Seed SeedUtils DynamicSeed
            SeedParameters Mode
            SeedFunction
            StateSettings
            ClojurePlatformFunctions
            TypedSeed])
  (:require [geex.core.defs :as defs]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.check :refer [checked-defn]]
            [geex.core.jvm :as gjvm]
            [bluebell.utils.wip.timelog :as timelog]
            [geex.core.loop :as loopsp]
            [geex.core.seed :as seed]
            [bluebell.utils.wip.party :as party]
            [bluebell.utils.wip.party.coll :as partycoll]
            [geex.core.datatypes :as datatypes]
            [geex.core.xplatform :as xp]
            [bluebell.utils.wip.traverse :as traverse]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]))

(def check-debug false)

(declare to-seed-in-state)
(declare seed?)
(declare registered-seed?)
(declare state?)
(declare set-compilation-result)
(declare to-seed)
(declare type-signature)
(declare size-of)
(declare flatten-expr)
(declare populate-seeds)
(declare make-seed!)
(declare genkey!)
(declare flush!)
(declare set-local-struct!)
(declare get-local-struct!)
(declare begin-scope!)
(declare end-scope!)
(declare dont-bind!)
(declare wrap)

(def typed-seed? (partial instance? TypedSeed))

(spec/def ::make-dynamic-seed-body
  (spec/cat :state (spec/? any?)
            :fields  (spec/* (spec/cat :field-name symbol?
                                       :field-value any?))))

(defmacro make-dynamic-seed [& body]
  (let [parsed (spec/conform ::make-dynamic-seed-body body)]
    (if (= parsed ::spec/invalid)
      (throw (ex-info
              (str "Failed to parse dynamic seed body: "
                   (spec/explain-str ::make-dynamic-seed-body body))
              {:body body})))
    `(~@(if (contains? parsed :state)
          `(make-seed ~(:state parsed))
          `(make-seed!))
      (doto (SeedParameters.)
        ~@(mapv (fn [p] `(set-field ~(:field-name p)
                                    ~(:field-value p)))
                (:fields parsed))))))

(defn make-state [state-params]
  (if-let [platform (:platform state-params)]
    (State. (xp/call :settings-for-state state-params))
    (throw (ex-info "No platform specified"
                    {:params state-params}))))

(defmacro with-gensym-counter
  "Introduce an atom holding a counter for gensym as a dynamically bound var."
  [& body]
  `(binding [defs/gensym-counter
             (defs/new-or-existing-gensym-counter)]
     ~@body))

(def ^:dynamic global-state nil)

(defn- get-state []
  (if (nil? global-state)
    (throw (ex-info "No state"
                    {}))
    global-state))

(defn ensure-seed [x]
  (cond
    (instance? SeedParameters x) (DynamicSeed. x)
    (instance? Seed x) x
    :default (throw (ex-info "Cannot make seed from " x))))

(defn import-deps [state seed]
  (let [src-deps (.getRawDeps seed)
        dst-deps (.deps seed)]
    (when (not (nil? src-deps))
      (doseq [[k v] src-deps]
        (.addDep dst-deps
                 k (to-seed-in-state state v))))))

(defn make-seed [state x0]
  (let [seed (ensure-seed x0)]
    (import-deps state seed)
    (.addSeed state seed false)
    seed))

(defn make-reverse-seed [state x0]
  (let [seed (ensure-seed x0)]
    (assert (nil? (.getRawDeps seed)))
    (.addSeed state seed true)
    seed))

(defn make-nothing [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "Nothing")
     (set-field type ::defs/nothing)
     (set-field bind false)
     (set-field mode Mode/Pure)
     (set-field compiler (xp/caller :compile-nothing)))))

(defn class-seed [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "class-seed")
     (set-field type java.lang.Class)
     (set-field mode Mode/Pure)
     (set-field data {:class x})
     (set-field compiler (xp/caller :compile-class)))))

(defn primitive? [x]
  (or (number? x)
      (string? x)
      (keyword? x)
      (symbol? x)
      (boolean? x)
      (nil? x)
      (char? x)))

(defn- value-literal-type [x]
  (if (symbol? x)
    defs/dynamic-type
    (datatypes/unboxed-class-of x)))

(defn- primitive-seed [state x]
  {:post [(registered-seed? %)]}
  (when (not (primitive? x))
    (throw (ex-info "Not a primitive"
                    {:x x})))
  (let [cleaned-type (value-literal-type x)]
    (make-seed
     state
     (doto (SeedParameters.)
       (set-field description (str "primitive " x))
       (set-field mode Mode/Pure)
       (set-field bind false)
       (set-field data x)
       (set-field type cleaned-type)
       (set-field compiler (xp/get :compile-static-value))))))

(defn- compile-forward-value [state seed cb]
  (let [v (-> seed .deps (.get :value))]
    (.setCompilationResult seed (.getCompilationResult v))
    (cb state)))

(defn- flush-bindings [state cb]
  (let [bds (.bindings (.localBindings state))]
    (if (.isEmpty bds)
      (cb state)
      (xp/call
       :render-bindings
       bds
       (fn []
         (.clear bds)
         (cb state))))))

(defn- compile-flush [state seed cb]
  (flush-bindings
   state
   (fn [state]
     (compile-forward-value state seed cb))))

(defn flush-seed [state x]
  (let [input (to-seed-in-state state x)]
    (make-seed
     state
     (doto (SeedParameters.)
       (set-field description "flush")
       (set-field seedFunction SeedFunction/Bind)
       
       ;; It is pure, but has special status of :bind,
       ;; so it cannot be optimized away easily
       (set-field mode Mode/Pure)

       (set-field rawDeps {:value input})

       (set-field type (.getType input))
       (set-field compiler compile-flush)))))

(defn- coll-seed [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description (str "Collection of type " (empty x)))
     (set-field mode Mode/Pure)
     (set-field rawDeps (seed/access-indexed-map
                         {}
                         (partycoll/normalized-coll-accessor x)))
     
     (set-field data x)
     (set-field type (xp/call :get-compilable-type-signature x))
     (set-field compiler (xp/get :compile-coll2)))))

(defn compile-default-value [state expr cb]
  (set-compilation-result
   state
   (xp/call :default-expr-for-type
            (seed/datatype expr))
   cb))

(defn decorate-typed-seed [x]
  (if (seed/typed-seed? x)
    (DynamicSeed.
     (doto (SeedParameters.)
       (set-field description "Default value seed")
       (set-field compiler compile-default-value)
       (set-field mode Mode/Pure)
       (set-field type (.getType x))))
    x))

(defn to-seed-in-state [state x]
  {:post [(seed? %)
          (SeedUtils/isRegistered %)]}
  (cond
    (= x ::defs/nothing) (make-nothing state x)
    
    (registered-seed? x) (do
                           (.addDependenciesFromDependingScopes
                            state x)
                           x)

    (class? x) (class-seed state x)

    (fn? x) (throw
             (ex-info
              "Don't know how to turn a function into a seed"
              {:fn x}))

    (nil? x) (xp/call :make-nil state)
    (seed/seed? x) (make-seed state (decorate-typed-seed x))
    
    (coll? x) (coll-seed state x)
    (keyword? x) (xp/call :keyword-seed state x)
    (symbol? x) (xp/call :symbol-seed state x)
    (string? x) (xp/call :string-seed state x)
    (primitive? x) (primitive-seed state x)

    :default (throw (ex-info "Cannot create seed from this"
                             {:x x}))))

(defn generate-code [state]
  (binding [defs/the-platform (.getPlatform state)]
    (.generateCode state)))

(defn- to-coll-expression [c]
  (if (seq? c)
    (cons 'list c)
    c))

(defn- compile-to-nothing [state seed cb]
  (.setCompilationResult seed ::defs/nothing)
  (cb state))

(defn- begin-seed [state]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "begin")
     (set-field type nil)
     (set-field mode Mode/Undefined)
     (set-field seedFunction SeedFunction/Begin)
     (set-field compiler compile-to-nothing))))

(defn- end-seed [state x]
  {:pre [(state? state)
         (seed? x)]
   :post [(seed? %)]}
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "end")
     (set-field type (.getType x))
     (set-field rawDeps {:value x})
     (set-field mode (.maxMode state))
     (set-field seedFunction SeedFunction/End)
     (set-field compiler compile-forward-value))))

(defn- end-scope [state x]
  (let [begin-seed (.popScopeId state)
        input-seed (to-seed-in-state state x)
        output (end-seed state input-seed)]
    (.setData begin-seed output)
    (.popScope state)
    output))

(defn- compile-local-var-seed [state seed cb]
  (let [sym (xp/call :local-var-sym (.getIndex (.getData seed)))]
    `(let [~sym (atom nil)]
       ~(cb (defs/compilation-result state ::declare-local-var)))))

(defn- compile-set-local-var [state expr cb]
  (let [lvar (.getData expr)
        sym (xp/call :local-var-sym (.getIndex lvar))
        deps (.deps expr)
        v (.getCompilationResult (.get deps :value))]
    (set-compilation-result
      state
      `(reset! ~sym ~v)
      cb)))

(defn- declare-local-var-seed [lvar]
  (doto (SeedParameters.)
    (set-field data lvar)
    (set-field mode Mode/Pure)
    (set-field type nil)
    (set-field description "Local var declaration")
    (set-field compiler (xp/caller :compile-local-var-seed))))

(defn- declare-local-var-object [state]
  (let [lvar (.declareLocalVar state)
        seed (make-reverse-seed
              state (declare-local-var-seed lvar))]
    lvar))

(defn- declare-local-var [state]
  {:post [(int? %)]}
  (.getIndex (declare-local-var-object state)))

(defn declare-local-vars [state n]
  (take n (repeatedly #(declare-local-var-object state))))

(defn counter-to-str [counter] (str "sym" counter))

(defn lvar-str-for-seed [seed]
  {:pre [(contains? seed :seed-id)]}
  (let [id (:seed-id seed)]
    (format
     "s%s%03d%s"
     (if (< id 0) "m" "")
     id
     (if-let [i (::lvar-counter seed)]
       (str "_" i)
       ""))))

(defn local-var-str [id]
  (str "lvar" id))

(defn- set-local-var [state var-id dst-value]
  {:pre [(state? state)
         (int? var-id)]}
  (let [lvar (.get (.getLocalVars state) var-id)]
    (if (typed-seed? dst-value)
      (.setType lvar dst-value)
      (let [seed (to-seed-in-state state dst-value)
            tp (.getType seed)]
        (.setType lvar tp)
        (make-seed
         state
         (doto (SeedParameters.)
           (set-field type nil)
           (set-field description
                      (str "Set local var of type " tp))
           (set-field data lvar)
           (set-field mode Mode/SideEffectful)
           (set-field rawDeps {:value seed})
           (set-field compiler (xp/caller :compile-set-local-var))))
        nil))))

(defn- get-local-var-from-object [state lvar]
  (let [id (.getIndex lvar)
        tp (.getType lvar)]
    (if (not (.isPresent tp))
      (throw (ex-info
              (str "No type information for var with id " 
                   id)
              {})))
    (make-seed
     state
     (doto (SeedParameters.)
       (set-field type (.get tp))
       (set-field description (str "Get var id " id))
       (set-field mode Mode/Ordered)
       (set-field compiler (xp/caller :compile-get-var))
       (set-field data id)))))

(defn- get-local-var [state id]
  (let [lvar (.get (.getLocalVars state) id)]
    (get-local-var-from-object state lvar)))

(defn- compile-get-var [state expr cb]
  (set-compilation-result
   state
   `(deref ~(xp/call :local-var-sym (.getData expr)))
   cb))

(defn- allocate-local-struct [state id input]
  (let [type-sig (type-signature input)]
    (if-let [ls (.getLocalStruct state id)] 
      (if (not= type-sig (.getTypeSignature ls))
        (throw
         (ex-info
          (str
           "Inconsistent type signatures of local struct with id " id)
          {:current (.getTypeSignature ls)
           :new type-sig}))
        ls)
      (let [n (size-of type-sig)
            lvars (into-array (declare-local-vars state n))]
        (.allocateLocalStruct
         state id type-sig lvars)))))

(defn- set-local-struct [state id input]
  (let [ls (allocate-local-struct state id input)
        flat-input (flatten-expr input)
        lvars (.getFlatVars ls)]
    (assert (= (count lvars) (count flat-input)))
    (doseq [[lvar src-value] (map vector lvars flat-input)]
      (set-local-var state (.getIndex lvar) src-value))))

(defn- get-local-struct [state id]
  (if-let [ls (.getLocalStruct state id)]
    (let [type-sig (.getTypeSignature ls)
          flat-vars (.getFlatVars ls)]
      (populate-seeds
       type-sig
       (map (partial get-local-var-from-object state) flat-vars)))
    (throw (ex-info (str "No local struct at id " id)
                    {}))))


(def ^:dynamic access-no-deeper-than-seeds
  (party/wrap-accessor
   {:desc "access-no-deeper-than-seeds"
    :getter (fn [x] (if (seed/seed? x)
                      []
                      x))
    :setter (fn [x y] (if (seed/seed? x)
                        x
                        y))}))

(def ^:dynamic top-seeds-accessor
  (party/chain
   access-no-deeper-than-seeds
   partycoll/normalized-coll-accessor))


(defn- selective-conj-mapping-visitor [pred-fn f]
  (fn [state x0]
    (let [x (if (symbol? x0)
              (to-seed x0)
              x0)]
      (if (pred-fn x)
        [(conj state x) (f x)]
        [state x]))))

(defn- state-gensym [state]
  (xp/call :counter-to-sym (.generateSymbolIndex state)))

(defn- compile-if [state expr cb]
  (xp/call :compile-if state expr cb))

(defn if-sub [condition on-true on-false]
  (make-seed!
   (doto (SeedParameters.)
     (set-field description "If")
     (set-field mode Mode/SideEffectful)
     (set-field compiler compile-if)
     (set-field type nil)
     (set-field rawDeps {:cond condition
                         :on-true on-true
                         :on-false on-false}))))

(defn- compile-loop [state expr cb]
  (let [deps (.getMap (.deps expr))
        body (:body deps)
        body-result (.getCompilationResult body)]
    (set-compilation-result
     state
     `(loop []
        (when ~(-> deps :body .getCompilationResult)
          (recur)))
     cb)))

(defn- loop-sub [body]
  (make-seed!
   (doto (SeedParameters.)
     (set-field description "Loop")
     (set-field rawDeps {:body body})
     (set-field mode Mode/SideEffectful)
     (set-field type nil)
     (set-field compiler compile-loop))))

(defn- compile-bind-name [comp-state expr cb]
  (cb (defs/compilation-result comp-state
        (xp/call
         :compile-bind-name
         (defs/access-name expr)))))

(defn- nil-seed [cl]
  (make-dynamic-seed
   mode Mode/Pure
   bind false
   type cl
   compiler (xp/get :compile-nil)))

(defn- compile-return-value [state seed cb]
  (let [dt (seed/datatype seed)
        compiled-expr (-> seed
                          seed/access-compiled-deps
                          :value)]
    (cb (defs/compilation-result
          state
          (xp/call
           :compile-return-value
           dt
           compiled-expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nil-of
  "Create a Geex nil value of a particular type."
  ([state cl]
   (make-seed state (nil-seed cl)))
  ([cl]
   (make-seed! (nil-seed cl))))


(defn gensym! []
  (state-gensym (get-state)))

(defn genkey! []
  (keyword (gensym!)))

(def clojure-state-settings {:platform :clojure})

(defn seed? [x]
  (instance? Seed x))

(defn make-seed! [x]
  (make-seed (get-state) x))

(defn registered-seed? [x]
  (and (seed? x)
       (SeedUtils/isRegistered x)))

(defn state? [x]
  (instance? State x))

(defn to-seed [x]
  (to-seed-in-state (get-state) x))

(def wrap to-seed)

(defn with-state-fn [state-params body-fn]
  {:pre [(fn? body-fn)]}
  (binding [defs/the-platform (:platform state-params)]
    (let [state (make-state state-params)]
      (binding [global-state state
                defs/state state]
        (.setOutput global-state (body-fn))
        global-state))))

(defmacro with-state [init-state & body]
  `(with-state-fn ~init-state (fn [] ~@body)))

(defn flush! [x]
  (flush-seed (get-state) x))

(defn eval-body-fn
  "Introduce a current state from init-state, evaluate body-fn and then post-process the resulting state."
  [init-state body-fn]
  (doto (with-state-fn init-state (comp flush! body-fn))
    (.finalizeState)))

(defmacro eval-body [init-state & body]
  `(eval-body-fn ~init-state (fn [] ~@body)))

(defmacro demo-embed [& code]
  "Embed code that will be evaluated."
  (let [body-fn (eval `(fn [] ~@code))
        state (eval-body-fn clojure-state-settings body-fn)
        ;_ (.disp state)
        code (generate-code state)]
    code))

(defmacro generate-and-eval
  "Generate code and evaluate it."
  [& code]
  `(->> (fn [] ~@code)
        (eval-body-fn clojure-state-settings)
        generate-code
        eval))

(defn begin-scope!
  ([]
   (begin-scope! {}))
  ([opts]
   (let [state (get-state)
         seed (begin-seed state)]
     (.beginScope state seed (if (:depending-scope? opts)
                               true false))
     seed)))

(defn end-scope! [x]
  (end-scope (get-state) x))

(defn declare-local-var! []
  (declare-local-var (get-state)))

(defn set-local-var! [var-id input]
  (set-local-var (get-state) var-id input))

(defn get-local-var! [id]
  (get-local-var (get-state) id))

(defn set-local-struct!
  "Set a local variable holding a composite value."
  [id data]
  (set-local-struct (get-state) id data))

(defn get-local-struct! [id]
  (get-local-struct (get-state) id))

(defn set-compilation-result [state seed cb]
  (.setCompilationResult state seed)
  (cb state))

(defn populate-seeds-visitor
  [state x]
  (if (seed/seed? x)
    [(rest state) (first state)]
    [state x]))

(defn dont-bind!
  "Indicate that a seed should not be bound."
  [x]
  {:pre [(seed? x)]}
  (.setBind x false)
  x)

(defmacro If
  "If statement"
  [condition on-true on-false]
  `(let [cond# ~condition
         true-fn# (fn [] ~on-true)
         false-fn# (fn [] ~on-false)]
     (if (seed/seed? cond#)
       (let [evaled-cond# (flush! (wrap cond#))
             key# (genkey!)]
         (if-sub evaled-cond#
                 (do (begin-scope!)
                     (set-local-struct! key# (true-fn#))
                     (dont-bind!
                      (end-scope! (flush! ::defs/nothing))))
                 (do (begin-scope!)
                     (set-local-struct! key# (false-fn#))
                     (dont-bind!
                      (end-scope!
                       (flush! ::defs/nothing)))))
         (get-local-struct! key#))
       (if cond#
         (true-fn#)
         (false-fn#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Datastructure traversal
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defn strip-seed [sd]
  {:pre [(seed? sd)]}
  (TypedSeed. (.getType sd)))

;; Get a datastructure that represents this type.
(defn type-signature
  "Compute an expression that encodes the type of the input expression."
  [x]
  (second
   (flat-seeds-traverse
    seed/seed?
    x
    strip-seed)))

;; Get only the seeds, in a vector, in the order they appear
;; when traversing. Opposite of populate-seeds
(defn flatten-expr
  "Convert a nested expression to a vector of seeds"
  [x]
  (let [p (flat-seeds-traverse seed/seed? x identity)]
    (first p)))

(def size-of (comp count flatten-expr))

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
    (assert (every? seed/seed? dst))
    (populate-seeds expr dst)))

(defn typed-seed [tp]
  (TypedSeed. tp))

(defn constant-code-compiler
  "Creates a compiler function for a seed, that always compiles to a constant expression."
  [code]
  (fn [state seed cb]
    (.setCompilationResult seed code)
    (cb state)))




(defn loop0-impl [init-state prep loop? next]
  (let [key (genkey!)]
    (flush! (set-local-struct! key init-state))
    (loop-sub
     (do (begin-scope! {:depending-scope? true})
         (let [x (get-local-struct! key)
               p (prep x)]
           (dont-bind!
            (end-scope!
             (flush!
              
              (If
               (loop? p)
               (do (set-local-struct! key (next p))
                   (wrap true))
               (do (wrap false))) ))))))
    (get-local-struct! key)))

(checked-defn
 loop0
 [::loopsp/init init-state
  ::loopsp/eval prep
  ::loopsp/loop? loop?
  ::loopsp/next next-state]
 (xp/call :loop0 init-state prep loop? next-state))

(checked-defn
 basic-loop
 [::loopsp/args bloop]
 ((:result bloop)
  (loop0 (:init bloop)
         (:eval bloop)
         (:loop? bloop)
         (:next bloop))))

(defn wrap-expr-compiler
  "Converts a function that returns the compiled result to a function that provides it to a callback."
  [c]
  {:pre [(fn? c)]}
  (fn [state seed cb]
    (.setCompilationResult seed (c seed))
    (cb state)))

(defn add-static-code
  "Add code that should be statically evaluated before the block being compiled."
  [state added-code]
  (.addStaticCode state added-code)
  state)

(defn get-static-code [state]
  (vec (.getStaticCode state)))

(defn bind-name
  "Bind a name to some variable."
  [datatype binding-name]
  (make-dynamic-seed
   description "bind-name"
   mode Mode/SideEffectful
   type datatype
   data binding-name
   bind false
   compiler compile-bind-name))

(defn return-value
  "Geex expression to return a value."
  [x0]
  (let [x (to-seed x0)]
    (make-dynamic-seed
     description "return-value"
     mode Mode/SideEffectful
     bind false
     type (defs/datatype x)
     rawDeps {:value x}
     compiler compile-return-value)))

(def contextual-gensym defs/contextual-gensym)

(def contextual-genkey (comp keyword contextual-gensym))

(def contextual-genstring (comp str contextual-gensym))

(defn to-indexed-map [x]
  {:pre [(sequential? x)]}
  (zipmap
   (range (count x))
   x))

(defmacro with-gensym-counter
  "Introduce an atom holding a counter for gensym as a dynamically bound var."
  [& body]
  `(binding [defs/gensym-counter
             (defs/new-or-existing-gensym-counter)]
     ~@body))

(defmacro full-generate
  "Given Geex code, not only generate code but also return the state, the top expr, etc."
  [[settings] & code]
  `(with-gensym-counter
     (let [log# (timelog/timelog)
           state# (eval-body-fn
                   (merge clojure-state-settings ~settings)
                   (fn [] ~@code))
           log# (timelog/log log# "Evaluated state")
           result# (generate-code state#)
           log# (timelog/log log# "Generated code")]
       {:result result#
        :state state#
        :timelog log#
        :expr (.getLastSeed state#)})))

(xp/register
 :clojure
 {:keyword-seed primitive-seed

  :default-expr-for-type (fn [x] nil)

  :loop0 loop0-impl  

  :compile-nothing (constant-code-compiler nil)

  :symbol-seed primitive-seed

  :string-seed primitive-seed

  :make-nil #(primitive-seed % nil)

  :compile-static-value
  (fn  [state seed cb]
    (.setCompilationResult seed (.getData seed))
    (cb state))

  :compile-coll2
  (fn [state seed cb]
    (let [deps (vec (.compilationResultsToArray (.deps seed)))
          output-coll (partycoll/normalized-coll-accessor
                       (.getData seed)
                       deps)]
      (.setCompilationResult seed (to-coll-expression output-coll))
      (cb state)))

  :settings-for-state
  (fn [state-params]
    (doto (StateSettings.)
      (set-field platformFunctions (ClojurePlatformFunctions.))
      (set-field platform :clojure)))

  :render-bindings
  (fn [tail fn-body]
    `(let ~(reduce into []
                   (map (fn [x]
                          [(symbol (.varName x)) (.value x)])
                        tail))
       ~(fn-body)))

  :local-var-sym (comp symbol local-var-str)
  :compile-local-var-seed compile-local-var-seed
  :compile-set-local-var compile-set-local-var
  :compile-get-var compile-get-var

  :get-compilable-type-signature
  gjvm/get-compilable-type-signature

  :counter-to-sym (comp symbol counter-to-str)

  :compile-if (fn [state expr cb]
                (let [deps (.getMap (.deps expr))]
                  (set-compilation-result
                   state
                   `(if ~(-> deps :cond .getCompilationResult)
                      ~(-> deps :on-true .getCompilationResult)
                      ~(-> deps :on-false .getCompilationResult))
                   cb)))

  :compile-return-value
  (fn [datatype expr]
    (throw (ex-info "Return value not supported on this platform"
                    {:datatype datatype
                     :expr expr})))

})

nil
