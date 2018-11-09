(ns geex.core
  (:import [geex State Seed SeedUtils DynamicSeed
            Binding
            SeedParameters Mode
            SeedFunction
            LocalVar
            StateSettings
            ClojurePlatformFunctions
            TypedSeed
            LocalStruct
            ContinueException
            CodeMap
            CodeItem]
           [java.util HashMap])
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
            [bluebell.utils.wip.java :as jutils :refer [set-field]]
            [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.type :as etype]
            [bluebell.utils.ebmd.ops :as ebmd-ops]
            [geex.ebmd.type :as gtype])
  (:refer-clojure :exclude [cast]))

;; (set! *warn-on-reflection* true)


(def check-debug false)

(def valid-flags #{:disp-final-state
                   :disp-initial-state
                   :disp-bind?
                   :disp-trace
                   :disp-generated-output
                   :disp
                   :disp-time})

(spec/def ::recur (spec/cat :prefix #{::recur}
                            :keys set?
                            :value any?))

(def recur? (partial spec/valid? ::recur))

(def ^:dynamic recur-keys nil)

(defn- register-recur [key]
  (if (nil? recur-keys)
    (throw (ex-info "Cannot call recur outside of loop"
                    {:key key}))
    (swap! recur-keys conj key)))

(defn- make-recur [keys value]
  {:pre [(set? keys)]}
  [::recur keys value])

(defn- unwrap-recur [x]
  (if (spec/valid? ::recur x)
    (last x)
    x))

(defn- get-recur-keys [r]
  (if (recur? r)
    (second r)
    #{}))


(declare wrap-recursive)
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

(defn- clojure-settings-for-state [_]
  (doto (StateSettings.)
    (set-field platformFunctions (ClojurePlatformFunctions.))
    (set-field platform :clojure)))

(defn make-clojure-state
  "Make a state, for debugging"
  []
  (State. (clojure-settings-for-state nil)))

(spec/def ::make-dynamic-seed-body
  (spec/cat :state (spec/? any?)
            :fields  (spec/* (spec/cat :field-name symbol?
                                       :field-value any?))))

(def required-dynamic-seed-fields
  '#{description})

(defn- parse-dynamic-seed-body [body]
  (let [parsed (spec/conform ::make-dynamic-seed-body body)
        field-syms (set (map :field-name (:fields parsed)))]
    (doseq [fs required-dynamic-seed-fields]
      (if (not (contains? field-syms fs))
        (throw (ex-info (str "Missing field " fs)
                        {}))))
    (if (= parsed ::spec/invalid)
      (throw (ex-info
              (str "Failed to parse dynamic seed body: "
                   (spec/explain-str ::make-dynamic-seed-body body))
              {:body body})))
    parsed))

(defn- render-seed-params [parsed]
  `(doto (SeedParameters.)
     ~@(mapv (fn [p] `(set-field ~(:field-name p)
                                 ~(:field-value p)))
             (:fields parsed))))

(defmacro make-dynamic-seed [& body]
  (let [parsed (parse-dynamic-seed-body body)]
    `(~@(if (contains? parsed :state)
          `(make-seed ~(:state parsed))
          `(make-seed!))
      ~(render-seed-params parsed))))

(defmacro make-seed-parameters [& body]
  (let [parsed (parse-dynamic-seed-body body)]
    (assert (not (contains? parsed :state)))
    (render-seed-params parsed)))

(defn make-state ^State [state-params]
  (if-let [platform (:platform state-params)]
    (State. (xp/call :settings-for-state state-params))
    (throw (ex-info "No platform specified"
                    {:params state-params}))))

(defn- in-state? []
  (not (nil? defs/global-state)))

(defn- ensure-seed [x]
  (cond
    (instance? SeedParameters x) (DynamicSeed. x)
    (instance? Seed x) x
    :default (throw (ex-info "Cannot make seed from " x))))

(defn- import-deps [state ^Seed seed]
  (let [src-deps (.getRawDeps seed)
        dst-deps (.deps seed)]
    (when (not (nil? src-deps))
      (doseq [[k v] src-deps]
        (.addDep dst-deps
                 k (to-seed-in-state state v))))))

(defn make-seed [^State state x0]
  (let [seed (ensure-seed x0)]
    (import-deps state seed)
    (.addSeed state seed false)
    seed))

(defn- make-reverse-seed [^State state x0]
  (let [^Seed seed (ensure-seed x0)]
    (assert (nil? (.getRawDeps seed)))
    (.addSeed state seed true)
    seed))

(defn- make-nothing [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "Nothing")
     (set-field type ::defs/nothing)
     (set-field bind false)
     (set-field mode Mode/Pure)
     (set-field compiler (xp/caller :compile-nothing)))))

(defn- class-seed [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "class-seed")
     (set-field type java.lang.Class)
     (set-field mode Mode/Pure)
     (set-field data {:class x})
     (set-field compiler (xp/caller :compile-class)))))

(defn- primitive? [x]
  (or (number? x)
      (string? x)
      (keyword? x)
      (symbol? x)
      (boolean? x)
      (nil? x)
      (char? x)))

(defn- value-literal-type [x]
  (if (symbol? x)
    nil
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

(defn- compile-forward-value [^State state
                              ^Seed seed cb]
  (let [v (-> seed .deps (.get :value))]
    (.setCompilationResult seed (.getCompilationResult v))
    (cb state)))

(defn- flush-bindings [^State state cb]
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

(defn- flush-seed [state x]
  (let [^Seed input (to-seed-in-state state x)]
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

(defn- compile-default-value [state expr cb]
  (set-compilation-result
   state
   (xp/call :default-expr-for-type
            (seed/datatype expr))
   cb))

(defn- decorate-typed-seed [x]
  (if (seed/typed-seed? x)
    (DynamicSeed.
     (doto (SeedParameters.)
       (set-field description "Default value seed")
       (set-field compiler compile-default-value)
       (set-field mode Mode/Pure)
       (set-field type (.getType ^Seed x))))
    x))

(defn- to-seed-in-state [^State state x]
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

(defn generate-code [^State state]
  (defs/with-platform (.getPlatform state)
    (.generateCode state)))

(defn- to-coll-expression [c]
  (if (seq? c)
    (cons 'list c)
    c))

(defn- compile-to-nothing [^State state
                           ^Seed seed cb]
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

(defn- end-seed [^State state
                 ^Seed x]
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

(defn- end-scope [^State state x]
  (let [begin-seed (.popScopeId state)
        ^Seed input-seed (to-seed-in-state state x)
        output (end-seed state input-seed)]
    (.setData begin-seed output)
    (.popScope state)
    output))

(defn- compile-local-var-seed [^State state
                               ^Seed seed cb]
  (let [sym (xp/call :local-var-sym (.getIndex ^LocalVar (.getData seed)))]
    `(let [~sym (atom nil)]
       ~(cb (seed/compilation-result state ::declare-local-var)))))

(defn- compile-set-local-var [^State state
                              ^Seed expr
                              cb]
  (let [lvar (.getData expr)
        sym (xp/call :local-var-sym (.getIndex ^LocalVar lvar))
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

(defn- declare-local-var-object [^State state]
  {:post [(instance? LocalVar %)]}
  (let [lvar (.declareLocalVar state)
        seed (make-reverse-seed
              state (declare-local-var-seed lvar))
        vs (.getLocalVarSection state)]
    (when (nil? vs)
      (throw (ex-info "No local var section" {})))
    (.addCounted (.deps vs) seed)
    lvar))

(defn- declare-local-var [^State state]
  {:post [(int? %)]}
  (.getIndex ^LocalVar (declare-local-var-object state)))

(defn declare-local-vars [state n]
  (take n (repeatedly #(declare-local-var-object state))))

(defn compile-local-var-section [state sd cb]
  (set-compilation-result
   state
   nil
   cb))

(defn local-var-section []
  (make-dynamic-seed
   mode Mode/Statement
   description "Local var section"
   compiler (xp/caller :compile-local-var-section)))


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

(defn- set-local-var [^State state var-id dst-value]
  {:pre [(state? state)
         (int? var-id)]}
  (let [^LocalVar lvar (.get (.getLocalVars state) var-id)]
    (if (typed-seed? dst-value)
      (.setType lvar (.getType ^Seed dst-value))
      (let [^Seed seed (to-seed-in-state state dst-value)
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

(defn- get-local-var-from-object [^State state
                                  ^LocalVar lvar]
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

(defn- get-local-var [^State state id]
  (let [lvar (.get (.getLocalVars state) id)]
    (get-local-var-from-object state lvar)))

(defn- compile-get-var [^State state ^Seed expr cb]
  (set-compilation-result
   state
   `(deref ~(xp/call :local-var-sym (.getData expr)))
   cb))

(defn- allocate-local-struct [^State state id input]
  (let [type-sig (type-signature input)]
    (if-let [^LocalStruct ls (.getLocalStruct state id)] 
      (if (not= type-sig (.getTypeSignature ls))
        (throw
         (ex-info
          (str
           "Inconsistent type signatures of local struct with id " id)
          {:current (.getTypeSignature ls)
           :new type-sig}))
        ls)
      (let [n (size-of type-sig)
            lvars (into-array
                   LocalVar
                   (declare-local-vars state n))]
        (.allocateLocalStruct state id type-sig lvars)))))

(defn- set-local-struct [^State state id input]
  (let [^LocalStruct ls (allocate-local-struct state id input)
        flat-input (flatten-expr input)
        lvars (.getFlatVars ls)]
    (assert (= (count lvars) (count flat-input)))
    (doseq [[^LocalVar lvar src-value] (map vector lvars flat-input)]
      (set-local-var state (.getIndex lvar) src-value))))

(defn- get-local-struct [^State state id]
  (if-let [^LocalStruct ls (.getLocalStruct state id)]
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

(defn- state-gensym [^State state]
  (xp/call :counter-to-sym (.generateSymbolIndex state)))

(defn- compile-if [state expr cb]
  (xp/call :compile-if state expr cb))

(defn if-sub [condition on-true on-false]
  (make-seed!
   (doto (SeedParameters.)
     (set-field description "If")
     (set-field mode Mode/Statement)
     (set-field compiler compile-if)
     (set-field rawDeps {:cond condition
                         :on-true on-true
                         :on-false on-false}))))

(defn- compile-bind-name [^State comp-state
                          ^Seed expr cb]
  (cb (seed/compilation-result comp-state
        (xp/call
         :compile-bind-name
         (.getData expr)))))

(defn- nil-seed [cl]
  (make-seed-parameters
   description "nil"
   mode Mode/Pure
   bind false
   type cl
   compiler (xp/get :compile-nil)))

(defn- compile-return-value [state seed cb]
  (let [dt (seed/datatype seed)
        compiled-expr (-> seed
                          seed/access-compiled-deps
                          :value)]
    (cb (seed/compilation-result
          state
          (xp/call
           :compile-return-value
           dt
           compiled-expr)))))

(def ^:dynamic loop-key nil)

(defn- compile-recur [state expr cb]
  (set-compilation-result
   state
   `(throw (ContinueException.))
   cb))

(defn- compile-loop2 [state expr cb]
  (let [deps (.getMap (.deps expr))
        ^Seed body  (-> deps :body)]
    (set-compilation-result
     state
     `(loop []
        (if (try
              ~(.getCompilationResult body)
              false
              (catch ContinueException e#
                true))
          (recur)))
     cb)))

(defn- recur-seed []
  (make-dynamic-seed
   description "recur"
   mode Mode/SideEffectful
   compiler (xp/caller :compile-recur)
   type nil
   ))


(defn- check-recur-tail-fn [body-fn loop-state]
  {:pre [(fn? body-fn)]}
  (binding [recur-keys (atom #{})]
    (let [result (body-fn loop-state)
          rkeys (deref recur-keys)]

      ;; Sanity checks
      (if (recur? result)
        (when (not= rkeys (get-recur-keys result))
          (throw (ex-info "Not all recurs were at tail position"
                          {:expected rkeys
                           :at-tail (get-recur-keys result)})))
        (when (not (empty? rkeys))
          (throw (ex-info "Recur not at tail position"
                          {:result result}))))
      
      (unwrap-recur result))))

(defn- make-loop-seed [^Seed body]
  {:pre [(seed/seed? body)]}
  (make-dynamic-seed
   description "loop"
   mode Mode/Statement
   rawDeps {:body body}
   compiler (xp/caller :compile-loop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-state ^State []
  (if (nil? defs/global-state)
    (throw (ex-info "No state"
                    {}))
    defs/global-state))

(defn with-local-var-section-fn [body-fn]
  (let [state (get-state)
        old (.getLocalVarSection state)
        _ (.setLocalVarSection state (local-var-section))
        result (body-fn)]
    (.setLocalVarSection state old)
    result))

(defmacro with-local-var-section [& body]
  `(with-local-var-section-fn (fn [] ~@body)))

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


;;;------- Advanced wrapping -------
(defn wrap-quote [x]
  [::wrap-quote x])

(ebmd/def-arg-spec wrap-quote-spec {:pred (fn [x] (and (vector? x)
                                                       (= ::wrap-quote (first x))))
                                    :pos [(wrap-quote 119)]
                                    :neg [119]})


;; For loops and branches, recursive wrapping happens automatically.
;; To prevent a value from being wrapped, call 'wrap-quote' on that value,
;; or implement a custom 'wrap-recursive' for that type.
(ebmd/declare-poly wrap-recursive)

(ebmd/declare-poly wrap-at-key?)

(ebmd/def-poly wrap-at-key? [etype/any x]
  true)

(ebmd/def-poly wrap-recursive [etype/any x]
  (wrap x))

(ebmd/def-poly wrap-recursive [wrap-quote-spec [_ x]]
  x)

(ebmd/def-poly wrap-recursive [etype/keyword x]
  x)

(ebmd/def-poly wrap-recursive [etype/sequential x]
  (mapv wrap-recursive x))

(ebmd/def-poly wrap-recursive [etype/map m]
  (into {}
        (map (fn [[k v]]
               [k (if (wrap-at-key? k)
                    (wrap-recursive v)
                    v)])
             m)))



(defn with-state-fn [state-params body-fn]
  {:pre [(fn? body-fn)]}
  (defs/with-platform (:platform state-params)
    (let [^State state (make-state state-params)]
      (binding [defs/global-state state
                                        ;defs/state state
                ]
        (.setOutput ^State defs/global-state (body-fn))
        defs/global-state))))

(defmacro with-state [init-state & body]
  `(with-state-fn ~init-state (fn [] ~@body)))

(defn flush! [x]
  (flush-seed (get-state) x))

(defn eval-body-fn
  "Introduce a current state from init-state, evaluate body-fn and then post-process the resulting state."
  [init-state body-fn]
  (let [^State state (with-state-fn init-state (comp flush! body-fn))]
    (doto state
      (.finalizeState))))

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

(defn set-compilation-result [^State state ^Seed seed cb]
  (.setCompilationResult state seed)
  (cb state))

(defn populate-seeds-visitor
  [state x]
  (if (seed/seed? x)
    [(rest state) (first state)]
    [state x]))

(defn dont-bind!
  "Indicate that a seed should not be bound."
  [^Seed x]
  {:pre [(seed? x)]}
  (.setBind x false)
  x)

(defn set-branch-result [rkeys k value]
  (if (recur? value)
    (swap! rkeys into (get-recur-keys value))
    (set-local-struct! k (wrap-recursive value))))

(defn maybe-wrap-recur [rkeys x]
  {:pre [(set? rkeys)]}
  (if (empty? rkeys)
    x
    (make-recur rkeys x)))

(defmacro If
  "If statement"
  [condition on-true on-false]
  `(let [cond# ~condition
         true-fn# (fn [] ~on-true)
         false-fn# (fn [] ~on-false)]
     (if (seed/seed? cond#)
       (let [rkeys# (atom #{})
             evaled-cond# (flush! (wrap cond#))
             key# (genkey!)]
         (if-sub evaled-cond#
                 (do (begin-scope!)
                     (set-branch-result rkeys# key# (true-fn#))
                     (dont-bind!
                      (end-scope! (flush! ::defs/nothing))))
                 (do (begin-scope!)
                     (set-branch-result rkeys# key# (false-fn#))
                     (dont-bind!
                      (end-scope!
                       (flush! ::defs/nothing)))))

         ;; Propagate recur.
         (maybe-wrap-recur
          (deref rkeys#)
          (get-local-struct! key#)))
       
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

(defn strip-seed [^Seed sd]
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
  (fn [^State state ^Seed seed cb]
    (.setCompilationResult seed code)
    (cb state)))




(defn Recur [& next-loop-state]
  (let [recur-key (genkey!)]
    (register-recur recur-key)
    (set-local-struct! loop-key (wrap-recursive next-loop-state))
    (recur-seed)
    (make-recur #{recur-key} nil)))

(defn fn-loop [initial-state loop-body-fn]
  {:pre [(sequential? initial-state)
         (fn? loop-body-fn)]}
  (let [result-key (genkey!)
        state-key (genkey!)
        wrapped (wrap-recursive initial-state)]
    (flush! (set-local-struct!
             state-key
             wrapped))
    (binding [loop-key state-key]
      (make-loop-seed
       (do (begin-scope! {:depending-scope? true})
           (let [loop-state (get-local-struct! state-key)
                 loop-output (check-recur-tail-fn
                              loop-body-fn loop-state)]
             (set-local-struct!
              result-key
              (unwrap-recur loop-output))
             (dont-bind!
              (end-scope!
               (flush! ::defs/nothing)))))))
    (get-local-struct! result-key)))

(defmacro Loop [& args0]
  (let [args (loopsp/parse-loop-args args0)
        bds (:bindings args)]
    `(fn-loop ~(mapv :expr bds)
              (fn [~(mapv :vars bds)]
                ~@(:body args)))))

(defn wrap-expr-compiler
  "Converts a function that returns the compiled result to a function that provides it to a callback."
  [c]
  {:pre [(fn? c)]}
  (fn [^State state ^Seed seed cb]
    (.setCompilationResult seed (c seed))
    (cb state)))

(defn add-top-code
  "Add code that should be statically evaluated before the block being compiled."
  [^State state key added-code]
  (.addTopCode state (CodeItem. key (fn [] added-code) nil))
  state)

(defn get-top-code [^State state]
  {:pre [(state? state)]}
  (-> state
      .getTopCode
      .getUnorderedCode
      vec))

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
     type (seed/datatype x)
     rawDeps {:value x}
     compiler compile-return-value)))

(defn basic-nil?
  "Test if a geex expression is nil."
  [x]
  (make-dynamic-seed
   description "nil?"
   mode Mode/Pure
   type Boolean/TYPE
   rawDeps {:value x}
   compiler (xp/get :compile-nil?)))

(def cast (xp/caller :cast))

(def contextual-gensym defs/contextual-gensym)

(def contextual-genkey (comp keyword contextual-gensym))

(def contextual-genstring (comp str contextual-gensym))

(defn to-indexed-map [x]
  {:pre [(sequential? x)]}
  (zipmap
   (range (count x))
   x))

(defmacro full-generate
  "Given Geex code, not only generate code but also return the state, the top expr, etc."
  [[settings] & code]
  `(let [log# (timelog/timelog)
         state# (eval-body-fn
                 (merge clojure-state-settings ~settings)
                 (fn [] ~@code))
         log# (timelog/log log# "Evaluated state")
         result# (generate-code state#)
         log# (timelog/log log# "Generated code")]
     (when (.hasFlag state# :disp-final-state)
       (.disp state#))
     {:result result#
      :state state#
      :timelog log#
      :expr (.getLastSeed state#)}))

(defn set-flag! [& flags]
  (let [state (get-state)]
    (doseq [flag flags]
      (assert (contains? valid-flags flag))
      (.setFlag state flag))))

(defn flag-set? [flag]
  {:pre [(contains? valid-flags flag)]}
  (.hasFlag (get-state) flag))

(defn with-modified-state-var-fn [key f body-fn]
  {:pre [(fn? f)
         (fn? body-fn)]}
  (let [state (get-state)
        var-map (.getVarMap state)
        old (.get var-map key)
        _ (.put var-map key (f old))
        result (body-fn)]
    (.put var-map key old)
    result))

(defmacro with-modified-state-var [key f & body-fn]
  `(with-modified-state-var-fn ~key ~f (fn [] ~@body-fn)))

(defmacro with-new-state-var [key v & body-fn]
  `(with-modified-state-var
     ~key
     (constantly ~v)
     ~@body-fn))

(defn get-state-var [key]
  (-> (get-state)
      .getVarMap
      (.get key)))

(xp/register
 :clojure
 {:keyword-seed primitive-seed

  :default-expr-for-type (fn [x] nil)

  :compile-nothing (constant-code-compiler nil)

  :symbol-seed primitive-seed

  :string-seed primitive-seed

  :make-nil #(primitive-seed % nil)

  :compile-static-value
  (fn  [state ^Seed seed cb]
    (.setCompilationResult seed (.getData seed))
    (cb state))

  :compile-coll2
  (fn [^State state ^Seed seed cb]
    (let [deps (vec (.compilationResultsToArray (.deps seed)))
          output-coll (partycoll/normalized-coll-accessor
                       (.getData seed)
                       deps)]
      (.setCompilationResult seed (to-coll-expression output-coll))
      (cb state)))

  :settings-for-state clojure-settings-for-state

  :render-bindings
  (fn [tail fn-body]
    `(let ~(reduce into []
                   (map (fn [^Binding x]
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

  :compile-if (fn [^State state
                   ^Seed expr cb]
                (let [deps (.getMap (.deps expr))
                      ^Seed cond-seed  (-> deps :cond)
                      ^Seed on-true-seed (-> deps :on-true)
                      ^Seed on-false-seed (-> deps :on-false)]
                  (set-compilation-result
                   state
                   `(if ~(.getCompilationResult cond-seed)
                      ~(.getCompilationResult on-true-seed)
                      ~(.getCompilationResult on-false-seed))
                   cb)))

  :compile-return-value
  (fn [datatype expr]
    (throw (ex-info "Return value not supported on this platform"
                    {:datatype datatype
                     :expr expr})))

  :compile-recur compile-recur
  :compile-loop compile-loop2
  :compile-local-var-section compile-local-var-section
})

nil
