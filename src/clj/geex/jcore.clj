(ns geex.jcore
  (:import [geex State Seed SeedUtils DynamicSeed
            SeedParameters Mode
            SeedFunction
            StateSettings
            ClojurePlatformFunctions])
  (:require [geex.core.defs :as defs]
            [geex.core :as clj-core]
            [geex.core.seed :as seed]
            [bluebell.utils.wip.party :as party]
            [bluebell.utils.wip.party.coll :as partycoll]
            [geex.core.datatypes :as datatypes]
            [geex.core.xplatform :as xp]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]))

(def check-debug false)

(declare to-seed-in-state)
(declare seed?)
(declare registered-seed?)
(declare state?)

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
    (.addSeed state seed)
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
  (let [bds (.bindings (.localVars state))]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (.declareLocalVar (get-state)))










(xp/register
 :clojure
 {:keyword-seed primitive-seed

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

  
})

nil
