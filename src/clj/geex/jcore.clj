(ns geex.jcore
  (:import [geex State Seed SeedUtils DynamicSeed
            SeedParameters Mode
            SeedFunction
            StateSettings
            ClojurePlatformFunctions
            TypedSeed])
  (:require [geex.core.defs :as defs]
            [geex.core.jvm :as gjvm]
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

(def typed-seed? (partial instance? TypedSeed))

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

(defn- declare-local-var [state]
  {:post [(int? %)]}
  (let [lvar (.declareLocalVar state)
        seed (make-reverse-seed
              state (declare-local-var-seed lvar))]
    (.getIndex lvar)))

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

(defn- get-local-var [state id]
  (let [lvar (.get (.getLocalVars state) id)
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

(defn- compile-get-var [state expr cb]
  (set-compilation-result
   state
   `(deref ~(xp/call :local-var-sym (.getData expr)))
   cb))

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

#_(defn set-local-struct!
  "Set a local variable holding a composite value."
  [id data]
  (set-local-struct (get-state) id data))

(defn set-compilation-result [state seed cb]
  (.setCompilationResult state seed)
  (cb state))

(defn populate-seeds-visitor
  [state x]
  (if (seed/seed? x)
    [(rest state) (first state)]
    [state x]))

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




(xp/register
 :clojure
 {:keyword-seed primitive-seed

  :default-expr-for-type (fn [x] nil)

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
})

nil
