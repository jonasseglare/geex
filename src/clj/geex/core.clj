(ns geex.core
  (:import [geex State ISeed SeedUtils DynamicSeed
            Binding
            SeedParameters Mode
            LocalVar
            StateSettings
            TypedSeed
            LocalStruct
            ContinueException
            CodeMap
            CodeItem]
           [java.util HashMap ArrayList])
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

(def valid-flags #{:disp-state
                   :disp-trace
                   :disp-compilation-results
                   :disp
                   :disp-time
                   :format})

(spec/def ::recur (spec/cat :prefix #{::recur}
                            :keys set?
                            :value (spec/? any?)))

(def recur? (partial spec/valid? ::recur))

(def ^:dynamic recur-keys nil)

(defn- register-recur [key]
  (if (nil? recur-keys)
    (throw (ex-info "Cannot call recur outside of loop"
                    {:key key}))
    (swap! recur-keys conj key)))

(defn- make-recur
  ([keys value]
   {:pre [(set? keys)]}
   [::recur keys value])
  ([keys]
   {:pre [(set? keys)]}
   [::recur keys]))

(defn recur-has-value? [x]
  (= (count x) 3))

(defn- unwrap-recur [x]
  (if (spec/valid? ::recur x)
    (do (when (not (recur-has-value? x))
          (throw (ex-info "No value in recur!\n
Possible reasons:\n
  * You are only calling recur and not returning a value\n
  * The loop stop condition always evaluates to true\n"
                          {:recur x})))
        (last x))
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
(declare to-seed)
(declare type-signature)
(declare size-of)
(declare flatten-expr)
(declare populate-seeds)
(declare make-seed!)
(declare genkey!)
(declare set-local-struct!)
(declare get-local-struct!)
(declare dont-list!)
(declare wrap)

(def typed-seed? (partial instance? TypedSeed))

(defn ordered-indexed-deps [seed]
  (map second
       (sort-by
        first
        (transduce
         (filter (fn [[k v]] (number? k)))
         conj
         []
         (into {} (.getMap (seed/access-deps seed)))))))

(defn- to-binding [^ISeed x]
  (let [state (.getState x)]
    (if (.isListed state)
      (do
        [(if (.isBound state)
           (.getKey state)
           (gensym "not-bound"))
         (.getValue state)])
      [])))

(defn- close-scope-fn [state x]
  (let [deps (ordered-indexed-deps x)]
    (if (empty? deps) nil
        (let [bindings
              (reduce into
                      []
                      (map to-binding (butlast deps)))
              final-result (.getValue (.getState (last deps)))]
          (if (empty? bindings)
            final-result
            `(let ~bindings
               ~final-result))))))

(defn- gen-seed-sym [^ISeed x]
  (symbol (format "s%04d" (.getId x))))

(defn- clojure-settings-for-state [_]
  (doto (StateSettings.)
    (set-field platform :clojure)
    (set-field closeScope close-scope-fn)
    (set-field generateSeedSymbol gen-seed-sym)))

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
    (instance? ISeed x) x
    :default (throw (ex-info "Cannot make seed from " x))))

(defn- import-deps [state ^ISeed seed]
  (let [src-deps (.getRawDeps seed)
        dst-deps (.deps seed)]
    (when (not (nil? src-deps))
      (doseq [[k v] src-deps]
        (.addDep dst-deps
                 k (to-seed-in-state state v))))))

(defn make-seed [^State state x0]
  (let [seed (ensure-seed x0)]
    (import-deps state seed)
    (.addSeed state seed)
    seed))

(defn- make-nothing [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field description "Nothing")
     (set-field type ::defs/nothing)
     (set-field bind false)
     (set-field mode Mode/Pure)
     (set-field hasValue false)
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
                              ^ISeed seed cb]
  (let [v (-> seed .deps (.get :value))]
    (.setCompilationResult seed (.getCompilationResult v))
    (cb state)))

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

(defn- compile-default-value [state expr]
  (xp/call :default-expr-for-type
           (seed/datatype expr)))

(defn- decorate-typed-seed [x]
  (if (seed/typed-seed? x)
    (DynamicSeed.
     (doto (SeedParameters.)
       (set-field description "Default value seed")
       (set-field compiler compile-default-value)
       (set-field mode Mode/Pure)
       (set-field type (.getType ^ISeed x))))
    x))

(defn- to-seed-in-state [^State state x]
  {:post [(seed? %)
          (SeedUtils/isRegistered %)]}
  (cond
    (= x ::defs/nothing) (make-nothing state x)
    
    (registered-seed? x) (do
                           #_(.addDependenciesFromDependingScopes
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
    (char? x) (xp/call :char-seed state x)
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
                           ^ISeed seed cb]
  (.setCompilationResult seed ::defs/nothing)
  (cb state))

(defn- compile-set-local-var [^State state
                              ^ISeed expr]
  (let [lvar (.getData expr)
        sym (xp/call :local-var-sym (.getIndex ^LocalVar lvar))
        deps (.deps expr)
        v (.getCompilationResult
           (.getState (.get deps :value)))]
    `(reset! ~sym ~v)))

(defn- declare-local-var-object
  "Returns a LocalVar object"
  [^State state]
  {:post [(instance? LocalVar %)]}
  (.declareLocalVar state))

(defn- declare-local-var [^State state]
  {:post [(int? %)]}
  (.getIndex ^LocalVar (declare-local-var-object state)))

(defn declare-local-vars [state n]
  (take n (repeatedly #(declare-local-var-object state))))

;;; Used by 
(defn- local-var-binding [^LocalVar lvar]
  (let [sym (xp/call :local-var-sym (.getIndex lvar))]
    [sym `(atom nil)]))

(defn compile-local-var-section [state ^ISeed sd]
  (let [bindings (map local-var-binding (.getData sd))
        compiled-deps (seed/access-compiled-deps sd)]
    `(let ~(reduce into [] bindings)
       ~(:result compiled-deps))))

(defn local-var-section [result local-vars]
  (make-dynamic-seed
   mode Mode/SideEffectful
   hasValue false
   data local-vars
   description "Local var section"
   rawDeps {:result result}
   type (seed/datatype result)
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
      (.setType lvar (.getType ^ISeed dst-value))
      (let [^ISeed seed (to-seed-in-state state dst-value)
            tp (.getType seed)]
        (.setType lvar tp)
        (make-seed
         state
         (doto (SeedParameters.)
           (set-field type nil)
           (set-field hasValue false)
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

(defn- compile-get-var [^State state ^ISeed expr]
  `(deref ~(xp/call :local-var-sym (.getData expr))))

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

(defn- local-struct-to-data [^State state
                             ^LocalStruct ls]
  (let [type-sig (.getTypeSignature ls)
        flat-vars (.getFlatVars ls)]
    (populate-seeds
     type-sig
     (map (partial get-local-var-from-object state) flat-vars))))

(defn- get-local-struct [^State state id]
  (if-let [^LocalStruct ls (.getLocalStruct state id)]
    (local-struct-to-data state ls)
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

(defn- compile-if [state expr]
  (xp/call :compile-if state expr))

(defn if-sub [condition on-true on-false]
  (make-seed!
   (doto (SeedParameters.)
     (set-field description "If")
     (set-field mode Mode/SideEffectful)
     (set-field hasValue false)
     (set-field compiler compile-if)
     (set-field rawDeps {:cond condition
                         :on-true on-true
                         :on-false on-false}))))

(defn- compile-bind-name [^State comp-state
                          ^ISeed expr]
  (xp/call
   :compile-bind-name
   (.getData expr)))

(defn- nil-seed [cl]
  (make-seed-parameters
   description "nil"
   mode Mode/Pure
   bind false
   type cl
   compiler (xp/get :compile-nil)))

(defn- compile-return-value [state seed]
  (let [dps (seed/deps-map seed)
        ^ISeed value (:value dps)]
    (xp/call :compile-return-value value)))

(def ^:dynamic loop-key nil)

;; This is super-duper-hacky! The Clojure backend is
;; just for testing, though.
(defn- compile-recur [state expr]
  `(throw (ContinueException.)))

(defn- compile-loop2 [state expr]
  (let [deps (.getMap (.deps expr))
        ^ISeed body  (-> deps :body)]
    `(loop []
       (if (try
             ~(seed/compilation-result body)
             false
             (catch ContinueException e#
               true))
         (recur)))))

(defn- recur-seed []
  (make-dynamic-seed
   description "recur"
   mode Mode/SideEffectful
   compiler (xp/caller :compile-recur)
   hasValue false
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

(defn- make-loop-seed [^ISeed body]
  {:pre [(seed/seed? body)]}
  (make-dynamic-seed
   description "loop"
   mode Mode/SideEffectful
   hasValue false
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


;; open-scope! and close-scope! are extremely low level.
;; Don't use them directly.
(defn open-scope! []
  (.openScope (get-state)))

;; We must not expose seeds
;; before close-scope!, because
;; they may compile to symbols that are not visible outside
;; of the scope.
(defn close-scope! []
  (.closeScope (get-state)))

(defn scoped-do-fn [f]
  (open-scope!)
  (f)
  (close-scope!))

;; You may want to call 'wrap' around the body to make sure that
;; the last result gets properly returned.
(defmacro scoped-do [& body]
  `(scoped-do-fn (fn [] ~@body)))

(defn with-local-var-section-fn [body-fn]
  (let [state (get-state)
        old (.scopedLocalVars state)
        local-vars (ArrayList.)
        _ (set! (.scopedLocalVars state) local-vars)
        result (dont-list! (scoped-do (body-fn)))]
    (set! (.scopedLocalVars state) old)
    (local-var-section result local-vars)))

;;; Remember to 'wrap' the last result if necessary.
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
  (instance? ISeed x))

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

(ebmd/def-poly wrap-recursive [::etype/fn x]
  x)

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
      (binding [defs/global-state state]
        (body-fn)
        defs/global-state))))

(defmacro with-state [init-state & body]
  `(with-state-fn ~init-state (fn [] ~@body)))

(defn eval-body-fn
  "Introduce a current state from init-state, evaluate body-fn and then post-process the resulting state."
  [init-state body-fn]
  (let [^State state (with-state-fn init-state body-fn)]
    (doto state
      (.finalizeState))))

(defmacro eval-body [init-state & body]
  `(eval-body-fn ~init-state (fn [] ~@body)))

(defn demo-code-fn [body-fn]
  (let [state (eval-body-fn
               clojure-state-settings
               body-fn)]
    (generate-code state)))

(defn demo-embed-fn [body-fn]
  (eval (demo-code-fn body-fn)))

(defmacro demo-code [& code]
  "Embed code that will be evaluated."
  `(demo-code-fn (fn [] ~@code)))

(defmacro demo-embed [& code]
  "Embed code that will be evaluated."
  (demo-code-fn (eval `(fn [] ~@code))))

(defmacro generate-and-eval
  "Generate code and evaluate it."
  [& code]
  `(->> (fn [] ~@code)
        (eval-body-fn clojure-state-settings)
        generate-code
        eval))

(defn declare-local-var! []
  (declare-local-var (get-state)))

(defn set-local-var! [var-id input]
  (set-local-var (get-state) var-id input))

(defn get-local-var! [id]
  (get-local-var (get-state) id))



(defn set-local-struct!
  "Set a local variable holding a composite value."
  [id data]
  (if (not= data ::undefined)
    (set-local-struct (get-state) id data)))

(defn get-local-struct! [id]
  (get-local-struct (get-state) id))

(defn populate-seeds-visitor
  [state x]
  (if (seed/seed? x)
    [(rest state) (first state)]
    [state x]))

(defn dont-list!
  "Indicate that a seed should not be bound."
  [^ISeed x]
  {:pre [(seed? x)]}
  (.setBind x false)
  x)

(defn list!
  [^ISeed x]
  (.setBind x true)
  x)

(defn set-branch-result [rkeys k value]
  (if (recur? value)
    (do (when (recur-has-value? value)
          (set-local-struct! k (unwrap-recur value)))
        (swap! rkeys into (get-recur-keys value)))
    (set-local-struct! k (wrap-recursive value))))

(defn maybe-wrap-recur [rkeys k]
  {:pre [(set? rkeys)]}
  (let [state (get-state)
        ls (.getLocalStruct state k)
        n (count rkeys)]
    (if (nil? ls)
      (do
        (make-recur rkeys))
      (let [x (local-struct-to-data state ls)]
        (if (= 0 n)
          x
          (make-recur rkeys x))))))

;; See the if statements
(defn perform-branch [branch-data code-fn]
  {:pre [(fn? code-fn)]}
  (let [rkeys (:rkeys branch-data)
        key (:key branch-data)]
    (dont-list!
     (scoped-do
      (set-branch-result rkeys key (code-fn))))))


(defn with-branching-code [inner-fn]
  (let [rkeys (atom #{})
        key (genkey!)]
    (inner-fn {:rkeys rkeys
               :key key})
    (maybe-wrap-recur (deref rkeys) key)))

(defmacro If
  "If statement"
  [condition on-true on-false]
  `(let [cond# ~condition
         true-fn# (fn [] ~on-true)
         false-fn# (fn [] ~on-false)]
     (if (seed/seed? cond#)
       
       (with-branching-code
         (fn [branch-data#]
           (let [evaled-cond# (wrap cond#)]
             (if-sub
              evaled-cond#
              (perform-branch branch-data# true-fn#)
              (perform-branch branch-data# false-fn#)))))
       
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

(defn strip-seed [^ISeed sd]
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


(defn Recur [& next-loop-state]
  (let [recur-key (genkey!)]
    (register-recur recur-key)
    (set-local-struct! loop-key (wrap-recursive next-loop-state))
    (recur-seed)
    (make-recur #{recur-key})))

(defn fn-loop [initial-state loop-body-fn]
  {:pre [(sequential? initial-state)
         (fn? loop-body-fn)]}
  (let [result-key (genkey!)
        state-key (genkey!)
        wrapped (wrap-recursive initial-state)]
    (set-local-struct! state-key wrapped)
    (binding [loop-key state-key]
      (make-loop-seed
       (do (open-scope!)
           (let [loop-state (get-local-struct! state-key)
                 loop-output (check-recur-tail-fn
                              loop-body-fn loop-state)]
             (set-local-struct!
              result-key
              (unwrap-recur loop-output))
             (wrap ::defs/nothing)
             (dont-list! (close-scope!))))))
    (get-local-struct! result-key)))

(defmacro Loop [& args0]
  (let [args (loopsp/parse-loop-args args0)
        bds (:bindings args)]
    `(fn-loop ~(mapv :expr bds)
              (fn [~(mapv :vars bds)]
                ~@(:body args)))))

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
     hasValue false
     type (seed/datatype x) ;; This is not exactly true...
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
     (when (.hasFlag state# :disp-state)
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

  :compile-nothing (constantly nil)

  :symbol-seed primitive-seed

  :string-seed primitive-seed

  :make-nil #(primitive-seed % nil)

  :compile-static-value
  (fn  [state ^ISeed seed]
    (.getData seed))

  :compile-coll2
  (fn [^State state ^ISeed seed]
    (let [deps (vec (.compilationResultsToArray (.deps seed)))
          output-coll (partycoll/normalized-coll-accessor
                       (.getData seed)
                       deps)]
      (to-coll-expression output-coll)))

  :settings-for-state clojure-settings-for-state

  :local-var-sym (comp symbol local-var-str)
  :compile-set-local-var compile-set-local-var
  :compile-get-var compile-get-var

  :get-compilable-type-signature
  gjvm/get-compilable-type-signature

  :counter-to-sym (comp symbol counter-to-str)

  :compile-if (fn [^State state
                   ^ISeed expr]
                (let [deps (.getMap (.deps expr))
                      ^ISeed cond-seed  (-> deps :cond)
                      ^ISeed on-true-seed (-> deps :on-true)
                      ^ISeed on-false-seed (-> deps :on-false)]
                  `(if ~(seed/compilation-result cond-seed)
                     ~(seed/compilation-result on-true-seed)
                     ~(seed/compilation-result on-false-seed))))

  :compile-return-value
  (fn [value-seed]
    (throw (ex-info "Return value not supported on this platform"
                    {:value value-seed})))

  :compile-recur compile-recur
  :compile-loop compile-loop2
  :compile-local-var-section compile-local-var-section
})

;; Is this a good idea?
(ebmd/register-promotion ::gtype/seed
                         wrap
                         ::etype/any
                         10)

nil
