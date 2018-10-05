(ns geex.jcore
  (:import [geex State Seed SeedUtils DynamicSeed
            SeedParameters Mode
            SeedFunction])
  (:require [geex.core.defs :as defs]
            [geex.core :as clj-core]
            [geex.core.seed :as seed]
            [geex.core.datatypes :as datatypes]
            [geex.core.xplatform :as xp]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]))

(declare to-seed-in-state)
(declare seed?)
(declare registered-seed?)


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
       (set-field data (seed/static-value {} x))
       (set-field type cleaned-type)
       (set-field compiler (xp/get :compile-static-value))))))

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
       (set-field compiler (fn [] (assert false)))))))

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
    (keyword? x) (xp/call :keyword-seed state x)
    (symbol? x) (xp/call :symbol-seed state x)
    (string? x) (xp/call :string-seed state x)
    (primitive? x) (primitive-seed state x)

    :default (throw (ex-info "Cannot create seed from this"
                             {:x x}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seed? [x]
  (instance? Seed x))

(defn registered-seed? [x]
  (and (seed? x)
       (SeedUtils/isRegistered x)))

(defn state? [x]
  (instance? State x))

(defn to-seed [x]
  (to-seed-in-state (get-state) x))

(def wrap to-seed)

(defn with-state-fn [init-state body-fn]
  {:pre [(fn? body-fn)
         (or (nil? init-state)
             (state? init-state))]}
  (binding [global-state (or init-state (State.))]
    (.setOutput global-state (body-fn))
    global-state))

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








(xp/register
 :clojure
 {:keyword-seed primitive-seed

  :symbol-seed primitive-seed

  :string-seed primitive-seed

  :make-nil #(primitive-seed % nil)})

nil
