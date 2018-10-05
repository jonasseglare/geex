(ns geex.jcore
  (:import [geex State Seed SeedUtils DynamicSeed
            SeedParameters Mode])
  (:require [geex.core.defs :as defs]
            [geex.core :as clj-core]
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
        (.add dst-deps
              k (to-seed-in-state state ))))))

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

(defn to-seed-in-state [state x]
  {:post [(SeedUtils/isRegistered %)]}
  (cond
    (= x ::defs/nothing) (make-nothing state x)
    
    (registered-seed? x)
    (.addDependenciesFromDependingScopes state x)

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

(defn with-state-fn [init-state body-fn]
  {:pre [(fn? body-fn)
         (or (nil? init-state)
             (state? init-state))]}
  (binding [global-state (or init-state (State.))]
    (.setOutput global-state (body-fn))
    global-state))

(defmacro with-state [init-state & body]
  `(with-state-fn ~init-state (fn [] ~@body)))
