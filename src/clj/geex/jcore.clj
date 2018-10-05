(ns geex.jcore
  (:import [geex State Seed SeedUtils DynamicSeed
            SeedParameters Mode])
  (:require [geex.core.defs :as defs]
            [geex.core :as clj-core]
            [geex.core.xplatform :as xp]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]))

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

(defn make-seed [state x0]
  (let [seed (ensure-seed x0)]
    (assert (SeedUtils/isRegistered seed))))

(defn make-nothing [state x]
  (make-seed
   state
   (doto (SeedParameters.)
     (set-field type ::defs/nothing)
     (set-field bind false)
     (set-field mode Mode/Pure)
     (set-field compiler (xp/caller :compile-nothing)))))

(defn to-seed-in-state [state x]
  {:post [(SeedUtils/isRegistered %)]}
  (cond
    (= x ::defs/nothing) (make-nothing state x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn state? [x]
  (instance? State x))

(defn to-seed [x]
  (to-seed-in-state (get-state) x))

(defn with-state [init-state body-fn]
  {:pre [(fn? body-fn)
         (or (nil? init-state)
             (state? init-state))]}
  (binding [global-state (or init-state (State.))]
    (body-fn)))
