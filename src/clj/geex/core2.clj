(ns geex.core2
  (:require [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::platform any?)
(spec/def ::counter number?)
(spec/def ::seed-id ::counter)
(spec/def ::seed-map (spec/map-of ::seed-id ::defs/seed))
(spec/def ::injection-deps (spec/map-of any? ::seed-id))

(spec/def ::state (spec/keys :req-un [::platform
                                      ::counter
                                      ::seed-map
                                      ::injection-deps]))

(def state? (partial spec/valid? ::state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ensure-state [x]
  (assert (state? x))
  x)

;;;------- State operations -------
(def empty-state
  {:platform nil
   :counter 0
   :seed-map {}   
   :created-seeds []
   :injection-deps {}})

(def ^:dynamic state nil)

(defn swap-state! [f & args]
  (let [fargs (fn [x] (apply f (into [x] args)))]
    (swap! state (comp ensure-state fargs ensure-state))))

(defn get-state []
  {:post [(state? %)]}
  (if (nil? state)
    (throw (ex-info "No state bound, are you calling 
it outside of with-state?" {}))
    (deref state)))

(defn step-counter [state]
  {:pre [(state? state)]
   :post [(state? %)]}
  (update state :counter inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-injection-deps []
  (-> (get-state)
      :injection-deps))

(defn set-injection-deps! [new-deps]
  (swap-state! assoc :injection-deps new-deps))

(defn with-state [init-state body-fn]
  {:pre [(state? init-state)
         (fn? body-fn)]
   :post [(state? %)]}
  (binding [state (atom init-state)]
    (body-fn)
    (deref state)))
