(ns geex.core2
  (:require [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]))

(declare make-seed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::platform any?)
(spec/def ::counter number?)
(spec/def ::seed-id ::counter)
(spec/def ::seed-map (spec/map-of ::seed-id ::defs/seed))
(spec/def ::seed-ref any?)
(spec/def ::injection-deps (spec/map-of ::seed-ref ::seed-id))
(spec/def ::state (spec/keys :req-un [::platform
                                      ::counter
                                      ::seed-map
                                      ::injection-deps]))
(spec/def ::seed-with-id (spec/and ::defs/seed
                                   (spec/keys :req-un [::seed-id])))

(spec/def ::state-and-output (spec/cat :state ::state
                                       :output any?))

(def state? (partial spec/valid? ::state))
(def state-and-output? (partial spec/valid? ::state-and-output))
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

(def ^:dynamic state-atom nil)

(defn swap-state! [f & args]
  (let [fargs (fn [x] (apply f (into [x] args)))]
    (swap! state-atom (comp ensure-state fargs ensure-state))))

(defn get-state []
  {:post [(state? %)]}
  (if (nil? state-atom)
    (throw (ex-info "No state bound, are you calling 
it outside of with-state?" {}))
    (deref state-atom)))

(defn step-counter [state]
  {:pre [(state? state)]
   :post [(state? %)]}
  (update state :counter inc))

(defn registered-seed? [x]
  (and (seed/seed? x)
       (contains? x :seed-id)))

;; Should take anything
(defn to-seed-in-state [state x]
  {:pre [(state? state)]
   :post [(state-and-output? %)]}
  (cond
    (registered-seed? x) [state x]
    (seed/seed? x) (make-seed state x)))

(defn import-deps [state seed-prototype]
  (let [deps (vec (seed/access-deps-or-empty seed-prototype))
        [state deps] (reduce
                      (fn [[state mapped-deps] [k v]]
                        (let [[state v] (to-seed-in-state )]))
                      [state []]
                      deps)]))

(defn register-seed [state seed-prototype]
  (let [[state seed-prototype] (import-deps state seed-prototype)]))

(defn make-seed [state seed-prototype]
  {:pre [(state? state)
         (defs/seed? seed-prototype)
         (not (registered-seed? seed-prototype))]
   :post [(spec/valid? ::state-and-output %)]}
  [(-> state
        step-counter
        (register-seed seed-prototype)) nil])

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
  (binding [state-atom (atom init-state)]
    (body-fn)
    (deref state-atom)))

;; Create a new seed state flushes the bindings
(defn flush-bindings! [])
