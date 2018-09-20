(ns geex.core2
  (:require [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [geex.core :as old-core]
            [bluebell.utils.wip.specutils :as specutils]
            [geex.core.xplatform :as xp]))

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
(spec/def ::output any?)

(spec/def ::state (spec/keys :req-un [::platform
                                      ::counter
                                      ::seed-map
                                      ::injection-deps
                                      ::output]))
(spec/def ::seed-with-id (spec/and ::defs/seed
                                   (spec/keys :req-un [::seed-id])))

(spec/def ::state-and-output (spec/cat :state ::state
                                       :output any?))

(def state? (partial spec/valid? ::state))
(def state-and-output? (partial spec/valid? ::state-and-output))
(def seed-map? (specutils/pred ::seed-map))
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
  {:output nil
   :platform nil
   ;; Used to assign ids to seeds
   :counter 0

   ;; All the seeds
   :seed-map {}

   ;; A list of created seeds so far
   :created-seeds []

   ;; Dependencies that new seeds should have
   :injection-deps {}})

(def ^:dynamic state-atom nil)

(defn wrap-f-args [f args]
  (fn [x] (apply f (into [x] args))))

(defn swap-state! [f & args]
  (let [fargs (wrap-f-args f args)]
    (swap! state-atom (comp ensure-state fargs ensure-state))))

(defn put-in-output [[state output]]
  (assoc state :output output))

(defn get-output [state]
  {:pre [(state? state)]}
  (:output state))

(defn swap-with-output! [f & args]
  (get-output (swap-state! (comp put-in-output (wrap-f-args f args)))))

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

(defn get-counter [state]
  {:pre [(state? state)]}
  (:counter state))

(defn registered-seed? [x]
  (and (seed/seed? x)
       (contains? x :seed-id)))

(defn set-seed-id [x id]
  {:pre [(seed/seed? x)
         (spec/valid? ::seed-id id)]}
  (assoc x :seed-id id))

(defn primitive-seed [state x]
  (make-seed
   state
   (-> {}
       (seed/access-bind? false)
       (seed/static-value x)
       (defs/datatype (old-core/value-literal-type x))
       (seed/compiler (xp/get :compile-static-value)))))

;; Should take anything
(defn to-seed-in-state [state x]
  {:pre [(state? state)]
   :post [(state-and-output? %)
          (registered-seed? (second %))]}
  (cond
    (registered-seed? x) [state x]
    (seed/seed? x) (make-seed state x)
    :default (primitive-seed state x)))

(defn import-deps
  "Replace the deps of the seed by keys, and "
  [state seed-prototype]
  {:pre [(state? state)
         (seed/seed? seed-prototype)]}
  (let [deps (vec (seed/access-deps-or-empty seed-prototype))
        [state deps] (reduce
                      (fn [[state mapped-deps] [k v]]
                        (let [[state v]
                              (to-seed-in-state state v)]
                          [state (conj mapped-deps [k v])]))
                      [state []]
                      deps)]
    (assert (empty? deps))
    [state seed-prototype]))

(defn make-seed [state seed0]
  {:pre [(state? state)
         (defs/seed? seed0)
         (not (registered-seed? seed0))]
   :post [(spec/valid? ::state-and-output %)]}
  (let [state (step-counter state)
        id (get-counter state)
        seed0 (merge {::defs/deps {}} (set-seed-id seed0 id))
        [state seed0] (import-deps state seed0)
        state (update state :seed-map conj [id seed0])]
    (spec/explain ::state-and-output [state seed0])
    [state seed0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn seed-map [state]
  {:pre [(state? state)]
   :post [(seed-map? %)]}
  (:seed-map state))

(defn make-seed! [seed-prototype]
  (swap-with-output! make-seed seed-prototype))

;; Call this before evaluating a subscope, so that we can restore things
(defn get-injection-deps []
  (-> (get-state)
      :injection-deps))

;; Call this after evaluating a subscope
(defn set-injection-deps! [new-deps]
  {:pre [(map? new-deps)]}
  (swap-state! assoc :injection-deps new-deps))

(defn with-state [init-state body-fn]
  {:pre [(state? init-state)
         (fn? body-fn)]
   :post [(state? %)]}
  (binding [state-atom (atom init-state)]
    (let [body-ressult (body-fn)]
      (deref state-atom))))

(defn to-seed [x]
  (swap-with-output! to-seed-in-state x))

(def wrap to-seed)

;; Create a new seed state flushes the bindings
#_(defn flush-bindings! []
#_  (assert false))

;; Should take all seeds created up to now,
;; create a new seed that depend on them,
;; clear the :created-seeds vector
;; make the new the seed the constant dependency. (::order)
;; This seed also flushes the bindings
(defn wrap-up! [& extra-deps]
  (assert false))
