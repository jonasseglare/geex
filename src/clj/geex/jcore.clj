(ns geex.jcore
  (:import [geex State Seed SeedUtils]))

(def ^:dynamic global-state nil)

(defn- get-state []
  (if (nil? global-state)
    (throw (ex-info "No state"
                    {}))
    global-state))

(defn to-seed-in-state [state x])

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
