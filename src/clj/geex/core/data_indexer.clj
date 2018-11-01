(ns geex.core.data-indexer)


(defn- assign-index [data-map x]
  (if (contains? data-map x)
    data-map
    (assoc data-map x (count data-map))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn index-of [indexer x]
  (get (swap! indexer assign-index x) x))

(defn indexer []
  (atom {}))
