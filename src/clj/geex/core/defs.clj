(ns geex.core.defs
  
  "Common definitions that are shared between different modules of the code."

  (:require [geex.core.data-indexer :as data-indexer]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The state used for book-keeping when generating code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def platform-indexer (data-indexer/indexer))

(def default-platform :clojure)

(def ^:dynamic the-platform default-platform)
(def ^:dynamic the-platform-index (data-indexer/index-of platform-indexer default-platform))

(def ^:dynamic gensym-counter nil)

(defn make-gensym-counter []
  (atom 0))


;; Keys are unique within a context. That way, we should always generate the same expression
;; for the same data, and can thus compare values for equality to see if something changed.
(defn contextual-gensym
  ([] (contextual-gensym "untagged"))
  ([prefix0]
   (let [prefix (str prefix0)]
     (assert (not (nil? gensym-counter)))
     (symbol (str "gs-" prefix "-" (swap! gensym-counter inc))))))

(defn new-or-existing-gensym-counter []
  (or gensym-counter
      (make-gensym-counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Platforms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-platform
  "Get the platform identifier, or :clojure if undefined."
  []
  the-platform)

(defmacro with-platform [p & body]
  `(binding [the-platform ~p
             the-platform-index (data-indexer/index-of platform-indexer ~p)]
     ~@body))
