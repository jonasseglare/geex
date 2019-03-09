(ns geex.core.defs
  
  "Common definitions that are shared between different modules of the code."

  (:import [geex State]))

(def ^:dynamic global-state nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The state used for book-keeping when generating code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-platform :clojure)

(def ^:dynamic the-platform default-platform)

;; Keys are unique within a context. That way, we should always generate the same expression
;; for the same data, and can thus compare values for equality to see if something changed.
(defn contextual-gensym
  ([] (contextual-gensym "untagged"))
  ([prefix0]
   (let [prefix (str prefix0)]
     (assert (not (nil? global-state)))
     (symbol (str "gs-" prefix
                  "-" (.generateSymbolIndex
                       global-state))))))

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
  `(binding [the-platform ~p]
     ~@body))
