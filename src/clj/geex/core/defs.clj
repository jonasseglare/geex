(ns geex.core.defs
  
  "Common definitions that are shared between different modules of the code."
  
  (:require [bluebell.utils.wip.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.specutils :as specutils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The state used for book-keeping when generating code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-platform :clojure)

(def ^:dynamic the-platform default-platform)

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
;;;  Various definitions with no or little logic related to them
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Access the datatype of the seed
(defn datatype [x]
  (.getType x))

(defn compilation-result
  ([state x]
   (.setCompilationResult state x)
   state)
  ([state]
   (.getCompilationResult state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Platforms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def clojure-platform :clojure)
(def java-platform :java)
(def jvm-bytecode-platform :jvm-bytecode)
(def js-platform :js)


(defn get-platform
  "Get the platform identifier, or :clojure if undefined."
  []
  the-platform)


(defn platform-dispatch
  "This function can be used as a dispatch function when we write platform specific code."
  [& args]
  (get-platform))

(defn get-platform-tag []
  [:platform (get-platform)])
