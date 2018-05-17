(ns lime.java

  "Generation of Java backed code"

  (:require [lime.java.defs :as jdefs]
            [lime.core :as lime]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as specutils])
  (:import [org.codehaus.janino SimpleCompiler]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn janino-cook-and-load
  "Dynamically compile and load Java code as a class"
  [class-name source-code]
  (let [sc (SimpleCompiler.)]
    (.cook sc source-code)
    (let [cl (.loadClass (.getClassLoader sc) class-name)]
      (.newInstance cl))))

(defn parse-typed-defn-args [args0]
  (specutils/force-conform ::jdefs/defn-args args0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (supers (class (fn [x] (* x x))))
;; #{java.lang.Runnable java.util.Comparator java.util.concurrent.Callable clojure.lang.IObj java.io.Serializable clojure.lang.AFunction clojure.lang.Fn clojure.lang.IFn clojure.lang.AFn java.lang.Object clojure.lang.IMeta}



(defmacro typed-defn [& args0]
  (let [args (parse-typed-defn-args args0)]
    (str args)))
