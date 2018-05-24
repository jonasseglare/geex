(ns lime.java

  "Generation of Java backed code"

  (:require [lime.java.defs :as jdefs]
            [lime.core :as lime]
            [lime.platform.low :as low]
            [clojure.spec.alpha :as spec]
            [lime.core.seed :as seed]
            [lime.core :as core]
            [bluebell.utils.specutils :as specutils]
            [bluebell.utils.core :as utils]
            [clojure.string :as cljstr])
  (:import [org.codehaus.janino SimpleCompiler]))

(def platform-tag [:platform :java])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn janino-cook-and-load-class [class-name source-code]
  "Dynamically compile and load Java code as a class"
  [class-name source-code]
  (let [sc (SimpleCompiler.)]
    (.cook sc source-code)
    (.loadClass (.getClassLoader sc) class-name)))

(defn janino-cook-and-load-object  [class-name source-code]
  (.newInstance (janino-cook-and-load-class
                 class-name
                 source-code)))

(defn parse-typed-defn-args [args0]
  (specutils/force-conform ::jdefs/defn-args args0))

(defn java-class-name [parsed-args]
  (-> parsed-args
      :name
      name
      (cljstr/replace "-" "_")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (supers (class (fn [x] (* x x))))
;; #{java.lang.Runnable java.util.Comparator java.util.concurrent.Callable clojure.lang.IObj java.io.Serializable clojure.lang.AFunction clojure.lang.Fn clojure.lang.IFn clojure.lang.AFn java.lang.Object clojure.lang.IMeta}


(defn generate-typed-defn [args]
  `(let [[top# code#] (lime/top-and-code
                       [{:platform :java}]
                       (core/return-value (do ~@(:body args))))]
     (utils/indent-nested
      [~(str "public class " (java-class-name args) " {")
       ["public static " (low/get-type-signature platform-tag top#) " apply() {"
        code#
        "}"]
       "}"])))

(defmacro typed-defn [& args0]
  (let [args (parse-typed-defn-args args0)]
    (generate-typed-defn
     args
     )))

(comment
  (do

    (typed-defn return-primitive-number []
                1)
    
    ))
