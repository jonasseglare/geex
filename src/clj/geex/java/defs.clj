(ns geex.java.defs
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.specutils :as specutils]
            [geex.core.datatypes :as dt]))

(spec/def ::typed-argument (spec/cat :type any?
                                     :name symbol?))

(defn parsed-typed-argument? [x]
  (and (map? x)
       (contains? x :type)
       (contains? x :name)))

(defn parsed-typed-arguments? [x]
  (and (or (sequential? x)
           (nil? x))
       (every? parsed-typed-argument? x)))

(spec/def ::typed-arguments (spec/spec (spec/* ::typed-argument)))

(spec/def ::defn-args (spec/cat :name symbol?
                                :arglist ::typed-arguments
                                :body (spec/* any?)))

(defn parsed-defn-args? [x]
  (and (map? x)
       (parsed-typed-arguments? (:arglist x))
       (symbol? (:name x))
       (let [b (:body x)]
         (or (sequential? b)
             (nil? b)))))

;; https://docs.oracle.com/javase/tutorial/java/nutsandbolts/opsummary.html

(def binary-math-operators ["+" "-" "*" "/"])

(def comparison-operators  ["<" "<=" ">=" ">" "==" "!="])

(def logical-operators ["&&" "||" "!"])

(def bit-operators ["~" 
                    "<<" 
                    ">>" 
                    ">>>" 
                    "&" 
                    "^" 
                    "|"])

(def boolean-result (constantly Boolean/TYPE))

(defn make-operator-info-map [result-fn operators]
  (into {} (map (fn [s]
                  [s {:result-fn result-fn
                      :name s}])
                operators)))



(def operator-info-map
  (merge
   
   (make-operator-info-map
    dt/math-op-result-type
    binary-math-operators)
   
   (make-operator-info-map
    boolean-result
    comparison-operators)

   (make-operator-info-map
    boolean-result
    logical-operators)

   (make-operator-info-map
    dt/bit-op-result-type
    bit-operators)))


(spec/def ::math-fn-decl (spec/cat :key (spec/? keyword?)
                                   :java-name string?
                                   :arg-count (spec/? number?)))

(defn normalize-math-fn-decl [sp]
  (let [conformed (specutils/force-conform ::math-fn-decl (if (string? sp) [sp] sp))]
    [(or (:key conformed)
         (keyword (:java-name conformed)))
     (:java-name conformed)
     (or (:arg-count conformed)
         1)]))

(def math-functions
  (mapv normalize-math-fn-decl
        ["asin"
         "atan"
         ["atan2" 2]
         "cbrt"
         "ceil"
         ["copySign" 2]
         "cos"
         "cosh"
         "exp"
         "expm1"
         "floor"
         ["floorDiv" 2]
         ["floorMod" 2]
         "getExponent"
         ["hypot" 2]
         "log"
         "log10"
         "log1p"
         [:math-min "max" 2]
         [:math-max "min" 2]
         "nextDown"
         "nextUp"
         "negateExact"
         "multiplyExact"
         ["pow" 2]
         "rint"
         "round"
         ["scalb" 2]
         "signum"
         "sin"
         "sinh"
         "signum"
         "sqrt"
         "tan"
         "tanh"
         "toDegrees"
         "toIntExact"
         "toRadians"
         "ulp"
         ]))
