(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Spec
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::settings map?)

(spec/def ::class (spec/alt :symbol symbol?
                            :class class?))

(spec/def ::classes (spec/* ::class))

(spec/def ::extends (spec/cat :prefix #{:extends}
                              :classes (spec/spec ::classes)))

(spec/def ::implements (spec/cat :prefix #{:implements}
                                 :classes (spec/spec ::classes)))

(spec/def ::visibility #{:private :public :protected})

(spec/def ::scope (spec/and vector?
                            (spec/spec (spec/cat :visibility ::visibility
                                                 :data ::class-data))))

(defn maybe-static [x]
  (spec/cat :static? (spec/? #{:static})
            :value x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init-class-def [name]
  {:name name
   :extends []
   :implements []
   :methods []
   :vars []
   :static? false
   :visibility :public})

(defn apply-to-class-def [class-def f]
  (f class-def))

(defn push-key-value [dst k body-fn]
  (let [backup (get dst k)]
    (assoc (body-fn dst) k backup)))

(defn with-key-value [dst k value body-fn]
  (push-key-value
   dst k
   (fn [dst]
     (body-fn (assoc dst k value)))))

(defn with-visibility [visibility & args]
  (fn [class-def]
    (with-key-value class-def :visibility visibility
      (fn [class-def]
        (reduce apply-to-class-def class-def args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def private (partial with-visibility :private))
(def public (partial with-visibility :public))
(def protected (partial with-visibility :protected))

(defmacro variable [var-type var-symbol]
  {:pre [(symbol? var-symbol)]}
  `(fn [class-def#]
     (update class-def#
             :vars
             conj
             {:symbol (quote ~var-symbol)
              :type ~var-type
              :visibility (:visibility class-def#)
              :static? (:static? class-def#)})))

(defmacro defclass [& args]
  `(let [evaled-args# [~@args]]
     ))
