(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as specutils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Spec
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::class-def-body (spec/or :fn fn?
                                    :vec (spec/coll-of ::class-def-body)))

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

(defn apply-body-to-class-def [class-def body]
  (if (coll? body)
    (reduce apply-body-to-class-def class-def body)
    (body class-def)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def private (partial with-visibility :private))
(def public (partial with-visibility :public))
(def protected (partial with-visibility :protected))

(defn static [& args]
  (fn [class-def]
    (with-key-value class-def :static? true
      (fn [class-def]
        (reduce apply-to-class-def class-def args)))))

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

(defmacro method [method-name arg-list & body]
  {:pre [(symbol? method-name)
         (vector? arg-list) ; <-- todo
         ]}
  `(fn [class-def#]
     (update class-def#
             :methods
             conj
             {:name (quote ~method-name)
              :static? (:static? class-def#)
              :visibility (:visibility class-def#)
              ;;; TODO!!!
              })))

(defmacro defclass [class-name & args]
  `(let [args# ~(vec args)]
     (specutils/validate ::class-def-body args#)
     (apply-body-to-class-def (init-class-def (quote ~class-name)) args#)))
