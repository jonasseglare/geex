(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.dsl :as dsl]
            [bluebell.utils.specutils :as specutils]))


(declare public)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def empty-class-def
  {:name nil
   :extends []
   :implements []
   :methods []
   :variables []
   :current-settings {:static? false
                      :visibility :public}
   })

(defn try-assoc [dst k v]
  (if (map? dst)
    (assoc dst k v)
    dst))

(defn conjer [key data]
  (fn [state]
    (update state key conj
            (try-assoc data :settings (:current-settings state)))))

(defn method-sub [method-data]
  (conjer :methods method-data))

(defn var-sub [var-data]
  (conjer :variables var-data))

(defn intoer [key values]
  (fn [state] (update state key into values)))

(def visibilities #{:public :private :protected})

(defn class-spec-sub [name body]
  (fn [state]
    (dsl/do-body
     (update (assoc state :settings (:current-settings state))
             :name (fn [old-name]
                     {:pre [(nil? old-name)]}
                     name))
     body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro class-spec [name & body]
  `(class-spec-sub (quote ~name) ~(vec body)))

(defmacro method [name arglist & body]
  {:pre (symbol? name)}
  `(method-sub {:name (quote ~name)}))

(defmacro variable [type name]
  {:pre [(symbol? name)]}
  `(var-sub {:name (quote ~name)
             :type type}))

(defn implements [& interfaces]
    {:pre [(every? class? interfaces)]}
    (intoer :implements interfaces))

(defn extends [& classes]
  {:pre [(every? class? classes)]}
  (intoer :extends classes))

(defn static [& body]
  (fn [state]
    (dsl/with-updated
      state
      (dsl/path :current-settings :static?)
      (constantly true)
      body)))

(defn visibility [v & body]
  {:pre [(contains? visibilities v)]}
  (fn [state]
    (dsl/with-updated
      state
      (dsl/path :current-settings :visibility)
      (constantly v)
      body)))

(def private (partial visibility :private))
(def public (partial visibility :public))
(def protected (partial visibility :protected))
