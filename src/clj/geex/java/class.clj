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

(spec/def ::var (spec/cat :type any?
                          :name symbol?))

(spec/def ::arglist (spec/and vector?
                              (spec/spec (spec/* ::var))))

(spec/def ::method (spec/and seq?
                             (spec/spec (spec/cat :name symbol?
                                                  :arglist ::arglist
                                                  :body any?))))

(spec/def ::class-data (spec/* (spec/alt :extends ::extends
                                         :implements ::implements
                                         :scope ::scope
                                         :method (maybe-static ::method)
                                         :var (maybe-static ::var))))

(spec/def ::class-def (spec/cat :settings (spec/? ::settings)
                                :name symbol?
                                :data ::class-data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defclass [& args]
  `(let [evaled-args# [~@args]]
     ))
