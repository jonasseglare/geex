(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Spec
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(spec/def ::class (spec/alt :symbol symbol?
                            :class class?))

(spec/def ::classes (spec/* ::class))

(spec/def ::extends (spec/cat :prefix #{:extends}
                              :classes (spec/spec ::classes)))

(spec/def ::implements (spec/cat :prefix #{:implements}
                                 :classes (spec/spec ::classes)))

(spec/def ::visibility #{:private :public :protected})

(spec/def ::scope (spec/spec (spec/cat :visibility ::visibility
                                       :data ::class-data)))

(defn maybe-static [x]
  (spec/cat :static? (spec/? #{:static})
            :value x))

(spec/def ::var (spec/cat :type any?
                          :name symbol?))

(spec/def ::arglist (spec/spec (spec/* ::var)))

(spec/def ::method (spec/spec (spec/cat :name symbol?
                                        :arglist ::arglist
                                        :body any?)))

(spec/def ::class-data (spec/* (spec/alt :extends ::extends
                                         :implements ::implements
                                         :scope ::scope
                                         :method (maybe-static ::method)
                                         :var (maybe-static ::var))))

(spec/def ::class-def (spec/cat :name symbol?
                                :data ::class-data))

