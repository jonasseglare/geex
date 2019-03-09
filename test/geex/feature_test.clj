(ns geex.feature-test
  (:import [java.util ArrayList]
           [java.awt Point])
  (:require [geex.core :as core]
            [geex.java :as java]
            [geex.common :as c]
            [clojure.test :refer :all]))


;; New features are tested here

(java/typed-defn array-call-get [(c/array-class Double/TYPE) x]
                 (x 1))

(java/typed-defn array-call-set [(c/array-class Double/TYPE) x]
                 (x 1 119.0))

(deftest array-call-test
  (is (= 34.0 (array-call-get (double-array [9 34 2]))))
  (let [dst (double-array [0 0 0])]
    (array-call-set dst)
    (is (= 119.0 (aget dst 1)))))

(java/typed-defn
 build-array-list []
 (let [dst (java/new ArrayList)]
   (dst 'add (java/new Integer (int 3)))
   (dst 'add (java/new Integer (int 4)))
   (dst 'add (java/new Integer (int 5)))
   dst))

(deftest method-call-test
  (is (= (build-array-list)
         [3 4 5])))

(java/typed-defn point-to-clojure [Point pt]
                 [(pt "x")
                  (pt "y")])

(java/typed-defn make-point []
                 (let [dst (java/new Point)]
                   (dst "x" 9)
                   (dst "y" 20)
                   dst))

(deftest field-access
  (is (= (point-to-clojure (Point. 3 4))
         [3 4]))
  (is (= (point-to-clojure
          (make-point))
         [9 20])))

(java/typed-defn get-key-in-map [clojure.lang.IPersistentMap m]
                 (m :kattskit))

(java/typed-defn set-key-in-map [clojure.lang.IPersistentMap m]
                 (m :kattskit 119))

(deftest kwd-access
  (is (= 119 (get-key-in-map {:kattskit 119})))
  (is (= {:kattskit 119} (set-key-in-map {}))))
