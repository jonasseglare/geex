(ns examples.circle-fit-test
  (:require [geex.lib :as lib]
            [geex.core :as core]
            [geex.java :as java]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(spec/def ::derivatives (spec/coll-of any?))
(spec/def ::value any?)
(spec/def ::ad (spec/keys :req-un [::value ::derivatives]))

(def ad? (partial  spec/valid? ::ad))

#_(defn variable [value n dim]
  {:value value
   :derivatives (assoc (repeat dim))})

(defn add [x y]
  {:pre [(ad? x)
         (ad? y)]
   :post [(ad? %)]}
  {:value (lib/+ (:value x) (:value y))
   :derivatives (map lib/+
                     (:derivatives x)
                     (:derivatives y))})

(defn mul [x y]
  {:pre [(ad? x)
         (ad? y)]
   :post [(ad? %)]}
  {:value (lib/* (:value x) (:value y))
   :derivatives (mapv (fn [dx dy]
                        (lib/+ (lib/* (:value x) dy)
                               (lib/* (:value y) dx)))
                      (:derivatives x)
                      (:derivatives y))})

(defn sqr [x]
  (mul x x))

(defn negate [x]
  {:value (lib/negate (:value x))
   :derivatives (mapv lib/negate (:derivatives x))})

(defn sub [x y]
  (add x (negate y)))

(defn sqrt [x]
  (let [value (lib/sqrt (:value x))]
    {:value value
     :derivatives (mapv (fn [d] (lib/* d (lib// 0.5 value)))
                        (:derivatives x))}))

(deftest various-ad-tests
  (is (= (java/eval
          (add {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 7.0 :derivatives [7 11]}))
  (is (= (java/eval
          (mul {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 12.0 :derivatives [23.0 36.0]})))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Objective function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn zeros [n]
  (vec (repeat n (lib/wrap 0.0))))

(defn variable [x i]
  {:value x
   :derivatives (assoc (zeros 3) i (lib/wrap 1.0))})

(defn constant [x]
  {:value x
   :derivatives (zeros 3)})

(defn evaluate-point-fit [[cx cy radius] [x y]]
  {:pre [(ad? cx)
         (ad? cy)
         (ad? radius)
         (ad? x)
         (ad? y)]}
  (let [dif-x (sub cx x)
        dif-y (sub cy y)
        dist (sqrt (add (sqr dif-x) (sqr dif-y)))]
    (sqr (sub dist radius))))

(defn ad-wrap-params [params]
  (mapv variable params (range (count params))))

(defn ad-wrap-point [pt]
  (mapv constant pt))

(defn array-to-pts [arr]
  (lib/wrap-struct-array [(lib/typed-seed Double/TYPE)
                          (lib/typed-seed Double/TYPE)] arr))

(defn evaluate-objf-gradient [params point-array]
  (:derivatives
   (let [ad-params (ad-wrap-params params)]
     (println "The firest is"
              (-> point-array
                  :public-type))
     (lib/transduce
      (lib/map (fn [pt]
                 (println "pt=" pt)
                 (let [wrapped (ad-wrap-point pt)]
                   (evaluate-point-fit ad-params wrapped))))
      (completing (fn [x y]
                    (println "x=" x)
                    (println "y=" y)
                    (add x y)))
      (constant (lib/wrap 0.0))
      point-array))))

(java/typed-defn
 eval-grad-fn
 [(lib/array-class Double/TYPE) arr]
 (let [wrapped-arr (array-to-pts arr)]
   (println "wrapped-arra--------pub type--")
   (println (:public-type wrapped-arr))
   (evaluate-objf-gradient
    [3.0 4.0 5.0]
    wrapped-arr)))

#_(deftest basic-tests
  (is (ad? 
       (java/eval
        (evaluate-point-fit (ad-wrap-params [1.0 2.0 1.5])
                            (ad-wrap-point [5.0 5.0]))))))
