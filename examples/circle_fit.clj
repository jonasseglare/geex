(ns examples.circle-fit
  (:require [geex.core :as geex]
            [geex.lib :as lib]
            [geex.java :as java]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Common test setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def settings {:center [1 2]
               :radius 3.4
               :noise 0.1
               :count 100})

(defn generate-circle-point [[cx cy] radius noise-factor]
  (let [angle (* Math/PI 2.0 (rand))
        noise (* noise-factor (- (* 2.0 (rand)) 1))
        noisy-radius (+ radius noise)]
    [(+ cx (* noisy-radius (Math/cos angle)))
     (+ cy (* noisy-radius (Math/sin angle)))]))

(defn generate-samples [settings]
  (vec (take (:count settings)
             (repeatedly (partial generate-circle-point
                                  (:center settings)
                                  (:radius settings)
                                  (:noise settings))))))

(def test-samples (generate-samples settings))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ad-var [x active-index dim]
  {:value x
   :derivatives (assoc (vec (take dim  (repeat 0)))
                       active-index
                       1.0)})

(defn ad-cst [x dim]
  {:value x
   :derivatives (vec (take dim  (repeat 0)))})

(defn ad-add [a b]
  {:value (lib/+ (:value a) (:value b))
   :derivatives (map lib/+
                     (:derivatives a)
                     (:derivatives b))})

(defn mul-deriv [a b da db]
  (lib/+ (lib/* a db) (lib/* b da)))

(defn ad-mul [a b]
  {:value (lib/* (:value a) (:value b))
   :derivatives (mapv (partial mul-deriv (:value a) (:value b))
                      (:derivatives a)
                      (:derivatives b))})

(defn ad-neg [x]
  {:value (lib/- (:value x))
   :derivatives (mapv lib/- (:derivatives x))})

;; (java/eval-expr (ad-mul (ad-var 3.4 3 4) (ad-var 5.0 2 4)))

(defn ad-sqr [x]
  (ad-mul x x))

(defn ad-sqrt [x]
  (let [s (lib/sqrt (:value x))
        f (lib// 1.0 (lib/* 2.0 s))]
    {:value s
     :derivatives (mapv (partial lib/* f) (:derivatives x))}))

;; (java/eval-expr (ad-sqrt (ad-var 3.4 3 4)))

(defn eval-fitness [[ad-cx ad-cy ad-rad]
                    [ad-x ad-y]]
  (let [dist (ad-sqrt
              (ad-add
               (ad-sqr (ad-add ad-cx (ad-neg ad-x)))
               (ad-sqr (ad-add ad-cy (ad-neg ad-y)))))]
    (ad-sqr (ad-add dist (ad-neg ad-rad)))))

(defn ad-x [x]
  (ad-var x 0 3))

(defn ad-y [y]
  (ad-var y 1 3))

(defn ad-radius [r]
  (ad-var r 2 3))

(defn ad-const [x]
  (ad-cst x 3))

                                        ;(java/eval-expr (eval-fitness [(ad-x 1.0) (ad-y 2.0) (ad-radius 3.0)] [(ad-const 4.1) (ad-const 2.0)]))

(defn take-gradient-step [state point-array step-size]
  state)

(defn take-step [point-array step-size state]
  (-> state
      (update :X take-gradient-step point-array step-size)
      (update :counter lib/inc)))




;;; TODO: It should be enough with
;; just Double/TYPE instead of (lib/typed-seed Double/TYPE)
(java/typed-defn
 
 optimize-circle

 :print-source
 
 [{:init-radius Double/TYPE
   :init-center [Double/TYPE Double/TYPE]
   :step-size Double/TYPE
   :count Long/TYPE} params
  (lib/array-class Double/TYPE) points]
 (let [point-array (lib/wrap-struct-array
                    [(lib/typed-seed Double/TYPE) 
                     (lib/typed-seed Double/TYPE)] points)
       [cx cy] (:init-center params)
       radius (:init-radius params)
       X [cx cy radius]]
   (lib/iterate-while
    {:X X
     :counter (lib/wrap 0)}
    (partial take-step point-array (:step-size params))
    #(lib/< (:counter %) (:count params)))))




(time (optimize-circle {:init-radius 0.5 :init-center [1.3 2.3] :count 1000 :step-size 0.1} (double-array (flatten test-samples))))
