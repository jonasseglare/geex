(ns examples.cljd-circle-test
  (:require [geex.ebmd.type :as etype]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(require '[geex.common :as c]
         '[geex.java :as java]
         '[geex.core :as g])


(def default-settings {:ranges {:cx [-1 1]
                                :cy [-1 1]
                                :r [0.5 2]}
                       :noise 0.1
                       :count 30

                       :step-size 0.25
                       :iterations 30

                       :output-dir "./circledata"

                       :opt-count 10})

(defn sample-range [[a b]]
  (+ a (* (- b a) (Math/random))))

(defn sample-circle-parameters [settings]
  (let [ranges (:ranges settings)]
    (zipmap (keys ranges)
            (map sample-range (vals ranges)))))


;;;------- Point generation -------

(defn generate-point [params]
  (let [noise #(c/rand (c/- (:noise params))
                       (:noise params))
        angle (c/rand (* 2.0 Math/PI))]
    [(c/+ (:cx params)
          (c/* (:r params) (c/cos angle))
          (noise))
     (c/+ (:cy params)
          (c/* (:r params) (c/sin angle))
          (noise))]))

(java/typed-defn
 generate-circle-points [{:r Double/TYPE
                          :cx Double/TYPE
                          :cy Double/TYPE
                          :count Long/TYPE
                          :noise Double/TYPE} params]
 (let [result (c/make-array Double/TYPE (c/* 2 (:count params)))]
   (g/Loop
    [i 0]
    (g/If (c/< i (:count params))
          (let [[x y] (generate-point params)
                at (c/* 2 i)]
            (c/aset result (c/+ at 0) x)
            (c/aset result (c/+ at 1) y)
            (g/Recur (c/inc i)))
          119))
   result))

;; (vec (generate-circle-points (merge (sample-circle-parameters settings)
{:count 3 :noise 0.1}


;;;------- Evaluating it -------


(require '[geex.common :as c])

(defn sqr [x] (c/* x x))

(defn evaluate-point [{:keys [cx cy r]} ;; $\leftarrow$ Circle parameters
                      [x y]] ;; $\leftarrow$ The point
  (let [dist-to-centre (c/sqrt (c/+ (sqr (c/- x cx))
                                    (sqr (c/- y cy))))
        dist-to-circle (c/- dist-to-centre r)]
    (sqr dist-to-circle)))



(evaluate-point
 {:cx 0.0 :cy 0.0 :r 1.0}
 [1.0 0.0])



(evaluate-point
 {:cx 0.0 :cy 0.0 :r 1.0}
 [3.0 0.0])


(java/typed-defn test-eval-pt [{:cx Double/TYPE
                                :cy Double/TYPE
                                :r Double/TYPE} params
                               [Double/TYPE Double/TYPE] pt]
                 (evaluate-point params pt))
;; (test-eval-pt {:cx 0.0 :cy 0.0 :r 1.0} [3.0 0.0])

(defn get-point-2d [src-array index] ;; $\leftarrow Helper function$
  (let [offset (c/* 2 index)]
    [(c/aget src-array (c/+ offset 0))
     (c/aget src-array (c/+ offset 1))]))


(defn circle-fitness-cost [circle-params point-array init-cost]
  (let [n (c/quot (c/cast Long/TYPE (c/count point-array)) 2)]
    (c/* (c// 1.0 n)
         (c/transduce
          (c/map (comp (partial evaluate-point circle-params)
                       (partial get-point-2d point-array)))
          c/+
          init-cost ;; $\leftarrow$ Typically 0
          (c/range n)))))


(java/typed-defn eval-circle-fitness-cost
                 [{:cx Double/TYPE
                   :cy Double/TYPE
                   :r Double/TYPE} circle-params
                  (c/array-type Double/TYPE) points]
                 ;(g/set-flag! :disp)
                 (circle-fitness-cost
                  circle-params ;; $\leftarrow c_x, c_y, r$
                  points        ;; $\leftarrow$ double-array
                  0.0           ;; $\leftarrow$ initial cost
                  ))

(defn test-eval-objf []
  (let [true-params {:cx 0.0 :cy 0.0 :r 1.0 :count 10 :noise 0.0}
        bad-params {:cx 0.0 :cy 0.0 :r 2.0}
        pts (generate-circle-points true-params)]
    {:true-fit (eval-circle-fitness-cost true-params pts)
     :bad-fit (eval-circle-fitness-cost bad-params pts)}))

;; (test-eval-objf)



;;;------- Automatic differentiation -------
(defn variable [x]
  {:value x
   :deriv 1.0}) ;; $\frac{dx}{dx} = 1$

(defn constant [c]
  {:value c
   :deriv 0.0}) ;; $\frac{dc}{dx} = 0$, $c$ being a constant

(require '[bluebell.utils.ebmd :as ebmd])

(defn ad-number? [x]
  (and (map? x) (contains? x :value) (contains? x :deriv)))

(ebmd/def-arg-spec
  ::ad ;; $\leftarrow$ Name of the spec
  {:pred ad-number? ;; $\leftarrow$ Predicate function

   ;; Examples disambiguate overlapping predicates:
   :pos [(variable 3.0) (constant 5.0)] ;; $\leftarrow$ Matching examples
   :neg [2.0 :kwd {:kattskit 119}]}) ;; $\leftarrow$ Non-matching examples

(ebmd/def-poly c/binary-add [::ad a
                             ::ad b]
  {:value (c/+ (:value a)
               (:value b))
   :deriv (c/+ (:deriv a)
               (:deriv b))})

(let [x (variable 3)]
  (c/+ x x))

;; (c/+ (variable 3) 4)

(require '[geex.ebmd.type :as etype])

(ebmd/register-promotion
 ::ad ;; Destination type
 constant ;; Promoter
 ::etype/real) ;; Source type

(c/+ (variable 3) 4)

(ebmd/def-poly c/binary-mul [::ad a
                             ::ad b]
  {:value (c/* (:value a) (:value b))

   ;; Recall that $(a \cdot b)^{\prime} = a^{\prime} \cdot b + a \cdot b^{\prime}$
   :deriv (c/+ (c/* (:value a)
                    (:deriv b))
               (c/* (:deriv a)
                    (:value b)))})

(let [x (variable 9.0)]
  (c/* x x x))

(ebmd/def-poly c/binary-sub [::ad a
                             ::ad b]
  {:value (c/- (:value a) (:value b))
   :deriv (c/- (:deriv a)
               (:deriv b))})

(ebmd/def-poly c/sqrt [::ad x]
  (let [s (c/sqrt (:value x))]
    {:value s
     :deriv (c/* (c// 0.5 s)
                 (:deriv x))}))

(c/sqrt (variable 2.0))

(defn derivative-for-key [circle-params points k]
  (:deriv (circle-fitness-cost ;; $\leftarrow$ Derivative of objective function
           (update circle-params k variable) ;; $\leftarrow$ 'k' is a variable
           points
           (constant 0.0))))

(java/typed-defn gradient
   [{:cx Double/TYPE :cy Double/TYPE :r Double/TYPE} circle-params
    (c/array-type Double/TYPE) points]
   ;(g/set-flag! :disp)
   {:cx (derivative-for-key circle-params points :cx) ;; $\leftarrow \frac{df}{dc_x}$
    :cy (derivative-for-key circle-params points :cy) ;; $\leftarrow \frac{df}{dc_y}$
    :r (derivative-for-key circle-params points :r)}) ;; $\leftarrow \frac{df}{dr}$

(defn test-eval-objf-dr []
  (let [true-params {:cx 0.0 :cy 0.0 :r 1.0 :count 10 :noise 0.0}
        bad-params {:cx 0.0 :cy 0.0 :r 2.0}
        pts (generate-circle-points true-params)]
    {:true-fit (gradient true-params pts)
     :bad-fit (gradient bad-params pts)}))

;; (test-eval-objf-dr)

(defn gradient-step [params point-array step-size]
  (let [grad (gradient params point-array)
        ks (keys grad)]
    (zipmap ks
            (map (fn [k]
                   (let [derivative (get grad k)
                         value (get params k)]
                     (- value (* derivative step-size))))
                 ks))))

(defn optimize [init-params point-array
                iterations step-size]
  (first
   (drop
    iterations
    (iterate #(gradient-step % point-array step-size)
             init-params))))


(defn test-optimize []
  (let [true-params {:cx 2.0
                     :cy 3.0
                     :r 1.5
                     :noise 0.1
                     :count 100}
        point-array (generate-circle-points true-params)
        init-params {:cx 1.0
                     :cy 1.0
                     :r 1.0}]
    (optimize init-params
              point-array
              10
              0.5)))

;; (test-optimize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Produce sample data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn decorate-parameters-with-objf [params points]
  (merge params
         {:cost (eval-circle-fitness-cost params points)
          :gradient (gradient params points)}))

(defn opt-seq [init-params point-array
               iterations step-size]
  (vec
   (take
    iterations
    (map
     #(decorate-parameters-with-objf % point-array)
     (iterate #(gradient-step % point-array step-size)
              init-params)))))


(defn make-test-seq []
  (let [settings default-settings
        params (sample-circle-parameters settings)
        points (generate-circle-points (merge settings params))
        iterations 2
        step-size (:step-size settings)]
    (opt-seq (sample-circle-parameters settings) points iterations step-size)))

;; (pp/pprint (make-test-seq))

;; (def opt-samples (produce-sample-circles-and-points))




;;;------- Clojure implementation -------
(defn clj-constant [x]
  {:value x
   :deriv 0.0})

(defn clj-variable [x]
  {:value x
   :deriv 1.0})

(defn clj-add [a b]
  {:value (+ (:value a) (:value b))
   :deriv (+ (:deriv a) (:deriv b))})

(defn clj-sub [a b]
  {:value (- (:value a) (:value b))
   :deriv (- (:deriv a) (:deriv b))})

(defn clj-mul [a b]
  {:value (* (:value a) (:value b))
   :deriv (+ (* (:value a) (:deriv b))
             (* (:deriv a) (:value b)))})

(defn clj-sqrt [x]
  (let [s (Math/sqrt (:value x))]
    {:value s
     :deriv (* (/ 0.5 s) (:deriv x))}))

(defn clj-sqr [x]
  (clj-mul x x))


(defn clj-get-point-2d [array i]
  (let [at (* 2 i)]
    [(clj-constant (aget array (+ at 0)))
     (clj-constant (aget array (+ at 1)))]))

(defn clj-evaluate-point [{:keys [cx cy r]}
                          [x y]] ;; $\leftarrow$ The point
  (let [dist-to-centre (clj-sqrt
                        (clj-add (clj-sqr
                                  (clj-sub x cx))
                                 (clj-sqr
                                  (clj-sub y cy))))
        dist-to-circle (clj-sub dist-to-centre r)]
    (clj-sqr dist-to-circle)))

(defn clj-evaluate [params array]
  (let [N (quot (alength array) 2)]
    (clj-mul (clj-constant (/ 1.0 N))
             (transduce
              (map (comp (partial clj-evaluate-point params)
                         (partial clj-get-point-2d array)))
              (completing clj-add)
              (clj-constant 0.0)
              (range N)))))

(defn clj-derivative [params array k]
  (let [ad-params (zipmap
                   (keys params)
                   (map clj-constant (vals params)))
        ad-params (assoc ad-params k (clj-variable
                                      (get params k)))]
    (:deriv (clj-evaluate ad-params array))))

(defn clj-gradient [params array]
  {:cx (clj-derivative params array :cx)
   :cy (clj-derivative params array :cy)
   :r (clj-derivative params array :r)})

(defn clj-step [params array step-size]
  (let [grad (clj-gradient params array)]
    (into {}
          (map (fn [k]
                 [k (- (get params k)
                       (* step-size (get grad k)))])
               (keys params)))))

(defn clj-optimize [params array iterations step-size]
  (first
   (drop
    iterations
    (iterate
     #(clj-step % array step-size)
     params))))




;;;------- The benchmark -------
(defn array-from-vecs [v]
  (double-array (reduce into [] v)))

(def problem-points :points)
(def problem-step-size (comp :step-size :settings))
(def problem-iterations (comp :iterations :settings))
(def problem-params :init-params)

(defn benchmark-clj [problem]
  (clj-optimize
   (problem-params problem)
   (problem-points problem)
   (problem-iterations problem)
   (problem-step-size problem)))

(defn clj-import [problem]
  (update problem :points array-from-vecs))

(defn benchmark-geex [problem]
  (optimize
   (:init-params problem)
   (problem-points problem)
   (problem-iterations problem)
   (problem-step-size problem)))


(def problem
  {:settings
 {:ranges {:cx [-1 1], :cy [-1 1], :r [0.5 2]},
  :noise 0.1,
  :count 30,
  :step-size 0.25,
  :iterations 30,
  :output-dir "./circledata",
  :opt-count 10},
 :true-params
 {:cx -0.8909834954959839,
  :cy 0.1275074136409482,
  :r 1.6924200889247751},
 :init-params
 {:cx 0.8470531641015153,
  :cy -0.23643186601703037,
  :r 1.8813854709986932},
 :points
 [[0.4512665523085643 0.9220653919083143]
  [0.8536009162944576 -0.03476543157258946]
  [-0.5928022244880031 -1.471289836284194]
  [0.587703987644064 -0.73285069584352]
  [0.1583769089811083 1.5612758637951776]
  [-1.0194404261111467 1.8923473452195179]
  [0.7075951522088312 0.3762342529917818]
  [-2.509424062643524 0.008613111284491604]
  [0.7166394182451459 0.38741727394627445]
  [-2.3949265030988807 -0.6316537750594432]
  [0.45510933331358194 1.088530940143869]
  [0.47632757288054317 1.1575276280947369]
  [-2.472227972528737 0.7853534063050429]
  [0.5667705729271931 -0.5712243455137761]
  [-1.3038883742133676 1.8262764894674783]
  [-0.6826030501712917 -1.6164106929168345]
  [-0.7281601217756662 -1.6051308288309736]
  [-2.155786906518754 1.181526142786242]
  [-0.7228728682217355 -1.6266150805617559]
  [-1.528531726149816 1.651613816802768]
  [-1.738861226721658 1.5618840887466308]
  [-2.0921670590939403 1.3247235658618484]
  [-2.4687555888158332 0.8994298425047641]
  [0.20198088652994634 -1.1212114459603024]
  [-0.23413126496040065 -1.5129950413100164]
  [-1.079511667890468 -1.4908971769989376]
  [-0.18406771984189535 -1.5099445744817972]
  [0.7537833491497294 -0.16542090291823164]
  [-2.486839453356033 0.8762259746605392]
  [-0.551533697665864 -1.5848938191540451]]})

(deftest optimization-test
  (let [input (clj-import problem)
        results (benchmark-geex input)
        clj-results (benchmark-clj input)
        true-params (:true-params problem)]
    (is (= clj-results results))
    (doseq [p [:cx :cy :r]]
      (is (< (Math/abs (- (p results) (p true-params)))
             0.02)))))
