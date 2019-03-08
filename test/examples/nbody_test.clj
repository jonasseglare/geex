(ns examples.nbody-test

  (:require [geex.common :as c]
            [geex.java :as java]
            [geex.core :as geex]
            [clojure.test :refer :all]))

(def pi 3.141592653589793)
(def solar-mass (* 4.0 pi pi))
(def days-per-year 365.24)

(def init-bodies
  {:jupiter
   {:pos [4.84143144246472090e+00
          -1.16032004402742839e+00
          -1.03622044471123109e-01]
    :vel [(* days-per-year 1.66007664274403694e-03)
          (* days-per-year 7.69901118419740425e-03)
          (* days-per-year -6.90460016972063023e-05)]
    :mass (* solar-mass 9.54791938424326609e-04)}

   :saturn
   {:pos [8.34336671824457987e+00
          4.12479856412430479e+00
          -4.03523417114321381e-01]
    :vel [(* days-per-year -2.76742510726862411e-03)
          (* days-per-year 4.99852801234917238e-03)
          (* days-per-year 2.30417297573763929e-05)]
    :mass (* solar-mass 2.85885980666130812e-04)}

   :uranus
   {:pos [1.28943695621391310e+01
          -1.51111514016986312e+01
          -2.23307578892655734e-01]
    :vel [(* days-per-year 2.96460137564761618e-03)
          (* days-per-year 2.37847173959480950e-03)
          (* days-per-year -2.96589568540237556e-05)]
    :mass (* solar-mass 4.36624404335156298e-05)}

   :neptune
   {:pos [1.53796971148509165e+01
          -2.59193146099879641e+01
          1.79258772950371181e-01]
    :vel [(* days-per-year 2.68067772490389322e-03)
          (* days-per-year 1.62824170038242295e-03)
          (* days-per-year -9.51592254519715870e-05)]
    :mass (* solar-mass 5.15138902046611451e-05)}

   :sun
   {:pos [0.0 0.0 0.0]
    :vel [0.0 0.0 0.0]
    :mass solar-mass}})

;; Checking
(comment
  (defn body-from-java [j]
    {:pos [(.x j) (.y j) (.z j)]
     :vel [(.vx j) (.vy j) (.vz j)]
     :mass (.mass j)})

  (assert (= (:jupiter bodies)
             (body-from-java (Body/jupiter))))
  (assert (= (:saturn bodies)
             (body-from-java (Body/saturn))))
  (assert (= (:uranus bodies)
             (body-from-java (Body/uranus))))
  (assert (= (:neptune bodies)
             (body-from-java (Body/neptune))))
  (assert (= (:sun bodies)
             (body-from-java (Body/sun)))))

(defn scale-vector [s v]
  (mapv (fn [x] (c/* s x)) v))

(defn div-vector [v s]
  (mapv (fn [x] (c// x s)) v))

(defn add-vectors [a b]
  (mapv c/+ a b))

(defn sub-vectors [a b]
  (mapv c/- a b))

(defn dot-product [a b]
  (apply c/+ (map c/* a b)))

(defn squared-norm [x]
  (dot-product x x))

(defn norm [x]
  (c/sqrt (squared-norm x)))

(defn offset-momentum [body p]
  (update
   body
   :vel
   (fn [v]
     (scale-vector -1.0 (div-vector p solar-mass)))))

;; (offset-momentum (:neptune init-bodies) [1 2 3000000])

(defn compute-total-momentum [bodies]
  (transduce
   (map (fn [[k body]]
          (scale-vector (:mass body)
                        (:vel body))))
   (completing add-vectors)
   [0.0 0.0 0.0]
   bodies))

(defn offset-bodies [bodies]
  (let [tot (compute-total-momentum bodies)]
    (update bodies :sun #(offset-momentum % tot))))

;; (compute-total-momentum init-bodies)
;; (compute-total-momentum (offset-bodies init-bodies))

#_(java/typed-defn tot-mom-gen []
                 ;(geex/set-flag! :disp)
                 (compute-total-momentum init-bodies))

(def sorted-vec (comp vec sort))

(defn all-unordered-pairs [data]
  (filter
   (comp (partial = 2) count)
   (map
    vec
    (set (for [a data
               b data]
           (conj #{a} b))))))

(defn all-pairs [data]
  (sort
   (map
    sorted-vec
    (all-unordered-pairs data))))

;; (all-pairs [:a :b :c])

(defn energy [bodies]

  ;; Per body
  (c/+
   (transduce
    (map (fn [body]
           (c/* 0.5 (:mass body)
                (squared-norm (:vel body)))))
    c/+
    0.0
    (vals bodies))

   ;; Per pair
   (transduce
    (map (fn [[a b]]
           (let [distance (norm
                           (sub-vectors
                            (:pos a)
                            (:pos b)))]
             (c/- (c// (c/* (:mass a) (:mass b))
                       distance)))))
    c/+
    0.0
    (all-unordered-pairs (vals bodies)))))

;; (- (.energy (NBodySystem.)) (energy (offset-bodies init-bodies)))

(defn update-body-vel [bodies body-key change]
  (update-in bodies [body-key :vel]
             (partial add-vectors change)))

(defn update-pair-velocities [dt bodies [i j]]
  (let [ibody (get bodies i)
        jbody (get bodies j)
        pos-dif (sub-vectors (:pos ibody)
                             (:pos jbody))
        squared-distance (squared-norm pos-dif)
        distance (c/sqrt squared-distance)
        mag (c// dt (c/* distance squared-distance))
        jmass (:mass jbody)]
    (-> bodies
        (update-body-vel i (scale-vector
                            (c/* -1.0 mag (:mass jbody))
                            pos-dif))
        (update-body-vel j (scale-vector
                            (c/* mag (:mass ibody))
                            pos-dif)))))

(defn update-velocities [bodies dt]
  (reduce
   (partial update-pair-velocities dt)
   bodies
   (all-pairs (keys bodies))))

(defn update-body-pos [dt body]
  (update body :pos (partial add-vectors
                             (scale-vector dt (:vel body)))))

(defn update-positions [bodies dt]
  (zipmap
   (keys bodies)
   (map (partial update-body-pos dt) (vals bodies))))

(defn advance [bodies dt]
  (-> bodies
      (update-velocities dt)
      (update-positions dt)))

(defn iterate-bodies [bodies iterations dt]
  (geex/Loop [bodies bodies
              counter 0]
             (geex/If (c/= iterations counter)
                      bodies
                      (geex/Recur
                       (advance bodies dt)
                       (c/inc counter)))))

(java/typed-defn
 run [{:iterations Integer/TYPE
       :step-size Double/TYPE} problem]
 {:energy
  (energy
   (iterate-bodies
    (offset-bodies init-bodies)
    (:iterations problem)
    (:step-size problem)))})

(def vec-type (vec (repeat 3 Double/TYPE)))
(def body-type {:pos vec-type
                :vel vec-type
                :mass Double/TYPE})

(def system-type (zipmap
                  (keys init-bodies)
                  (repeat (count init-bodies) body-type)))

(java/typed-defn advance-system [system-type system
                                 Double/TYPE dt]
                 (advance system dt))

(defn system-seq [dt]
  (iterate #(advance-system % dt) (offset-bodies init-bodies)))

;; (run {:step-size 0.01 :iterations 10000000})


(defn run-for [n]
  (run {:step-size 0.01
        :iterations (int n)}))

(deftest run-it-test
  (let [init (run-for 0)
        long-run (run-for 300)]
    (is (= -0.1690751638285245 (:energy init)))
    (is (not= (:energy init)
              (:energy long-run)))
    (is (< (Math/abs (- (:energy init)
                        (:energy long-run)))
           0.001))))
