(ns geex.core2
  (:require [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [bluebell.utils.wip.party.coll :as partycoll]
            [bluebell.utils.wip.party :as party]
            [bluebell.utils.wip.core :as utils]
            [bluebell.utils.wip.check :refer [check-io checked-defn]]
            [geex.core :as old-core]
            [clojure.pprint :as pp]
            [bluebell.utils.wip.specutils :as specutils]
            [geex.core.xplatform :as xp]))

(declare make-seed)
(declare wrap)
(declare get-seed)

(def check-debug true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::platform any?)
(spec/def ::counter int?)
(spec/def ::seed-id ::counter)
(spec/def ::seed-map (spec/map-of ::seed-id ::defs/seed))
(spec/def ::seed-ref any?)
(spec/def ::injection-deps (spec/map-of ::seed-ref ::seed-id))
(spec/def ::output any?)
(spec/def ::with-mode (spec/keys :req [::seed/mode]))

(spec/def ::mode-stack (spec/coll-of ::seed/mode))

(spec/def ::max-mode ::seed/mode)

(spec/def ::maybe-seed-id (spec/or :seed-id ::seed-id
                                 :nil nil?))

(spec/def ::seed-cache (spec/map-of any? ::defs/seed))

(spec/def ::seed-cache-stack (spec/coll-of ::seed-cache))

(spec/def ::ids-to-visit (spec/coll-of ::seed-id))

(spec/def ::state (spec/keys :req-un [::seed-cache
                                      ::seed-cache-stack
                                      ::platform
                                      ::counter
                                      ::seed-map
                                      ::injection-deps
                                      ::output
                                      ::mode-stack
                                      ::max-mode]))
(spec/def ::seed-with-id (spec/and ::defs/seed
                                   (spec/keys :req-un [::seed-id])))

(spec/def ::state-and-output (spec/cat :state ::state
                                       :output any?))

(spec/def ::compiled-deps (spec/map-of any? ::seed-id))


(def state? (partial spec/valid? ::state))
(def state-and-output? (partial spec/valid? ::state-and-output))
(def seed-map? (specutils/pred ::seed-map))
(def seed-id? (specutils/pred ::seed-id))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-seed (-> {}
                    (seed/access-deps {})))

(defn ensure-state [x]
  (assert (state? x))
  x)

;;;------- State operations -------
(def empty-state
  {:output nil

   :seed-cache {}

   :seed-cache-stack []

   :mode-stack []

   :max-mode :pure

   :platform :clojure
   ;; Used to assign ids to seeds
   :counter 0

   ;; All the seeds
   :seed-map {}

   ;; A list of created seeds so far
   :created-seeds []

   :ids-to-visit []

   ;; Dependencies that new seeds should have
   :injection-deps {}})

(defn get-last-seed [state]
  {:pre [(state? state)]
   :post [(seed/seed? state)]}
  (get-in state [:seed-map (:counter state)]))

(def ^:dynamic state-atom nil)

(defn wrap-f-args [f args]
  (fn [x] (apply f (into [x] args))))

(defn swap-state! [f & args]
  (let [fargs (wrap-f-args f args)]
    (swap! state-atom (comp ensure-state fargs ensure-state))))

(defn put-in-output [[state output]]
  (assoc state :output output))

(defn set-output [state output]
  (assoc state :output output))

(defn get-output [state]
  {:pre [(state? state)]}
  (:output state))

(defn swap-with-output! [f & args]
  (get-output (swap-state! (comp put-in-output (wrap-f-args f args)))))

(defn get-state []
  {:post [(state? %)]}
  (if (nil? state-atom)
    (throw (ex-info "No state bound, are you calling 
it outside of with-state?" {}))
    (deref state-atom)))

(defn step-counter [state]
  {:pre [(state? state)]
   :post [(state? %)]}
  (update state :counter inc))

(defn get-counter [state]
  {:pre [(state? state)]}
  (:counter state))

(defn registered-seed? [x]
  (and (spec/valid? ::seed-with-id x)
       (spec/valid? ::compiled-deps (::defs/deps x))))

(defn set-seed-id [x id]
  {:pre [(seed/seed? x)
         (spec/valid? ::seed-id id)]}
  (assoc x :seed-id id))

(defn coll-seed [state x]
  (make-seed
   state
   (-> empty-seed
       (seed/access-mode :pure)
       (seed/access-indexed-deps (partycoll/normalized-coll-accessor x))
       (old-core/access-original-coll x)
       (seed/datatype (xp/call :get-type-signature x))
       (defs/access-omit-for-summary #{:original-coll})
       (seed/compiler (xp/get :compile-coll2)))))

(defn primitive-seed [state x]
  {:post [(not (coll? x))
          (state-and-output? %)
          (registered-seed? (second %))]}
  (make-seed
   state
   (-> {}
       (seed/access-mode :pure)
       (seed/access-bind? false)
       (seed/static-value x)
       (defs/datatype (old-core/value-literal-type x))
       (seed/compiler (xp/get :compile-static-value)))))

(defn look-up-cached-seed [state x]
  (let [cache (:seed-cache state)]
    (when-let [seed (get cache x)]
      (assert (registered-seed? seed))
      [state seed])))

(checked-defn
 register-cached-seed [:when check-debug

                       ::state-and-output [state c]
                       _ x

                       :post ::state-and-output]
 (assert (registered-seed? c))
 (if (registered-seed? x)
   (do (assert (= x c))
       [state x])
   [(update state :seed-cache assoc x c) c]))

;; Should take anything
(defn to-seed-in-state [state x]
  {:pre [(state? state)]
   :post [(state-and-output? %)
          (registered-seed? (second %))]}
  (or (look-up-cached-seed state x)
      (register-cached-seed
       (cond
         (registered-seed? x) [state x]
         (seed/seed? x) (make-seed state x)
         (coll? x) (coll-seed state x)
         :default (primitive-seed state x))
       x)))

(defn import-deps
  "Replace the deps of the seed by keys, and "
  [state seed-prototype]
  {:pre [(state? state)
         (seed/seed? seed-prototype)]}
  (let [deps (vec (seed/access-deps-or-empty seed-prototype))
        [state deps] (reduce
                      (fn [[state mapped-deps] [k v]]
                        (let [[state v]                              
                              (to-seed-in-state state v)]
                          [state (conj mapped-deps [k (:seed-id v)])]))
                      [state {}]
                      deps)]
    [state (seed/access-deps seed-prototype (or deps {}))]))

(checked-defn
 make-seed [:when check-debug

            ::state state
            (spec/and seed/seed?
                      ::with-mode) seed0

            :post k [(spec/valid? ::state-and-output k)
                     (registered-seed? (second k))]]
 (assert  (not (registered-seed? seed0)))
 (let [[state seed0] (import-deps state seed0)
       state (step-counter state)
       id (get-counter state)
       seed0 (merge {::defs/deps {}} (set-seed-id seed0 id))
       state (-> state
                 (update :seed-map conj [id seed0])
                 (update :max-mode seed/max-mode
                         (seed/access-mode seed0)))]
   [state seed0]))

(defn compile-to-nothing [comp-state expr cb]
  (cb (defs/compilation-result comp-state ::nothing)))

(defn begin-seed [state]
  (first
   (make-seed
    state
    (-> {}
        (seed/datatype nil)
        (seed/access-mode :undefined)
        (seed/access-special-function :begin)
        (seed/compiler compile-to-nothing)))))

(defn butlast-vec [x]
  (subvec x 0 (dec (count x))))

(defn pop-mode-stack [state]
  (update state :mode-stack butlast-vec))

(defn pop-seed-cache-stack [state]
  (-> state
      (assoc :seed-cache (last (:seed-cache-stack state)))
      (update :seed-cache-stack butlast-vec)))

(checked-defn begin-scope [::state state

                           :post ::state]
              (-> state
                  begin-seed
                  (update :mode-stack conj (:max-mode state))
                  (update :seed-cache-stack conj (:seed-cache state))
                  (assoc :seed-cache {})
                  (assoc :max-mode :pure)))

(defn compile-forward-value [comp-state expr cb]
  (let [value-id (-> expr seed/access-deps :value)]
    (cb (defs/compilation-result
          comp-state
          (defs/compilation-result
            (get-seed comp-state value-id))))))




(checked-defn end-seed [::state state
                        ::defs/seed x
                        
                        :post ::state-and-output]
              (make-seed
               state
               (-> {}
                   (seed/datatype (seed/datatype x))
                   (seed/access-deps {:value x})
                   (seed/access-mode (:max-mode state))
                   (seed/access-special-function :end)
                   (seed/compiler compile-forward-value))))




(checked-defn end-scope [::state state
                         _ x

                         :post ::state-and-output]
              (let [[state input-seed] (to-seed-in-state
                                        state
                                        x)
                    [state output] (end-seed state input-seed)]
                [(-> state
                     pop-mode-stack
                     pop-seed-cache-stack) output]))

(defn has-seed? [state id]
  {:pre [(state? state)
         (seed-id? id)]}
  (contains? (:seed-map state) id))

(defn get-seed [state id]
  {:pre [(state? state)
         (seed-id? id)]
   :post [(seed/seed? %)]}
  (get-in state [:seed-map id]))

(defn update-state-seed [state id f]
  (check-io
   [:pre [::state state
          ::seed-id id]
    :post x [::state x]]
   (update-in state [:seed-map id] f)))

(defn add-referents-to-dep-seeds [state id]
  {:pre [(state? state)
         (seed-id? id)
         (has-seed? state id)]}
  (reduce
   (fn [state [k other-id]]
     (update-state-seed state other-id
                        (fn [s]
                          (check-io
                           [:pre [::defs/seed s]
                            :post y [::defs/seed y]]
                           (seed/add-referent s k id)))))
   state
   (seed/access-deps (get-seed state id))))

(defn seed-ids [state]
  {:pre [(state? state)]}
  (-> state
      :seed-map
      keys))

(defn build-referents [state]
  {:pre [(state? state)]
   :post [(state? state)]}
  (reduce
   add-referents-to-dep-seeds
   state
   (seed-ids state)))

(checked-defn start-id [:when check-debug

                        ::state state]
              (->> state
                   :seed-map
                   keys
                   (apply min)))

(checked-defn get-next-id [:when check-debug
                           ::state state
                           ::seed-id id]
              (loop [i (inc id)]
                (cond
                  (< (:counter state) i) nil
                  (has-seed? state i) i
                  :default (recur (inc i)))))

(checked-defn set-seed-result [:when check-debug
                               ::state state
                               ::seed-id id
                               _ result

                               :post ::state]
              (update-state-seed
               state id
               #(defs/compilation-result % result)))


(checked-defn
 propagate-compilation-result-to-seed
 [:when check-debug

  ::state state
  ::seed-id id

  :post ::state]
 (let [result (defs/compilation-result state)]
   (set-seed-result state id result)))


(def ^:dynamic returned-state nil)

(checked-defn return-state [:when check-debug

                            ::state x

                            ::seed-id end-id
                            
                            :post ::state]
  (if (not returned-state)
    (throw (ex-info "No returned-state atom"))
    (swap! returned-state merge {:end-at end-id} x)))

(checked-defn step-generate-at [::state state
                                
                                :post ::state]
  (update state :ids-to-visit rest))


(declare generate-code-from)

(defn decorate-seed-for-compilation [state seed id]
  (let [deps (seed/access-deps seed)
        compiled-deps (zipmap
                       (keys deps)
                       (map (fn [k] (defs/compilation-result
                                      (get-seed state k)))
                            (vals deps)))]
    (seed/access-compiled-deps seed compiled-deps)))

(defn next-id-to-visit [state]
  (-> state
      :ids-to-visit
      first))

(defn continue-code-generation-or-terminate [state last-generated-code]
  (if (next-id-to-visit state)
    (generate-code-from state)
    last-generated-code))



(checked-defn
 generate-code-from [:when check-debug
                     
                     ::state state]
 (let [id (next-id-to-visit state)]
   (if-let [seed (get-seed state id)]
     (if (defs/has-compilation-result? seed)
       (continue-code-generation-or-terminate
        (step-generate-at state) (defs/compilation-result seed))
       (let [state (defs/clear-compilation-result state)
           c (defs/compiler seed)
           _ (assert (fn? c)
                     (str "No compiler for seed" seed))

           has-result? (atom false)
           
           inner-cb (fn [state]
                      (reset! has-result? true)
                      (let [state
                            (propagate-compilation-result-to-seed
                             state id)
                            state (step-generate-at state)
                            result (defs/compilation-result state)]
                        (if (seed/has-special-function? seed :end)
                          (do
                            (return-state state id)
                            result)
                          (continue-code-generation-or-terminate
                           state
                           result))))
           
           init-return-state {:begin-at id}
           returned-state-to-bind (if (seed/has-special-function?
                                       seed :begin)
                                    (atom init-return-state)
                                    returned-state)
           generated-code (binding [returned-state returned-state-to-bind]
                            (c
                             state
                             (decorate-seed-for-compilation
                              state
                              seed
                              id)
                             inner-cb))
           _ (when (not (deref has-result?))
               (throw (ex-info "Result callback not called for seed"
                               {:seed seed})))]
       (if (seed/has-special-function? seed :begin)
         (let [state (deref returned-state-to-bind)
               end-id (:end-at state)]
           (when (= state init-return-state)
             (throw (ex-info "Missing end-scope!"
                             {:begin-id id})))
           (assert (= (:begin-at state) id))
           (assert (spec/valid? ::seed-id end-id))
           (continue-code-generation-or-terminate
            (-> state
                (defs/compilation-result generated-code)
                (propagate-compilation-result-to-seed
                 end-id))
            generated-code))
         generated-code)))
     
     (throw (ex-info "Cannot generate code from this id"
                     {:id id
                      :state state})))))

(defn conj-if-different [dst x]
  (if (= (last dst) x)
    dst
    (conj dst x)))

(defn build-ids-to-visit [state]
  (assoc state :ids-to-visit
         (conj-if-different
          (-> state
              :seed-map
              keys
              sort
              vec)
          (:seed-id (:output state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn seed-map [state]
  {:pre [(state? state)]
   :post [(seed-map? %)]}
  (:seed-map state))

(defn make-seed! [seed-prototype]
  (swap-with-output! make-seed seed-prototype))

(defn begin-scope! []
  (swap-state! begin-scope))

(defn end-scope! [value]
  (swap-with-output! end-scope value))

;; Call this before evaluating a subscope, so that we can restore things
(defn get-injection-deps []
  (-> (get-state)
      :injection-deps))

;; Call this after evaluating a subscope
(defn set-injection-deps! [new-deps]
  {:pre [(map? new-deps)]}
  (swap-state! assoc :injection-deps new-deps))

(defn with-state [init-state body-fn]
  {:pre [(state? init-state)
         (fn? body-fn)]
   :post [(state? %)]}
  (let [new-state (atom init-state)]
    (binding [state-atom new-state
              defs/state new-state]
      (let [body-result (body-fn)]
        (set-output (deref state-atom) body-result)))))

(defn eval-body [init-state body-fn]
  (build-ids-to-visit
   (build-referents
    (with-state init-state
      (comp wrap body-fn)))))

(checked-defn disp-state [::state state]
              (clojure.pprint/pprint
               (-> state
                   (update :seed-cache keys)
                   (update :seed-map
                           (fn [sm]
                             (vec (sort-by first sm)))))))

(def pp-eval-body (comp disp-state eval-body))

(defn to-seed [x]
  (swap-with-output! to-seed-in-state x))

(def wrap to-seed)

;; Create a new seed state flushes the bindings
#_(defn flush-bindings! []
#_  (assert false))

;; Should take all seeds created up to now,
;; create a new seed that depend on them,
;; clear the :created-seeds vector
;; make the new the seed the constant dependency. (::order)
;; This seed also flushes the bindings
(defn wrap-up! [& extra-deps]
  (assert false))

(checked-defn
 generate-code [::state state]
 (generate-code-from state))
















(xp/register
 :clojure
 {

  :compile-coll2
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state
          (partycoll/normalized-coll-accessor
           (old-core/access-original-coll expr)
           (seed/access-compiled-indexed-deps expr)))))

  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Extra stuff
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn demo-add-compiler [comp-state expr cb]
  (cb [:add]))

(defn demo-add [a b]
  (make-seed!
   (-> {}
       (seed/access-mode :pure)
       (seed/datatype java.lang.Object)
       (seed/access-deps {:a a
                          :b b})
       (seed/compiler demo-add-compiler))))
