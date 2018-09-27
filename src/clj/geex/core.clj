(ns geex.core
  (:require [clojure.spec.alpha :as spec]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [bluebell.utils.wip.party.coll :as partycoll]
            [bluebell.utils.wip.party :as party]
            [bluebell.utils.wip.core :as utils]
            [bluebell.utils.wip.check :refer [check-io checked-defn]]
            [geex.core.utils :as old-core]
            [clojure.pprint :as pp]
            [clojure.set :as cljset]
            [bluebell.utils.wip.specutils :as specutils]
            [geex.core.xplatform :as xp]))

(declare make-seed)
(declare wrap)
(declare get-seed)
(declare disp-state)
(declare make-seed!)

(def check-debug true)

;; Checking policy:
;;  - check-debug checks everything!
;;  - in addition to that, we can have extra shallow checks
;;    that are cheap and makes sure the user does not provide
;;    bad input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::platform any?)
(spec/def ::counter int?)
(spec/def ::reverse-counter int?)
(spec/def ::seed-id ::counter)
(spec/def ::seed-map (spec/map-of ::seed-id ::defs/seed))
(spec/def ::seed-ref any?)
(spec/def ::var-id int?)
(spec/def ::sym-counter int?)
(spec/def ::local-var-info (spec/keys :opt [::type]))
(spec/def ::local-vars (spec/map-of ::var-id ::local-var-info))
(spec/def ::type-signature any?)
(spec/def ::boolean-or-nil (fn [v]
                             (or (true? v)
                                 (false? v)
                                 (nil? v))))
(spec/def ::bind-if? ::boolean-or-nil)
(spec/def ::return-if? ::boolean-or-nil)
(spec/def ::if-opts (spec/keys :req [::bind-if? ::return-if?]))
(spec/def ::flat-local-var-ids (spec/coll-of ::var-id))

(spec/def ::local-struct (spec/keys :req [::type-signature
                                          ::flat-var-ids]))

(spec/def ::local-structs (spec/map-of any? ::local-struct))
(spec/def ::output any?)
(def flags #{:disp-final-state
             :disp-initial-state
             :disp-bind?
             :disp-trace
             :disp-generated-output})
(spec/def ::flag flags)
(spec/def ::flags (spec/* ::flag))
(spec/def ::with-mode (spec/keys :req [::seed/mode]))

(spec/def ::mode-stack (spec/coll-of ::seed/mode))

(spec/def ::max-mode ::seed/mode)

(spec/def ::maybe-seed-id (spec/or :seed-id ::seed-id
                                 :nil nil?))

(spec/def ::seed-cache (spec/map-of any? ::defs/seed))

(spec/def ::seed-cache-stack (spec/coll-of ::seed-cache))

(spec/def ::ids-to-visit (spec/coll-of ::seed-id))

(spec/def ::name any?)
(spec/def ::result any?)

(spec/def ::lvar-binding (spec/keys :req-un [::name
                                             ::result]))

(spec/def ::lvar-bindings (spec/coll-of ::lvar-binding))

(spec/def ::lightweight-state? #{true})

(spec/def ::lightweight-state (spec/keys :req-un [::lightweight-state?]))

(spec/def ::full-state (spec/keys :req-un [::seed-cache
                                           ::seed-cache-stack
                                           ::platform
                                           ::counter
                                           ::reverse-counter
                                           ::sym-counter
                                           ::seed-map
                                           ::lvar-bindings
                                           ::output
                                           ::mode-stack
                                           ::local-vars
                                           ::local-structs
                                           ::max-mode]))

(spec/def ::state (spec/or :light ::lightweight-state
                           :full ::full-state))

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

(def default-if-opts {::bind-if? nil ::return-if? false})

(def empty-seed (-> {}
                    (seed/referents [])
                    (seed/access-deps {})))

(defn ensure-state [x]
  (assert (state? x))
  x)

;;;------- State operations -------
(def empty-state
  {;; Set this to true to avoid checking the full structure
   ;; of the state every time we pass it through a function
   :lightweight-state? true

   :output nil

   :prefix ""

   :lvar-bindings []

   :seed-cache {}

   :seed-cache-stack []

   :mode-stack []

   :max-mode :pure

   :platform :clojure
   
   ;; Used to assign ids to seeds
   :counter 0
   
   :sym-counter 0

   :reverse-counter 1

   :local-vars {}

   :local-structs {}

   ;; All the seeds
   :seed-map {}

   :ids-to-visit []

   })

(def ^:dynamic state-atom nil)


(defn get-last-seed [state]
  {:pre [(state? state)]
   :post [(seed/seed? state)]}
  (get-in state [:seed-map (:counter state)]))

(checked-defn
 state-gensym [:when check-debug

               ::state state

               :post ::state-and-output]
 (let [counter (:sym-counter state)]
   [(update state :sym-counter inc) (xp/call :counter-to-sym counter)]))

(defn wrap-f-args [f args]
  (fn [x] (apply f (into [x] args))))

(defn swap-the-state! [f & args]
  (when (not state-atom)
    (throw (ex-info "Swapping state without a state atom. Did you forget to wrap your code in a function?"
                    {:f f})))
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
  (get-output (swap-the-state!
               (comp put-in-output (wrap-f-args f args)))))

(defn swap-without-output! [& args]
  (apply swap-the-state! args)
  nil)

(checked-defn add-binding [:when check-debug
                           ::state state

                           any? sym
                           any? expr

                           :post ::state]
              (update state :lvar-bindings conj {:name sym
                                                 :result expr}))


(checked-defn get-lvar-bindings [:when check-debug
                                 ::state state
                                 :post ::lvar-bindings]
              (:lvar-bindings state))


(checked-defn clear-lvar-bindings [:when check-debug
                                   ::state state
                                   :post ::state]
              (assoc state :lvar-bindings []))




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

(checked-defn step-reverse-counter [:when check-debug
                                    ::state state
                                    :post ::state]
              (update state :reverse-counter dec))

(checked-defn get-reverse-counter
              [:when check-debug
               ::state state
               :post ::reverse-counter]
              (:reverse-counter state))

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

(checked-defn set-compilation-result [:when check-debug

                                      ::state state
                                      _ result
                                      fn? cb]
  (cb (defs/compilation-result state result)))

(defn primitive-seed [state x]
  {:post [(not (coll? x))
          (state-and-output? %)
          (registered-seed? (second %))]}
  (make-seed
   state
   (-> {}
       (seed/description (str "primitive " x))
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

(defn populate-input-seed [seed0 id]
  (merge empty-seed
         (set-seed-id seed0 id)))

(checked-defn get-seed-id [:when check-debug
                           ::defs/seed seed
                           :post ::seed-id]
              (:seed-id seed))

(defn add-seed-to-state [state seed]
  (update state :seed-map conj [(get-seed-id seed) seed]))

(defn update-state-max-mode [state seed]
  (update state :max-mode seed/max-mode
          (seed/access-mode seed)))

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
       seed0 (populate-input-seed seed0 id)
       state (-> state
                 (add-seed-to-state seed0)
                 (update-state-max-mode seed0))]
   [state seed0]))

(defn compile-to-nothing [comp-state expr cb]
  (cb (defs/compilation-result comp-state ::nothing)))

(defn begin-seed [state]
  (first
   (make-seed
    state
    (-> {}
        (seed/description "begin")
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

(checked-defn begin-scope [:when check-debug
                           ::state state

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




(checked-defn end-seed [:when check-debug
                        ::state state
                        ::defs/seed x
                        
                        :post ::state-and-output]
              (make-seed
               state
               (-> {}
                   (seed/description "end")
                   (seed/datatype (seed/datatype x))
                   (seed/access-deps {:value x})
                   (seed/access-mode (:max-mode state))
                   (seed/access-special-function :end)
                   (seed/compiler compile-forward-value))))




(checked-defn end-scope [:when check-debug
                         ::state state
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

(defn disp-indented [state & args]
  (println (apply str (into [(:prefix state)]
                            args))))

(checked-defn return-state [:when check-debug

                            ::state x

                            ::seed-id end-id]
  (if (not returned-state)
    (throw (ex-info "No returned-state atom"))
    (let [r (swap! returned-state merge
                   ;; Dissociate any other begin-at,
                   ;; because x should already contain it.
                   (dissoc x :begin-at))]
      (when (:disp-trace x)
        (disp-indented x "Return result to " (:begin-at r))))))

(checked-defn step-generate-at [:when check-debug
                                ::state state
                                
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

(def final-state (atom nil))

(defn continue-code-generation-or-terminate [state last-generated-code]
  (if (next-id-to-visit state)
    (generate-code-from state)
    (do
      (reset! final-state state)
      (when (:disp-final-state state)
        (println "Final state")
        (disp-state state))
      last-generated-code)))

(defn should-bind-result [seed]
  (let [explicit-bind (seed/access-bind? seed)
        ref-count (-> seed seed/referents count)]
    (if (nil? explicit-bind)
      (if (seed/has-special-function? seed :begin)
        false
        (case (seed/access-mode seed)
          :pure (<= 2 ref-count)
          :ordered (<= 1 ref-count)
          :side-effectful true))
      explicit-bind)))

(checked-defn
 maybe-bind-result [:when check-debug
                    ::state state
                    ::defs/seed seed

                    :post ::state]
 (let [bind? (should-bind-result seed)]
   (when (:disp-bind? state)
     (println (str "Bind seed '"
                   (:seed-id seed) "'? "
                   (if bind? "YES" "NO"))))
   (if bind?
     (let [lvar (xp/call :lvar-for-seed seed)
           state (add-binding state lvar
                              (defs/compilation-result state))]
       (when (:disp-bind? state)
         (println "new bindings " (:lvar-bindings state)))
       
       (defs/compilation-result state lvar))
     state)))


(defn flush-bindings [state cb]
  (let [bds (get-lvar-bindings state)]
    (if (empty? bds)
      (cb state)
      (xp/call
       :render-bindings
       bds
       (cb (clear-lvar-bindings state))))))

(defn compile-flush [comp-state expr cb]
  (flush-bindings
   comp-state
   (fn [comp-state]
     (compile-forward-value comp-state expr cb))))

(defn flush-seed [state input]
  (let [[state input] (to-seed-in-state state input)]
    (make-seed
     state
     (-> empty-seed
         (seed/description "flush")
         (seed/access-special-function :bind)
         
         ;; It is pure, but has special status of :bind,
         ;; so it cannot be optimized away easily
         (seed/access-mode :pure)
         (seed/add-deps {:value input})
         (seed/datatype (seed/datatype input))
         (seed/compiler compile-flush)))))

(defn get-returned-at-begin-at []
  (if returned-state
    (:begin-at
     (deref returned-state))))

(defn disp-generated [state generated]
  (when (:disp-generated-output state)
    (println "Output at " (next-id-to-visit state))
    (clojure.pprint/pprint generated))
  generated)

(checked-defn
 generate-code-from [:when check-debug
                     
                     ::state state]
 (disp-generated
  state
  (let [state (update state :prefix #(str % "  "))
        id (next-id-to-visit state)]
    (when (:disp-trace state)
      (disp-indented state "Generate from " id))
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
                         (let [state (maybe-bind-result state seed)
                               state (propagate-compilation-result-to-seed
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

              generated-code (binding [returned-state
                                       returned-state-to-bind]
                               (c
                                state
                                (decorate-seed-for-compilation
                                 state
                                 seed
                                 id)
                                inner-cb))

              _ (when (:disp-trace state)
                  (println (str (:prefix state) "Back at " id)))
              
              _ (when (not (deref has-result?))
                  (throw (ex-info "Result callback not called for seed"
                                  {:seed seed})))]
          (if (seed/has-special-function? seed :begin)
            (let [state (deref returned-state-to-bind)
                  end-id (:end-id seed)]
              (if (not (seed-id? end-id))
                (throw (ex-info "Invalid end-id for begin-seed"
                                {:begin-id id
                                 :end-id end-id})))
              (when (:disp-trace state)
                (disp-indented state "Put result of " id " into "
                               end-id))
              (when (= state init-return-state)
                (throw (ex-info "Missing end-scope!"
                                {:begin-id id})))
              (when (not= (:begin-at state) id)
                ;; Of course, it could be the sanity check that
                ;; is bad in case it doesn't work!!!
                (throw (ex-info "SANITY CHECK: Bad begin-at"
                                {:expected id
                                 :begin-at (:begin-at state)})))
              (assert (spec/valid? ::seed-id end-id))
              (continue-code-generation-or-terminate
               (-> state
                   (defs/compilation-result generated-code)
                   (maybe-bind-result (get-seed state end-id))
                   (propagate-compilation-result-to-seed end-id))
               generated-code))
            generated-code)))
      
      (throw (ex-info "Cannot generate code from this id"
                      {:id id
                       :state state}))))))

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

(checked-defn set-begin-end-id [:when check-debug
                                ::state state
                                ::seed-id begin-id
                                ::seed-id end-id]
              (update-state-seed state
                                 begin-id
                                 #(assoc % :end-id end-id)))

(defn check-referent-visibility-for-id [state id]
  {:pre [(set? (:invisible state))
         (vector? (:begin-stack state))]}
  (let [seed (get-seed state id)]
    (if (seed/has-special-function? seed :begin)
      (update state :begin-stack conj id)
      (let [invisible (:invisible state)
            deps (-> seed seed/access-deps vals set)
            intersection (cljset/intersection deps invisible)]
        (when (not (empty? intersection))
          (disp-state state)
          (throw (ex-info "Seed refers to other seeds in closed scope"
                          {:seed-id id
                           :deps intersection})))
        (if (seed/has-special-function? seed :end)
          (let [begin-stack (:begin-stack state)
                begin-id (last begin-stack)]
            (-> state
                (set-begin-end-id begin-id id)
                (update :invisible into (range begin-id id))
                (update :begin-stack butlast-vec)))
          state)))))

(checked-defn
 check-referent-visibility
 [:when check-debug
  ::state state
  :post ::state]
 (let [state (reduce
              check-referent-visibility-for-id
              (merge state {:begin-stack []
                            :invisible #{}})
              (:ids-to-visit state))
       begin-stack (:begin-stack state)]
   (when (not (empty? begin-stack))
     (throw (ex-info "The following begin-scopes were not closed"
                     {:begin-stack begin-stack})))
   state))

(defn to-coll-expression [c]
  (if (seq? c)
    (cons 'list c)
    c))

(checked-defn make-top-seed [:when check-debug
                             ::state state
                             ::defs/seed seed

                             :post ::state-and-output]
              (assert (empty? (seed/access-deps seed)))
              (let [state (step-reverse-counter state)
                    seed (populate-input-seed
                          seed
                          (get-reverse-counter state))
                    state (add-seed-to-state state seed)]
                [state seed]))

(defn compile-local-var-seed [state expr cb]
  (let [sym (xp/call :local-var-sym (:var-id expr))]
    `(let [~sym (atom nil)]
       ~(cb (defs/compilation-result state ::declare-local-var)))))

(defn declare-local-var-seed [var-id]
  (-> empty-seed
      (assoc :var-id var-id)
      (seed/access-mode :pure)
      (seed/datatype nil)
      (seed/compiler compile-local-var-seed)))

(checked-defn
 declare-local-var [:when check-debug
                    ::state state
                    :post ::state-and-output]
 (let [id (count (:local-vars state))
       [state decl-seed]
       (-> state
           (assoc-in [:local-vars id] {::var-id id})
           (make-top-seed (declare-local-var-seed id)))]
   [state id]))

(defn compile-assign-local-var [state expr cb]
  (let [var-id (:var-id expr)
        sym (xp/call :local-var-sym var-id)
        deps (seed/access-compiled-deps expr)
        v (:value deps)]
    (set-compilation-result
      state
      `(reset! ~sym ~v)
      cb)))

(defn set-local-var [state var-id dst-value]
  (let [[state seed] (to-seed-in-state state dst-value)]
    (if (= (:get-local-var-id seed) var-id)
      state
      (let [seed-type (seed/datatype seed)
            state (update-in
                   state [:local-vars var-id]
                   (fn [var-info]
                     {:pre [(spec/valid?
                             ::local-var-info var-info)]}
                     (if (contains? var-info ::type)
                       (do 
                         (when (not= (::type var-info)
                                     seed-type)
                           (throw
                            (ex-info
                             "Incompatible type when assigning local var"
                             {:existing-type (::type var-info)
                              :new-type seed-type})))
                         var-info)
                       (assoc var-info ::type seed-type))))
            [state assignment] (make-seed
                                state
                                (-> empty-seed
                                    (seed/datatype nil)
                                    (assoc :var-id var-id)
                                    (seed/access-mode :side-effectful)
                                    (seed/access-deps {:value seed})
                                    (seed/compiler
                                     compile-assign-local-var)))]
        state))))

(defn declare-local-vars [state n]
  (loop [state state
         n n
         acc []]
    (if (= n 0)
      [state acc]
      (let [[state var-id] (declare-local-var state)]
        (recur state (dec n) (conj acc var-id))))))

(checked-defn allocate-local-struct [:when check-debug
                                     ::state state
                                     _ id
                                     _ input

                                     :post ::state]
  (let [type-sig (old-core/type-signature input)]
    (if-let [struct-info (get-in state [:local-structs id])]
      (do (when (not= (::type-signature struct-info)
                      type-sig)
            (throw (ex-info (str "Type mismatch for local struct at id " id)
                            {:current (::type-signature struct-info)
                             :new type-sig})))
          state)
      (let [flat (old-core/flatten-expr input)
            [state ids] (declare-local-vars state (count flat))]
        (assoc-in state [:local-structs id] {::type-signature type-sig
                                             ::flat-var-ids ids})))))

(defn set-local-vars [state id input]
  (let [info (get-in state [:local-structs id])
        flat-ids (::flat-var-ids info)
        flat-input (old-core/flatten-expr input)]
    (assert (= (count flat-ids)
               (count flat-input)))
    (reduce (fn [state [id input]]
              (set-local-var state id input))
            state
            (map vector flat-ids flat-input))))

(defn set-local-struct [state id input]
  (-> state
      (allocate-local-struct id input)
      (set-local-vars id input)))

(defn compile-get-var [state expr cb]
  (set-compilation-result
   state
   `(deref ~(xp/call :local-var-sym (:var-id expr)))
   cb))

(defn get-local-var [state var-id]
  (let [var-info (get-in state [:local-vars var-id])]
    (when (nil? var-info)
      (throw (ex-info (str  "No local var with id " var-id)
                      {})))
    (when (not (contains? var-info ::type))
      (throw (ex-info
              (str "Local var with id "
                   var-id
                   " must be assigned before it can be read.")
              {})))
    (let [var-type (::type var-info)]
      (make-seed
           state
           (-> empty-seed
               (seed/datatype var-type)
               (seed/access-mode :ordered)
               (seed/compiler compile-get-var)
               (assoc :var-id var-id)
               (assoc :get-local-var-id var-id))))))

(defn get-local-vars [state ids]
  (loop [ids ids
         state state
         acc []]
    (if (empty? ids)
      [state acc]
      (let [[state v] (get-local-var state (first ids))]
        (recur (rest ids)
               state
               (conj acc v))))))

(checked-defn
 get-local-struct [:when check-debug
                   ::state state
                   _ id
                   :post ::state-and-output]
 (let [info (get-in state [:local-structs id])]
   (when (nil? info)
     (throw (ex-info (str "No local struct at id " id)
                     {})))
   (let [type-sig (::type-signature info)
         flat-ids (::flat-var-ids info)
         [state vars] (get-local-vars state flat-ids)]
     [state
      (old-core/populate-seeds type-sig vars)])))

(defn set-bind [state x value]
  {:pre [(spec/valid? ::boolean-or-nil value)
         (registered-seed? x)]}
  (if (nil? value)
    state
    (update-in state [:seed-map (:seed-id x)]
               (fn [sd]
                 {:pre [(spec/valid? ::defs/seed sd)]}
                 (defs/access-bind? sd value)))))

(defn compile-if [state expr cb]
  (xp/call :compile-if state expr cb))

(defn if-sub [condition on-true on-false]
  (make-seed!
   (-> empty-seed
       (seed/access-mode
        :side-effectful
        #_(seed/max-mode (seed/access-mode on-true)
                         (seed/access-mode on-false)))
       (seed/compiler compile-if)
       (seed/datatype nil)
       (seed/access-deps {:cond condition
                          :on-true on-true
                          :on-false on-false}))))

(defn compile-loop [state expr cb]
  (xp/call :compile-loop state expr cb))

(defn loop-sub [body]
  (make-seed!
   (-> empty-seed
       (seed/access-deps {:body body})
       (seed/access-mode :side-effectful)
       (seed/datatype nil)
       (seed/compiler compile-loop))))

(defn call-recur []
  (xp/call :call-recur))

(defn call-break []
  (xp/call :call-break))

(defn compile-recur-seed [state expr cb]
  (set-compilation-result
   state
   `(recur)
   cb))

(defn recur-seed! []
  (make-seed!
   (-> empty-seed
       (seed/datatype nil)
       (seed/compiler compile-recur-seed)
       (seed/access-mode :pure)       )))


(defn compile-return-value [comp-state expr cb]
  (let [dt (seed/datatype expr)
        compiled-expr (-> expr
                          seed/access-compiled-deps
                          :value)]
    (cb (defs/compilation-result
          comp-state
          (xp/call
           :compile-return-value
           dt
           compiled-expr)))))

(defn compile-bind-name [comp-state expr cb]
  (cb (defs/compilation-result comp-state
        (xp/call
         :compile-bind-name
         (defs/access-name expr)))))


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
  (swap-without-output! begin-scope))

(defn end-scope! [value]
  (swap-with-output! end-scope value))

(defn with-state [init-state body-fn]
  {:pre [(state? init-state)
         (fn? body-fn)]
   :post [(state? %)]}
  (let [new-state (atom init-state)]
    (binding [state-atom new-state
              defs/state new-state]
      (let [body-result (body-fn)]
        (set-output (deref state-atom) body-result)))))

(defn flush! [x]
  (swap-with-output! flush-seed x))

(defn eval-body-fn [init-state body-fn]
  (-> init-state
      (with-state (comp flush! body-fn))
      build-referents
      build-ids-to-visit
      check-referent-visibility))

(defmacro eval-body [init-state & body]
  `(eval-body-fn ~init-state (fn [] ~@body)))

(checked-defn disp-state [:when check-debug
                          ::state state]
              (clojure.pprint/pprint
               (-> state
                   (update :seed-cache keys)
                   (update :seed-map
                           (fn [sm]
                             (vec (sort-by first sm)))))))

(def pp-eval-body-fn (comp disp-state eval-body-fn))

(defmacro pp-eval-body [init-state & body]
  `(pp-eval-body-fn ~init-state (fn [] ~@body)))

(defn to-seed [x]
  (swap-with-output! to-seed-in-state x))

(def wrap to-seed)

(defn generate-code [state]
  (binding [defs/state state]
    (generate-code-from state)))

(defn set-flag! [& flags]
  {:pre [(spec/valid? ::flags flags)]}
  (swap-without-output!
   (fn [state]
     (reduce (fn [state flag]
               (assoc state flag true))
             state flags))))


(defn declare-local-var! []
  (swap-with-output! declare-local-var))

(checked-defn set-local-var!
              [:when check-debug
               ::var-id var-id
               _ input]
  (swap-without-output!
   #(set-local-var % var-id input)))

(defn set-local-struct! [id data]
  (swap-without-output!
   #(set-local-struct % id data)))

(checked-defn
 get-local-var!
 [:when check-debug
  ::var-id var-id]
 (swap-with-output!
  #(get-local-var % var-id)))

(defn get-local-struct! [id]
  (swap-with-output!
   #(get-local-struct % id)))

(checked-defn set-bind! [:when check-debug
                          ::defs/seed x
                          ::boolean-or-nil v]
              (swap-without-output!
               #(set-bind % x v))
              x)

(defn dont-bind! [x]
  (set-bind! x false))

(defn gensym! []
  (swap-with-output! state-gensym))

(defn genkey! []
  (keyword (gensym!)))

(defmacro If-with-opts [opts condition on-true on-false]
  `(let [evaled-cond# (flush! (wrap ~condition))
         key# (genkey!)]
     (if-sub evaled-cond#
                           (do (begin-scope!)
                               (set-local-struct! key# ~on-true)
                               (dont-bind! (end-scope! (flush! nil))))
                           (do (begin-scope!)
                               (set-local-struct! key# ~on-false)
                               (dont-bind! (end-scope! (flush! nil)))))
     (get-local-struct! key#)))

(defmacro If [condition on-true on-false]
  `(If-with-opts
    ~default-if-opts
    ~condition ~on-true ~on-false))

(checked-defn
 loop0
 [_ init-state
  fn? prep
  fn? loop?
  fn? next]
 (let [key (genkey!)]
   (flush! (set-local-struct! key init-state))
   (loop-sub
    (do (begin-scope!)
        (let [x (get-local-struct! key)
              p (prep x)]
          (dont-bind!
           (end-scope!
            (flush!
             (If
              (loop? p)
              (do (set-local-struct! key (next p))
                  (wrap true))
              (do (wrap false)))))))))
   (get-local-struct! key)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Stuff added when porting the other modules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn with-new-seed [desc f]
  (make-seed!
   (f (seed/description empty-seed desc))))

(defn wrap-expr-compiler [c]
  {:pre [(fn? c)]}
  (fn [comp-state expr cb]
    (cb (defs/compilation-result comp-state (c expr)))))

(def flat-seeds-traverse old-core/flat-seeds-traverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Basic binding
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bind-name [datatype binding-name]
  (with-new-seed
    "bind-name"
    (fn [s]
      (-> s
          (seed/access-mode :side-effectful)
          (seed/datatype datatype)
          (defs/access-name binding-name)
          (seed/access-bind? false)
          (seed/compiler compile-bind-name)))))

(defn nil-of [cl]
  (with-new-seed
    "nil"
    (fn [s]
      (-> s
          (seed/access-bind? false)
          (defs/datatype cl)
          (seed/compiler (xp/get :compile-nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Returning a value
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn return-value [x0]
  (let [x (to-seed x0)]
    (with-new-seed
      "return-value"
      (fn [s]
        (-> s
            (seed/access-mode :side-effectful)
            (seed/access-bind? false)
            (defs/datatype (defs/datatype x))
            (defs/access-deps {:value x})
            (seed/compiler compile-return-value))))))








(xp/register
 :clojure
 {

  :compile-coll2
  (fn [comp-state expr cb]
    (let [output-coll (partycoll/normalized-coll-accessor
                       (old-core/access-original-coll expr)
                       (seed/access-compiled-indexed-deps expr))]
      (cb (defs/compilation-result
            comp-state
            (to-coll-expression output-coll)))))

  :lvar-for-seed (fn [seed]
                   {:pre [(contains? seed :seed-id)]}
                   (let [id (:seed-id seed)]
                     (symbol (format
                              "s%s%03d"
                              (if (< id 0) "m" "")
                              id))))

  :local-var-sym (fn [id]
                   (symbol (str "lvar" id)))

  :counter-to-sym (fn [counter] (symbol (str "sym" counter)))

  :compile-if (fn [state expr cb]
                (let [deps (seed/access-compiled-deps expr)]
                  (set-compilation-result
                   state
                   `(if ~(:cond deps)
                      ~(:on-true deps)
                      ~(:on-false deps))
                   cb)))

  :compile-loop (fn [state expr cb]
                  (let [deps (seed/access-compiled-deps expr)]
                    (set-compilation-result
                     state
                     `(loop []
                        (when ~(:body deps)
                          (recur)))
                     cb)))

  :call-recur (fn [] (recur-seed!))
  :call-break (fn [] nil)

  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Extra stuff
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn demo-add-compiler [comp-state expr cb]
  (cb [:add]))

(defn demo-add [a b]
  (let [a (wrap a)
        b (wrap b)]
    (make-seed!
     (-> {}
         (seed/access-mode :pure)
         (seed/datatype Double/TYPE)
         (seed/access-deps {:a a
                            :b b})
         (seed/compiler demo-add-compiler)))))

(defn demo-compile-call-fn [comp-state expr cb]
  (let [compiled-deps (seed/access-compiled-indexed-deps expr)]
    (cb (defs/compilation-result
          comp-state
          `(~(:f expr) ~@compiled-deps)))))

(checked-defn demo-call-fn [:when check-debug
                            ::seed/mode mode
                            symbol? f
                            sequential? args

                            :post ::defs/seed]
  (make-seed!
   (-> empty-seed
       (assoc :f f)
       (seed/description (str "call " f))
       (seed/access-mode mode)
       (seed/access-indexed-deps args)
       (seed/datatype nil)
       (seed/compiler demo-compile-call-fn))))

(defmacro demo-make-fn [mode f]
  `(fn [& args#]
     (demo-call-fn ~mode (quote ~f) args#)))

(defn demo-sub-step-counter [dst counter-key]
  (swap! dst #(update % counter-key (fn [x] (inc (or x 0))))))

(def demo-pure-add (demo-make-fn :pure +))

(def demo-step-counter (demo-make-fn
                        :side-effectful demo-sub-step-counter))

(defmacro demo-embed [& code]
  (let [body-fn (eval `(fn [] ~@code))
        state (eval-body-fn empty-state body-fn)
        code (generate-code state)]
    code))

(defmacro full-generate [[init-state] & code]
  `(let [init-state# (eval-body-fn
                      (merge empty-state ~init-state)
                      (fn [] ~@code))
         result# (generate-code init-state#)
         final-state# (deref final-state)]
     {:init-state ~init-state
      :result result#
      :comp-state final-state#
      :final-state final-state#
      :expr (quote ~code)}))

(defmacro generate-and-eval [& code]
  `(->> (fn [] ~@code)
        (eval-body-fn empty-state)
        generate-code
        eval))
