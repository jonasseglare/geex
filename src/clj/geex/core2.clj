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
            [clojure.set :as cljset]
            [bluebell.utils.wip.specutils :as specutils]
            [geex.core.xplatform :as xp]))

(declare make-seed)
(declare wrap)
(declare get-seed)
(declare disp-state)

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
(spec/def ::output any?)
(def flags #{:disp-final-state
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

(spec/def ::state (spec/keys :req-un [::seed-cache
                                      ::seed-cache-stack
                                      ::platform
                                      ::counter
                                      ::seed-map
                                      ::lvar-bindings
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
                    (seed/referents [])
                    (seed/access-deps {})))

(defn ensure-state [x]
  (assert (state? x))
  x)

;;;------- State operations -------
(def empty-state
  {:output nil

   :prefix ""

   :lvar-bindings []

   :seed-cache {}

   :seed-cache-stack []

   :mode-stack []

   :max-mode :pure

   :platform :clojure
   ;; Used to assign ids to seeds
   :counter 0

   ;; All the seeds
   :seed-map {}

   :ids-to-visit []

   })

(defn get-last-seed [state]
  {:pre [(state? state)]
   :post [(seed/seed? state)]}
  (get-in state [:seed-map (:counter state)]))

(def ^:dynamic state-atom nil)

(defn wrap-f-args [f args]
  (fn [x] (apply f (into [x] args))))

(defn swap-state! [f & args]
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
  (get-output (swap-state!
               (comp put-in-output (wrap-f-args f args)))))

(checked-defn add-binding [:when check-debug
                           ::state state

                           any? sym
                           any? expr

                           :post ::state]
              (update state :lvar-bindings conj {:name sym
                                                 :result expr}))


(checked-defn get-lvar-bindings [::state state
                                 :post ::lvar-bindings]
              (:lvar-bindings state))


(checked-defn clear-lvar-bindings [::state state
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
       seed0 (merge empty-seed
                    (set-seed-id seed0 id))
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
                   (seed/description "end")
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

(defn disp-indented [state & args]
  (println (apply str (into [(:prefix state)]
                            args))))

(checked-defn return-state [:when check-debug

                            ::state x

                            ::seed-id end-id]
  (if (not returned-state)
    (throw (ex-info "No returned-state atom"))
    (let [r (swap! returned-state merge {:end-at end-id}

                   ;; Dissociate any other begin-at,
                   ;; because x should already contain it.
                   (dissoc x :begin-at))]
      (disp-indented x "Return result to " (:begin-at r)))))

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
    (do
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
 maybe-bind-result [::state state
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

              generated-code (binding [returned-state returned-state-to-bind]
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
                  end-id (:end-at state)]
              (when (= state init-return-state)
                (throw (ex-info "Missing end-scope!"
                                {:begin-id id})))
              (when (not= (:begin-at state) id)
                (throw (ex-info "Bad begin-at"
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

(defn check-referent-visibility-for-id [state id]
  {:pre [(set? (:invisible state))
         (vector? (:begin-stack state))]}
  (let [seed (get-seed state id)]
    (if (seed/has-special-function? seed :begin)
      (update state :begin-stack conj id)
      (let [invisible (:invisible state)
            deps (-> seed seed/access-deps vals set)
            intersection (cljset/intersection deps invisible)]
        (if (not (empty? intersection))
          (throw (ex-info "Seed refers to other seeds in closed scope"
                          {:seed-id id
                           :deps intersection})))
        (if (seed/has-special-function? seed :end)
          (let [begin-stack (:begin-stack state)
                begin-id (last begin-stack)]
            (-> state
                (update :invisible into (range begin-id id))
                (update :begin-stack butlast-vec)))
          state)))))

(checked-defn check-referent-visibility [::state state
                                         :post ::state]
  (reduce
   check-referent-visibility-for-id
   (merge state {:begin-stack []
                 :invisible #{}})
   (:ids-to-visit state)))

(defn to-coll-expression [c]
  (if (seq? c)
    (cons 'list c)
    c))

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

(checked-defn disp-state [::state state]
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
  (generate-code-from state))

(defn set-flag! [& flags]
  {:pre [(spec/valid? ::flags flags)]}
  (swap-state! (fn [state]
                 (reduce (fn [state flag]
                           (assoc state flag true))
                         state flags))))













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
                   (symbol (format "s%03d" (:seed-id seed))))

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

(defn demo-compile-call-fn [comp-state expr cb]
  (let [compiled-deps (seed/access-compiled-indexed-deps expr)]
    (cb (defs/compilation-result
          comp-state
          `(~(:f expr) ~@compiled-deps)))))

(checked-defn demo-call-fn [::seed/mode mode
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

