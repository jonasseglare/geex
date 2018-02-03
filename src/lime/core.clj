(ns lime.core
  (:require [bluebell.utils.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]))
;;;;;;;;;;;;;;;;;;
(spec/def ::comp-state (spec/keys :req [::result
                                        ::node-map]))

(def compilation-result (party/key-accessor ::compilation-result))
(def node-map (party/key-accessor ::node-map))


;;;;;;;;;;;;;;;;;;;,
;; State used during meta-evaluation
(def state (atom {::last-dirty nil
                  ::requirements []
                  ::dirty-counter 0}))

(spec/def ::seed (spec/keys :req [::type
                                  ::compiler
                                  ::deps]))

(spec/def ::snapshot (spec/keys :req [::result-value
                                      ::last-dirty]))

(def recording? (party/key-accessor ::recording?))

;; Test if something is a seed
(defn seed? [x]
  (spec/valid? ::seed x))

;; Access the last dirty
(def last-dirty (party/key-accessor ::last-dirty))

;; Access the requirements
(def requirements (party/key-accessor ::requirements))

;; Access the dirty-counter
(def dirty-counter (party/key-accessor ::dirty-counter))

;; Increase the counter of the state map
(def inc-counter #(party/update % dirty-counter inc))

;; Helper for with-requirements
(defn append-requirements [r s]
  (party/update s requirements #(into % r)))

(defn with-requirements [r f]
  (assert (fn? f))
  (let [initial-reqs (-> state deref requirements)
        new-reqs (swap! state (partial append-requirements r))
        result (f)
        old-reqs (swap! state #(requirements % initial-reqs))]
    result))

;; Associate the requirements with random keywords in a map,
;; so that we can merge it in deps.
(defn make-req-map []
  (into {} (map (fn [x] [(keyword (gensym "req")) x])
                (-> state deref requirements))))


;; Special access to a dirty, if any
(def dirty (party/key-accessor ::dirty))


;; The dependencies of a seed
(def deps (party/key-accessor ::deps))

;; The compiler of a seed
(def compiler (party/key-accessor ::compiler))

;; Access the datatype of the seed
(def datatype (party/key-accessor ::type))

(def description (party/key-accessor ::description))

;; Create a new seed, with actual requirements
(defn initialize-seed [desc]
  (assert (string? desc))
  (-> {}
      (deps (make-req-map))
      (compiler nil)
      (datatype nil)
      (description desc)))

;; Extend the deps map
(defn add-deps [dst extra-deps]
  (party/update dst deps #(merge % extra-deps)))

;; Access the last dirty in the deps map
(def last-dirty-dep (party/chain deps last-dirty))

;; Call this function when a seed has been constructed,
;; but is side-effectful
(defn dirty [x]
  (last-dirty
   (swap! state
          (fn [s]
            (inc-counter
             (last-dirty
              s
              (-> x
                  (dirty-counter (dirty-counter s))
                  (last-dirty-dep (last-dirty s)))))))))


;; Access a backup place for the dirty, when using record-dirties
(def backup-dirty (party/key-accessor ::backup-dirty))

;; Access result value, of a snapshot type
(def result-value (party/key-accessor ::result-value))

(def snapshot? (partial spec/valid? ::snapshot))

(defn replace-dirty [s new-dirty]
  (-> s
      (backup-dirty (last-dirty s))
      (last-dirty new-dirty)))

;; Given an initial dirty, initialize the state
;; with that dirty, call (f) without any arguments,
;; and then return the result of f along with the final dirty
(defn record-dirties [initial-dirty f]
  (let [start-state (swap! state #(replace-dirty % initial-dirty))
        out (f)
        restored-state (swap! state #(replace-dirty % (backup-dirty start-state)))]
    (-> {}
        (result-value out)
        (last-dirty (backup-dirty restored-state)))))

;; The opposite of the above: f gets as input the last dirty,
;; and then it returns a snapshot with the result and
;; the new dirty that we'd like to use after this.
(defn inject-pure-code [f]
  (let [current-state (deref state)
        snapshot (f (last-dirty current-state))]
    (assert (snapshot? snapshot))
    (swap! state #(last-dirty % (last-dirty snapshot)))
    (result-value snapshot)))

;; TODO: Analyze all collections
;;       Build a map from keyword to expr
;;       Traverse expr and replace all exprs by their keys
;;       Start write compiler
;;       Later on: Ability to delay propagation (e.g. when evaluating the if)

;;; Accessors

;; Access the deps of a seed
(def seed-deps-accessor (party/conditional-accessor

                         ;; Extract the dependency map, then the values
                         ;; for ordered keys
                         (party/chain deps utils/map-vals-accessor)

                         ;; Let anything else than a seed? fall through.
                         seed?))

;; Access the original-coll
(def access-original-coll (party/key-accessor :original-coll))

;; Access a collection as indexed elements in a map
(defn access-indexed-map
  ([] {:desc "access-indexed-map"})
  ([x] (mapv second (sort-by first x)))
  ([x y] (merge x (zipmap (range (count y)) y))))


;; Access indexed dependencies
(def access-indexed-deps (party/chain deps access-indexed-map))

(defn lookup-compiled-results [state arg-map]
  (assert (map? arg-map))
  (let [m (node-map state)]
    (into {} (map (fn [[k v]]
                    [k (compilation-result (get m v))]) arg-map))))

;; Compiler for the coll-seed type
(defn compile-coll [state expr cb]
  (cb (compilation-result
       state
       (utils/normalized-coll-accessor
        (access-original-coll expr)
        (access-indexed-map
         (lookup-compiled-results state (deps expr)))))))

(defn coll-seed [x]
  (-> (initialize-seed "coll-seed")
      (access-indexed-deps (utils/normalized-coll-accessor x))
      (access-original-coll x)
      (compiler compile-coll)))

(def primitive-value (party/key-accessor :primitive-value))

(defn value-literal-type [x]
  (class x))

(defn compile-primitive-value [state expr cb]
  (cb (compilation-result state (primitive-value expr))))

(defn primitive-seed [x]
  (assert (not (coll? x)))
  (-> (initialize-seed "primitive-seed")
      (primitive-value x)
      (datatype (value-literal-type x))
      (compiler compile-primitive-value)))

;; Rewrites all collections into seeds, for easier analysis
(defn expand-collections [x]
  )
