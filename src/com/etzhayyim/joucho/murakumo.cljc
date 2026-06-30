(ns com.etzhayyim.joucho.murakumo
  "Religious-corp Joucho aggregation actor migrated from kotoba-kotodama."
  (:require [clojure.string :as str]))

(def actor-did "did:web:joucho.etzhayyim.com")
(def joucho-collection "com.etzhayyim.joucho.joucho")
(def default-window-days 7)

(def new-adherent-defaults
  {:joy 400
   :calm 400
   :stress 200
   :gratitude 300
   :focus 400})

(def signal-kind->axis-weights
  {"ritual" {"joy" 800 "focus" 300}
   "oath" {"calm" 900 "focus" 700}
   "contribution" {"gratitude" 1000 "focus" 500}
   "governance-participation" {"calm" 800}
   "kuniUmi-witness" {"joy" 600}})

(def mood-thresholds
  {:joyful [:joy 600]
   :calm [:calm 600]
   :stressed [:stress 700]
   :grateful [:gratitude 600]
   :focused [:focus 600]})

(defn clamp
  [lo hi n]
  (-> n (max lo) (min hi)))

(defn aggregate-signals
  "Pure port of joucho_murakumo.aggregate_signals.

  Signals are maps with `:signalKind`/`signalKind` and `:weight`/`weight`.
  Returned axes are permille integers in [0, 1000]."
  [signals]
  (if (empty? signals)
    new-adherent-defaults
    (let [signal-count (count signals)
          sums (reduce
                (fn [acc signal]
                  (let [kind (or (:signalKind signal) (get signal "signalKind") "")
                        weight (long (or (:weight signal) (get signal "weight") 0))]
                    (reduce-kv
                     (fn [m axis factor]
                       (update m (keyword axis) (fnil + 0) (quot (* weight factor) 1000)))
                     acc
                     (get signal-kind->axis-weights kind {}))))
                {:joy 0 :calm 0 :gratitude 0 :focus 0}
                signals)
          axes (reduce-kv
                (fn [m axis total]
                  (assoc m axis (clamp 0 1000 (quot total signal-count))))
                {}
                sums)
          positive-mean (quot (+ (:joy axes) (:calm axes) (:gratitude axes) (:focus axes)) 4)]
      (assoc axes :stress (clamp 0 1000 (- 1000 positive-mean))))))

(defn classify-mood
  [axes]
  (let [axes* (merge new-adherent-defaults axes)]
    (cond
      (>= (:stress axes*) (second (:stressed mood-thresholds))) :stressed
      (>= (:joy axes*) (second (:joyful mood-thresholds))) :joyful
      (>= (:calm axes*) (second (:calm mood-thresholds))) :calm
      (>= (:gratitude axes*) (second (:grateful mood-thresholds))) :grateful
      (>= (:focus axes*) (second (:focused mood-thresholds))) :focused
      :else :neutral)))

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn build-joucho-record
  [{:keys [adherent-did signals computed-at aggregator-node from-signals-since from-signal-days]}]
  (let [signals* (or signals [])
        axes (aggregate-signals signals*)]
    (merge
     {:$type joucho-collection
      :adherentDid adherent-did
      :computed_at computed-at
      :from_signal_count (count signals*)
      :from_signal_days (or from-signal-days default-window-days)
      :mood (classify-mood axes)}
     axes
     (when aggregator-node {:aggregator_node aggregator-node})
     (when from-signals-since {:from_signals_since from-signals-since}))))

(defn aggregation-effect
  [input]
  (let [record (build-joucho-record input)]
    {:op :mst/put-record
     :actor actor-did
     :collection joucho-collection
     :rkey (str "joucho-" (safe-rkey (:adherent-did input)))
     :record record}))

(defn aggregation-cell-plan
  "Plan for legacy joucho_aggregation_cell."
  [input]
  {:cell :joucho-aggregation
   :record (build-joucho-record input)
   :effects [(aggregation-effect input)]})

(defn fetch-joucho-effect
  "Read effect for legacy fetch_joucho."
  [adherent-did]
  {:op :mst/get-record
   :actor actor-did
   :collection joucho-collection
   :rkey (str "joucho-" (safe-rkey adherent-did))
   :adherentDid adherent-did})
