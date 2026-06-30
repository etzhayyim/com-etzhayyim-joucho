(ns com.etzhayyim.joucho.murakumo-test
  (:require [clojure.test :refer [deftest is]]
            [com.etzhayyim.joucho.murakumo :as joucho]))

(deftest aggregate-empty-signals
  (is (= joucho/new-adherent-defaults
         (joucho/aggregate-signals []))))

(deftest aggregate-weighted-signals
  (is (= {:joy 266 :calm 300 :gratitude 333 :focus 500 :stress 651}
         (joucho/aggregate-signals [{:signalKind "ritual" :weight 1000}
                                    {:signalKind "oath" :weight 1000}
                                    {:signalKind "contribution" :weight 1000}]))))

(deftest mood-classification
  (is (= :stressed (joucho/classify-mood {:stress 700 :joy 900})))
  (is (= :joyful (joucho/classify-mood {:joy 600 :stress 100})))
  (is (= :neutral (joucho/classify-mood {}))))

(deftest aggregation-effect-shape
  (let [effect (joucho/aggregation-effect {:adherent-did "did:web:alice.example"
                                           :signals []
                                           :computed-at "2026-06-29T00:00:00Z"
                                           :aggregator-node "levi"})]
    (is (= :mst/put-record (:op effect)))
    (is (= joucho/joucho-collection (:collection effect)))
    (is (= "joucho-alice.example" (:rkey effect)))
    (is (= "did:web:alice.example" (get-in effect [:record :adherentDid])))
    (is (= :neutral (get-in effect [:record :mood])))))

(deftest legacy-cell-and-fetch-effects
  (let [plan (joucho/aggregation-cell-plan {:adherent-did "did:web:alice.example"
                                            :signals [{:signalKind "kuniUmi-witness" :weight 1000}]
                                            :computed-at "2026-06-29T00:00:00Z"})
        read-effect (joucho/fetch-joucho-effect "did:web:alice.example")]
    (is (= :joucho-aggregation (:cell plan)))
    (is (= :mst/put-record (get-in plan [:effects 0 :op])))
    (is (= "joucho-alice.example" (get-in plan [:effects 0 :rkey])))
    (is (= :mst/get-record (:op read-effect)))
    (is (= joucho/joucho-collection (:collection read-effect)))))
