(require '[cljs.test :as t]
         '[com.etzhayyim.joucho.murakumo-test])

(let [result (t/run-tests 'com.etzhayyim.joucho.murakumo-test)]
  (when (pos? (+ (:fail result 0) (:error result 0)))
    (js/process.exit 1)))
