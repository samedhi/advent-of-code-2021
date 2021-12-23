#!/usr/bin/env bb

(def data
  (->> (clojure.string/split (slurp "input15.txt") #"\n")
       (map seq)
       (mapv (fn [xs] (mapv #(Integer/parseInt (str %)) xs)))))

(defn directions [[row col]]
  (let [within-rows? (< (inc row) (count data))
        within-cols? (< (inc col) (-> data first count))]
    (cond
      (and within-rows? within-cols?) [[(inc row) col] [row (inc col)]]
      within-rows? [[(inc row) col]]
      within-cols? [[row (inc col)]]
      :else [])))

(declare minimum-cost-memo)

(defn minimum-cost [coordinate]
  (let [local-cost (get-in data coordinate)
        available-paths (directions coordinate)
        children-costs (doall (map minimum-cost-memo available-paths))]
    (+ local-cost (if (empty? children-costs) 0 (apply min children-costs)))))

(def minimum-cost-memo (memoize minimum-cost))

(- (minimum-cost-memo [0 0]) (get-in data [0 0]))
