#!/usr/bin/env bb

(def data
  (->> (clojure.string/split (slurp "input15.txt") #"\n")
       (map seq)
       (mapv (fn [xs] (mapv #(Integer/parseInt (str %)) xs)))))

(def additions (into [] (for [row (range 5)]
                          (into [] (map #(+ row %) (range 5))))))

(def expanded-data
  (vec
   (apply
    concat
    (for [addition-row additions]
      (apply
       map
       (fn [& args] (vec (flatten args)))
       (for [addition addition-row]
         (mapv (fn [xs] (mapv #(let [sum (+ addition %)] (if (<= sum 9) sum (inc (mod sum 10)))) xs)) data)))))))

(defn print-data [data]
  (doseq [line data]
    (println (clojure.string/join line))))

#_(print-data expanded-data)

(defn directions [[row col]]
  (if (and (= row (-> expanded-data count dec))
           (= col (-> expanded-data first count dec)))
    []
    (let [down-rows? (< 0 row)
          up-rows? (< (inc row) (count expanded-data))
          left-cols? (< 0 col)
          right-cols? (< (inc col) (-> expanded-data first count))]
      (remove
       nil?
       [(when right-cols? [row (inc col)])
        (when down-rows? [(dec row) col])
        (when up-rows? [(inc row) col])
        (when left-cols? [row (dec col)])]))))

(def counter (atom 0))

(defn next-int []
  (swap! counter inc))

(def big-number (* 1000 1000 1000))

(defn minimum-cost [costs start-coordinate end-coordinate]
  (loop [cost->coordinate (into (sorted-map) {[0 0] start-coordinate})
         coordinate->cost {start-coordinate [0 0]}]
    (let [[[cost :as k] coordinate] (first cost->coordinate)
          neighbor-coordinates (directions coordinate)
          neighbor-costs (map #(get-in costs %) neighbor-coordinates)
          new-directions (for [[neighbor-cost neighbor-coordinate] (map list neighbor-costs neighbor-coordinates)
                               :let [new-cost [(+ cost neighbor-cost) (next-int)]
                                     new-coordinate neighbor-coordinate]
                               :when (< (first new-cost) (first (get coordinate->cost new-coordinate [big-number])))]
                           [new-cost new-coordinate])
          removed-costs (->> new-directions (map second) (map coordinate->cost))]
      (println k coordinate (count cost->coordinate) (count coordinate->cost))
      (if (= coordinate end-coordinate)
        cost
        (recur
         (-> (apply dissoc cost->coordinate removed-costs)
             (dissoc k)
             (into  new-directions))
         (into coordinate->cost (map (fn [[a b]] [b a]) new-directions)))))))

(def end-coordinate [(-> expanded-data count dec) (-> expanded-data first count dec)])

(minimum-cost expanded-data [0 0] end-coordinate)
