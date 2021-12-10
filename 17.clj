#!/usr/bin/env bb

(def data (slurp "input09.txt"))

(def heightmap
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #"") $)
    (mapv (fn [row] (mapv #(Integer/parseInt %) row)) $)))

(defn heightmap-get [row col]
  (or (get (get heightmap row) col) ##Inf))

(defn lower-than-neighbors? [row col]
  (let [center-height (heightmap-get row col)
        compare-fn (fn [row col] (< center-height (heightmap-get row col)))]
    (every?
     #(apply compare-fn %)
     [[(inc row) col] [(dec row) col] [row (inc col)] [row (dec col)]])))

(def lowest-points
  (for [[row heightrow] (map-indexed vector heightmap)]
    (for [[col height] (map-indexed vector heightrow)]
      (when (lower-than-neighbors? row col)
        height))))

(let [results (->> lowest-points
                   (apply concat)
                   (remove nil?))]
  (apply + (count results) results))
