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
  (->> (for [[row heightrow] (map-indexed vector heightmap)]
         (for [[col height] (map-indexed vector heightrow)]
           (when (lower-than-neighbors? row col)
             [row col])))
       (apply concat)
       (remove nil?)
       set))

(defn bfs [point]
  (loop [basin #{}
         [[r c :as point] :as frontier] [point]]
    (if (-> frontier count zero?)
      basin
      (let [new-frontier (subvec frontier 1)]
        (if (or (contains? #{##Inf 9} (heightmap-get r c)) ;; Off board or peak
                (contains? basin point)) ;; Already processed
          (recur basin new-frontier)
          (recur (conj basin point)
                 (->> [[1 0] [-1 0] [0 1] [0 -1]]
                      (map (fn [[delta-r delta-c]] [(+ r delta-r) (+ c delta-c)]))
                      (into new-frontier))))))))

;; I guess we don't deal with the possibility of a single basin having 2+ peaks?
(->> lowest-points
     (map bfs)
     (map count)
     (sort-by #(* -1 %))
     (take 3)
     (apply *))
