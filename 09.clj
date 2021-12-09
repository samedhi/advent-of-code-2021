#!/usr/bin/env bb

(def data (slurp "input05.txt"))

(defn expand-segment [[x1 y1 x2 y2]]
  (cond
    (= x1 x2) (let [[y1 y2] (sort [y1 y2])]
                (map #(vector x1 %) (range y1 (inc y2))))
    (= y1 y2) (let [[x1 x2] (sort [x1 x2])]
                (map #(vector % y1) (range x1 (inc x2))))))

(def point-frequencies
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #" -> ") $)
    (map #(clojure.string/join "," %) $)
    (map #(clojure.string/split % #",") $)
    (map (fn [line] (mapv #(Integer/parseInt %) line)) $)
    (map expand-segment $)
    (remove nil? $) ;; Remove the non horizontal or vertical segments
    (apply concat $)
    (frequencies $)))

(def dangerous-overlap-count (count (filter (fn[[k v]] (<= 2 v)) point-frequencies)))
dangerous-overlap-count
