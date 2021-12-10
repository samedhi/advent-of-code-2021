#!/usr/bin/env bb

(def data (slurp "input08.txt"))

(def display-data
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #" \| ") $)
    (map (fn [row] (mapv #(clojure.string/split % #"\s+") row)) $)))

(->> display-data
     (map second)
     (apply concat)
     (filter #(contains? #{2,3,4,7} (count %)))
     count)

