#!/usr/bin/env bb

(require '[clojure.test :as test :refer [deftest testing is are]])

(->> (clojure.string/split (slurp "input02.txt") #"\n")
     (map #(clojure.string/split % #" ") )
     (map (fn [[op v]] [op (Integer/parseInt v)]))
     (map (fn [[op v]] (if (= op "up") ["down" (* -1 v)] [op v])))
     (map (fn [[op v]] {op v}))
     (apply merge-with +)
     vals
     (apply *))



