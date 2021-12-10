#!/usr/bin/env bb

(def data (slurp "input07.txt"))

(def crabs
  (as-> data $
    (clojure.string/replace $ #"\n" "")
    (clojure.string/split $ #",")
    (map #(Integer/parseInt %) $)
    (frequencies $)))

(def highest-crab (apply max (keys crabs)))

(def lowest-crab (apply min (keys crabs)))

(first ;; take the fuel from the [fuel height] tuple
 (apply
  min-key
  first ;; Compare by fuel
  (for [height (range lowest-crab (inc highest-crab))]
    [(apply + (for [[crab cnt] crabs] (* cnt (Math/abs (- crab height))))) height])))



