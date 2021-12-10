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

(def lookup (atom {0 0}))

(defn fuel-cost [distance]
  (when-not (contains? @lookup distance)
    (swap! lookup assoc distance (+ distance (fuel-cost (dec distance)))))
  (get @lookup distance))

(first ;; take the fuel from the [fuel height] tuple
 (apply
  min-key
  first ;; Compare by fuel
  (for [height (range lowest-crab (inc highest-crab))]
    [(apply + (for [[crab cnt] crabs
                    :let [distance (Math/abs (- crab height))
                          fuel (fuel-cost distance)]]
                (* cnt fuel)))
     height])))



