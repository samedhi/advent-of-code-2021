#!/usr/bin/env bb

(def data (slurp "input06.txt"))

(def fishies
  (as-> data $
    (clojure.string/replace $ #"\n" "")
    (clojure.string/split $ #",")
    (map #(Integer/parseInt %) $)
    (frequencies $)))

(defn daily-grind [[timer cnt]]
  (if (zero? timer)
    {8 cnt 6 cnt}
    {(dec timer) cnt}))

(defn next-day [fishies]
  (apply merge-with + (map daily-grind fishies)))

(def day-256
  (first (drop 256 (iterate next-day fishies)))) ;; 1st is day zero

(->> day-256 vals (apply +))
