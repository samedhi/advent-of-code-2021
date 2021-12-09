#!/usr/bin/env bb

(def data (slurp "input06.txt"))

(def fishies
  (as-> data $
    (clojure.string/replace $ #"\n" "")
    (clojure.string/split $ #",")
    (map #(Integer/parseInt %) $)))

(defn daily-grind [fish]
  (if (zero? fish)
    [6 8]
    [(dec fish)]))

(defn next-day [fishies]
  (apply concat (map daily-grind fishies)))

(def eighty-days
  (last (take 81 (iterate next-day fishies)))) ;; 1st is day zero

(count eighty-days)
