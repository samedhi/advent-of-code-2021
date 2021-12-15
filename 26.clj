#!/usr/bin/env bb

(def data (clojure.string/split (slurp "input13.txt") #"\n\n"))

(def dots
  (as-> data $
    (first $)
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #",") $)
    (map (fn [[row col]] [(Integer/parseInt row) (Integer/parseInt col)]) $)
    (set $)))

(def folds
  (as-> data $
    (second $)
    (clojure.string/split $ #"\n")
    (map #(re-find #"fold along ((x|y)=(\d+))" %) $)
    (map (fn [[_ _ axis num]] [axis (Integer/parseInt num)]) $)))

(defn x-fold [dots fold-line]
  (let [above (filter (fn [[x y]] (< x fold-line)) dots)
        below (filter (fn [[x y]] (< fold-line x)) dots)
        below-mirrored (map (fn [[x y]] [(- fold-line (- x fold-line)) y]) below)]
    (into (set above) below-mirrored)))

(defn y-fold [dots fold-line]
  (let [left (filter (fn [[x y]] (< y fold-line)) dots)
        right (filter (fn [[x y]] (< fold-line y)) dots)
        right-mirrored (map (fn [[x y]] [x (- fold-line (- y fold-line))]) right)]
    (into (set left) right-mirrored)))

(defn display [dots]
  (let [max-x (apply max (map second dots))
        max-y (apply max (map first dots))]
    (clojure.string/join
     "\n"
     (for [x (range (inc max-x))]
       (clojure.string/join
        (for [y (range (inc max-y))]
          (if (contains? dots [y x]) "#" " ")))))))

(defn folder [dots [axis line]]
  (if (= axis "x")
    (x-fold dots line)
    (y-fold dots line)))

(->
 ;; dots
 ;; (y-fold 7)
 ;; (x-fold 5)
 (reductions folder dots folds)
 last
 display
 println)
