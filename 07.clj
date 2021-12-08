#!/usr/bin/env bb

(def data (clojure.string/split (slurp "input04.txt") #"\n"))

(def numbers
  (as-> data $
    (first $)
    (clojure.string/split $ #",")
    (mapv #(Integer/parseInt %) $)))

(def cards
  (as-> data $
    (rest $)
    (remove empty? $)
    (map #(clojure.string/split % #"\s+") $)
    (map #(remove empty? %) $)
    (map (fn [line] (mapv #(Integer/parseInt %) line)) $)
    (partition 5 $)))

(defn mark-all-v [card v]
  (for [line card]
    (mapv (fn [n] (if (= n v) #{n} n)) line)))

(defn row-win? [card]
  (some
   true?
   (for [row card]
     (every? set? row))))

(defn transpose [m]
  (apply mapv vector m))

(defn col-win? [card]
  (some
   true?
   (for [row (transpose card)]
     (every? set? row))))

(defn bingo? [card]
  (or (row-win? card)
      (col-win? card)))

(defn play-round [cards v]
  (map #(mark-all-v % v) cards))

(def rounds (reductions play-round cards numbers))

(def game-states (split-with #(not-any? bingo? %) rounds))
(def playing-states (first game-states))
(def winning-states (second game-states))

(def winning-round (first winning-states))

;; What happens if two cards win?
(def winning-card (filter bingo? winning-round))

(def card-score (apply + (remove set? (flatten winning-card))))

(def last-number (->> playing-states count dec (nth numbers)))

(* card-score last-number)
