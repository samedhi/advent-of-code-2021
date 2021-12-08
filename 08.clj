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

(def still-in-it-to-loose-it-states (take-while #(not-every? bingo? %) rounds))

(def last-loosing-round (last still-in-it-to-loose-it-states))
last-loosing-round


(def last-loosing-card (remove bingo? last-loosing-round))

(def last-number (->> still-in-it-to-loose-it-states count dec (nth numbers)))

(def card-score (apply +
                       (* -1 last-number) ;; You haven't yet lost (won) this card
                       (remove set? (flatten last-loosing-card))) )

(* card-score last-number)

;; Dev Note: Man, I should really have put each rounds "called number" value in a map with the
;; end of round state of the board. Figuring out what the value of the round was is a pain.
