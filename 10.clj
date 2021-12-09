#!/usr/bin/env bb

(def data (slurp "input05.txt"))

(defn expand-segment [[x1 y1 x2 y2]]
  (cond
    (= x1 x2) (let [[y1 y2] (sort [y1 y2])]
                (map #(vector x1 %) (range y1 (inc y2))))
    (= y1 y2) (let [[x1 x2] (sort [x1 x2])]
                (map #(vector % y1) (range x1 (inc x2))))
    :else (let [x-multiplier (if (< x1 x2) 1 -1)
                y-multiplier (if (< y1 y2) 1 -1)
                [start end] (sort [y1 y2])]
            (map #(vector (+ (* % x-multiplier) x1)
                          (+ (* % y-multiplier) y1))
                 (range (- (inc end) start))))))

;; (assert (= (expand-segment [1 1 3 3]) [[1 1] [2 2] [3 3]]))
;; (assert (= (expand-segment [9 7 7 9]) [[9 7] [8 8] [7 9]]))

(def point-frequencies
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #" -> ") $)
    (map #(clojure.string/join "," %) $)
    (map #(clojure.string/split % #",") $)
    (map (fn [line] (mapv #(Integer/parseInt %) line)) $)
    (map expand-segment $)
    (remove nil? $)
    (apply concat $)
    (frequencies $)))

(defn visualize [frequencies]
  (let [max-x (inc (apply max (map first (keys frequencies))))
        max-y (inc (apply max (map second (keys frequencies))))]
    (doseq [x (range max-x)]
      (doseq [y (range max-y)]
        (print (get frequencies [y x] "."))) ;; Why does this seem backwards?
      (println))))

;; (visualize point-frequencies)
;; =>
;; 1.1....11.
;; .111...2..
;; ..2.1.111.
;; ...1.2.2..
;; .112313211
;; ...1.2....
;; ..1...1...
;; .1.....1..
;; 1.......1.
;; 222111....

(def dangerous-overlap-count (count (filter (fn[[k v]] (<= 2 v)) point-frequencies)))
dangerous-overlap-count
