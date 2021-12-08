#!/usr/bin/env bb

(def input
  (as-> (slurp "input03.txt") $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #"") $)
    (map (fn [binary] (mapv #(Integer/parseInt %) binary)) $)))

(defn frequency-at-i [i xs]
  (->> xs
       (map #(nth % i))
       (frequencies)))

(defn most-common-bit-at-i [i bits]
  (let [{zeros 0 ones 1} (frequency-at-i i bits)]
    (if (<= zeros ones) 1 0)))

(defn least-common-bit-at-i [i bits]
  (let [{zeros 0 ones 1} (frequency-at-i i bits)]
    (if (<= zeros ones) 0 1)))

(defn find-candidate [desired-bit-fx candidates]
  (loop [i 0
         candidates candidates]
    (if (= (count candidates) 1)
      (first candidates)
      (let [desired-bit (desired-bit-fx i candidates)]
        (recur
         (inc i)
         (filterv #(= desired-bit (nth % i)) candidates))))))

(def oxygen-generator-rating (find-candidate most-common-bit-at-i input))
(def co2-scrubber-rating (find-candidate least-common-bit-at-i input))

(->> [oxygen-generator-rating co2-scrubber-rating]
     (map #(clojure.string/join "" %))
     (map #(Integer/parseUnsignedInt % 2))
     (apply *))
