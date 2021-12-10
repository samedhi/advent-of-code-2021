#!/usr/bin/env bb

(def num->display {0 #{"a" "b" "c" "e" "f" "g"}
                   1 #{"c" "f"}
                   2 #{"a" "c" "d" "e" "g"}
                   3 #{"a" "c" "d" "f" "g"}
                   4 #{"b" "c" "d" "f"}
                   5 #{"a" "b" "d" "f" "g"}
                   6 #{"a" "b" "d" "e" "f" "g"}
                   7 #{"a" "c" "f"}
                   8 #{"a" "b" "c" "d" "e" "f" "g"}
                   9 #{"a" "b" "c" "d" "f" "g"}})

(def display->num (clojure.set/map-invert num->display))

(def data (slurp "input08.txt"))

(defn parse-display [input]
  (set (clojure.string/split input #"")))

(def captured-data
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #" \| ") $)
    (map (fn [row] (mapv #(clojure.string/split % #"\s+") row)) $)
    (map #(update-in % [0] (fn [test] (into #{} (map parse-display test)))) $)
    (map #(update-in % [1] (fn [test] (into [] (map parse-display test)))) $)))

(def correct-wiring ["a" "b" "c" "d" "e" "f" "g"])

;; https://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(def all-possible-wirings (permutations correct-wiring))

(defn find-mappings [actual-display]
  (for [possible-wiring all-possible-wirings
       :let [wiring-map (zipmap correct-wiring possible-wiring)
             scrambled-display (->> (range 10) ;; for all 10 numbers
                                    ;; get the 10 display patterns
                                    (map num->display) 
                                    ;; map each to this scrambled display patterns
                                    (map #(map wiring-map %))
                                    (map set)
                                    set)]
        :when (= scrambled-display actual-display)]
    wiring-map))

(apply
 +
 (for [[test-data result-data] captured-data
       :let [mapping (clojure.set/map-invert (first (find-mappings test-data)))]] ;; there should be only 1
   (Integer/parseInt
    (clojure.string/join
     (for [result result-data]
       (->> (map mapping result)
            set
            display->num))))))

;; DEV NOTE: This code is so bad
