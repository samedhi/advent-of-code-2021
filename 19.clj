#!/usr/bin/env bb

(def data (slurp "input10.txt"))

(def lines
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #"") $)))

(def open->close
  {"<" ">"
   "[" "]"
   "(" ")"
   "{" "}"})

(def close->open (clojure.set/map-invert open->close))

(defn valid? [stack c]
  (if (contains? open->close c)
    (do (swap! stack conj c) true)
    (if (= (close->open c) (peek @stack))
      (do (swap! stack pop) true)
      false)))

(def scoring
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn score [acc c]
  (+ (* acc 5) (scoring c)))

(as-> (for [line lines]
       (let [stack (atom [])
             predicate (partial valid? stack)
             valid-line (take-while predicate line)]
         (when (= valid-line line)
           (map open->close (reverse @stack))))) $
     (remove nil? $)
     (map #(reduce score 0 %) $)
     (sort $)
     (drop (-> $ count (/ 2) int) $)
     (first $))
