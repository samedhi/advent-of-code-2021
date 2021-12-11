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

(def scoring
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(defn valid-fx? []
  (let [stack (atom [])]
    (fn [c]
      (if (contains? open->close c)
        (do (swap! stack conj c) true)
        (if (= (close->open c) (peek @stack))
          (do (swap! stack pop) true)
          (do (reset! stack nil) false))))))

(apply
 +
 (for [line lines
       :let [bad-character (first (drop-while (valid-fx?) line))]
       :when (some? bad-character)]
   (scoring bad-character)))
