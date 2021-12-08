#!/usr/bin/env bb

(defn append-bits [acc bits]
  (reduce
   (fn [acc [index bit]] (update-in acc [index] conj bit))
   acc
   (map-indexed vector bits)))

(def gamma
  (as-> (slurp "input03.txt") $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #"") $)
    (map (fn [binary] (mapv #(Integer/parseInt %) binary)) $)
    (reduce append-bits (vec (repeat (count (first $)) [])) $)
    (map frequencies $)
    (map (fn [m] (if (< (get m 0) (get m 1)) 1 0)) $)))

(def epsilon
  (map {0 1 1 0} gamma))

(->> [gamma epsilon]
     (map #(clojure.string/join "" %))
     (map #(Integer/parseUnsignedInt % 2))
     (apply *))
