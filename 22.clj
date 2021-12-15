#!/usr/bin/env bb

(def data (slurp "input11.txt"))

(def octopi
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #"") $)
    (map (fn [line] (mapv #(Integer/parseInt %) line)) $)
    (vec $)))

(def largest-x (-> octopi count dec))
(def largest-y (-> octopi first count dec))

(defn neighbors [x y]
  (for [x-delta [-1 0 1]
        y-delta [-1 0 1]
        :let [neighbor-x (+ x x-delta)
              neighbor-y (+ y y-delta)]
        :when (and
               (or (not= x-delta 0)
                   (not= y-delta 0))
               (<= 0 neighbor-x largest-x)
               (<= 0 neighbor-y largest-y))]
    [neighbor-x neighbor-y]))

(defn energize [octopi]
  (mapv #(mapv inc %) octopi))

(defn flashers [octopi]
  (for [x (-> octopi count range)
        y (-> octopi first count range)
        :let [energy (nth (nth octopi x) y)]
        :when (< 9 energy)]
    [x y]))

(defn flash-neighbors [octopi flashers]
  (reduce
   (fn [octopi [x y]]
     (reduce
      #(update-in %1 %2 inc)
      octopi
      (neighbors x y)))
   octopi
   flashers))

(defn step [octopi]
  (loop [octopi (energize octopi)
         seen-flashers #{}]
    (let [new-flashers (remove #(contains? seen-flashers %) (flashers octopi))]
      (if (empty? new-flashers)
        (reduce #(assoc-in %1 %2 0) octopi seen-flashers)
        (recur
         (flash-neighbors octopi new-flashers)
         (into seen-flashers new-flashers))))))

(defn all-spent? [octopi]
  (every? zero? (flatten octopi)))

(->> octopi
     (iterate step)
     (take-while (complement all-spent?))
     count)
