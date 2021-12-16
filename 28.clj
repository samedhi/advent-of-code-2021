#!/usr/bin/env bb

(def data (clojure.string/split (slurp "input14.txt") #"\n\n"))

(def start
  (->> data
      first
      (partition 2 1)
      (map #(clojure.string/join "" %))
      (group-by identity)
      (map (fn [[k v]] [k (count v)]))
      (into {})))

(def last-character (-> data first last str))

(def pair->insertion
  (->>
   (clojure.string/split (second data) #"\n")
   (map (fn [s] (clojure.string/split s #" -> ")))
   flatten
   (apply hash-map)))

(defn expand [m]
  (let [fx (fn [[[a c :as k] v]]
             (let [b (pair->insertion k)]
               {(clojure.string/join [a b]) v
                (clojure.string/join [b c]) v}))]
    (apply
     merge-with
     +
     (map fx m))))

(let [frequencies (->> start
                       (iterate expand)
                       (drop 40)
                       first
                       (map (fn [[k v]] {(-> k first str) v}))
                       (cons {last-character 1})
                       (apply merge-with +)
                       vals
                       sort)]
  (- (apply max frequencies)
     (apply min frequencies)))
