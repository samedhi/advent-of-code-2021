#!/usr/bin/env bb

(def data (clojure.string/split (slurp "input14.txt") #"\n\n"))

(def start (first data))

(def pair->insertion
  (->>
   (clojure.string/split (second data) #"\n")
   (map (fn [s] (clojure.string/split s #" -> ")))
   flatten
   (apply hash-map)))

(defn expand [s]
  (->> (partition 2 1 nil s)
       (map (fn [tpl] (clojure.string/join "" tpl)))
       (map (fn [[a c :as s]] [a (get pair->insertion s)]))
       flatten
       (remove nil?)
       (clojure.string/join "")))

(expand start)

(let [frequencies (->> (iterate expand start)
                       (drop 5)
                       first
                       (group-by identity)
                       (map (fn [[k v]] [k (count v)]))
                       (map second)
                       sort)]
  (- (apply max frequencies) (apply min frequencies)))
