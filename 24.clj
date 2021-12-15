#!/usr/bin/env bb

(def data (slurp "input12.txt"))

(def edge-tuples
  (as-> data $
    (clojure.string/split $ #"\n")
    (map #(clojure.string/split % #"-") $)
    (vec $)))

(def edges
  (reduce
   (fn [acc [v1 v2]]
     (-> acc
         (update v1 conj v2)
         (update v2 conj v1)))
   (zipmap (-> edge-tuples flatten set) (repeat []))
   edge-tuples))

(def startless-edges
  (reduce-kv
   (fn [m k vs] (assoc m k (filterv #(not= % "start") vs)))
   {}
   edges))

(defn paths
  ([]
   (paths {:path ["start"] :seen #{"start"} :repeated? false}))
  ([{:keys [path seen repeated?]}]
   (let [current-edge (peek path)]
     (if (= "end" current-edge)
       [path]
       (apply
        concat
        (for [outbound (get startless-edges current-edge)
              :let [big? (= (clojure.string/upper-case outbound) outbound)
                    seen? (contains? seen outbound)
                    next-path (conj path outbound)
                    next-seen (into seen (when-not big? [outbound]))
                    next-repeated? (or repeated? seen?)]
              :when (if-not seen? true (when-not repeated? true))]
          (paths {:path next-path :seen next-seen :repeated? next-repeated?})))))))

(count (paths))
