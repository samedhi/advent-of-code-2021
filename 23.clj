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

(defn paths
  ([]
   (paths {:path ["start"] :seen #{"start"}}))
  ([{:keys [path seen]}]
   (let [current-edge (peek path)]
     (if (= "end" current-edge)
       [path]
       (apply
        concat
        (for [outbound (get edges current-edge)
              :let [big? (= (clojure.string/upper-case outbound) outbound)]
              :when (not (contains? seen outbound))]
          (paths {:path (conj path outbound)
                  :seen (into seen (when-not big? [outbound]))})))))))

(count (paths))
