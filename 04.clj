#!/usr/bin/env bb

(defn traverse [{:keys [aim] :as m} [op v]]
  (condp = op
    :up (update m :aim - v)
    :down (update m :aim + v)
    :forward (-> m
                 (update :horizontal + v)
                 (update :depth + (* aim v)))))

(as-> (slurp "input02.txt") $
  (clojure.string/split $ #"\n")
  (map #(clojure.string/split % #" ") $)
  (map (fn [[op v]] [(keyword op) (Integer/parseInt v)]) $)
  (reduce traverse {:aim 0 :depth 0 :horizontal 0} $)
  (select-keys $ [:depth :horizontal])
  (vals $)
  (apply * $))
