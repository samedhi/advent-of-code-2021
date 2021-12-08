#!/usr/bin/env bb

(require '[clojure.test :as test :refer [deftest testing is are]])

(def depths (as-> (slurp "input01.txt") $
              (clojure.string/split $ #"\n")
              (map #(Integer/parseInt %) $)))

(defn increasing-value-count [xs]
  (->> (map vector xs (rest xs))
       (filter (fn [[a b]] (< a b)))
       count))

(deftest increasing-value-count-test
  (are [expected depths] (= expected (increasing-value-count depths))
    0       []
    0       [1]
    1       [1 2]
    1       [1 2 1]
    2       [1 2 3]))

(test/run-tests *ns*)

(increasing-value-count depths)
