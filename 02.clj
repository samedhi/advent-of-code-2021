#!/usr/bin/env bb

(require '[clojure.test :as test :refer [deftest testing is are]])

(def depths (as-> (slurp "input01.txt") $
              (clojure.string/split $ #"\n")
              (map #(Integer/parseInt %) $)))

(defn partition-3 [xs]
  (partition 3 1 xs))

(defn sum-x-in-xs [xss]
  (map (fn [xs] (apply + xs)) xss))

(defn sliding-window-count [xs]
  (->> (map vector xs (rest xs))
       (filter (fn [[a b]] (< a b)))
       count))

(deftest tests
  (is (= [[199 200 208] [200 208 242]] (partition-3 [199 200 208 242])))
  (is (= [607 650] (sum-x-in-xs (partition-3 [199 200 208 242])))))

(test/run-tests *ns*)

(-> depths
    partition-3
    sum-x-in-xs
    sliding-window-count)
