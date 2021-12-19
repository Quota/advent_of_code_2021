(ns aoc2021.day06
  (:require [clojure.string :as str]
            [aoc2021.util :as u]))

(defn parse-input
  "Input: (\"n,n,...\") ; list with one string with numbers separated by comma
  Output: (n n ...) ; list of integers"
  [lines]
  (as-> lines $
    (first $)
    (clojure.string/split $ #"[, ]+")
    (map #(Integer/parseInt %) $)))

(defn calc-next-day
  "Input: (n[d] m[d] ...)
  Output: (n[d+1] m[d+1] ... x[d+1])"
  [fishes]
  (let [zero-count (count (filter #(= 0 %) fishes))
        fishes-next-day (map #(if (= 0 %) 6 (dec %)) fishes)]
    (concat fishes-next-day (repeat zero-count 8)))) ; 6 or 8

(defn loop-days
  [initial-fishes last-day]
  (loop [day 0
         fishes initial-fishes]
    (println day (count fishes) (map #(case % 7 \. 8 \. %) fishes))
    ;(println day (count fishes))
    (if (= day last-day)
      [day (count fishes)]
      (do
        (recur (inc day)
               (calc-next-day fishes))))))

(defn solve-part-1
  "80 days of lanternfish"
  [lines]
  (loop-days (parse-input lines)
             (let [n (second lines)]
               (if n (Integer/parseInt n) 80))))


(defn solve-part-2
  "256 days of lanternfish"
  [lines]
  "algo of part 1 does not terminate with days=256...")
