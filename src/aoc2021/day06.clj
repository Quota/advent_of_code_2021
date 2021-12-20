(ns aoc2021.day06
  (:require [clojure.string :as str]
            [aoc2021.util :as u]))

;;;
;;; naive / slow
;;; by keeping the input sequence and literally growing the string
;;;

(defn parse-input
  "Input: (\"n,n,...\") ; list with one string with numbers separated by comma
  Output: (n n ...) ; list of integers"
  [lines]
  (as-> lines $
    (first $)
    (clojure.string/split $ #"[, ]+")
    (map #(Integer/parseInt %) $)))

(defn calc-next-day-naive
  "Input: (n[d] m[d] ...)
  Output: (n[d+1] m[d+1] ... x[d+1])"
  [fishes]
  (let [zero-count (count (filter #(= 0 %) fishes))
        fishes-next-day (map #(if (= 0 %) 6 (dec %)) fishes)]
    (concat fishes-next-day (repeat zero-count 8)))) ; 6 or 8

(defn loop-days-naive
  [initial-fishes last-day]
  (loop [day 0
         fishes initial-fishes]
    (println day (count fishes) (map #(case % 7 \. 8 \. %) fishes))
    ;(println day (count fishes))
    (if (= day last-day)
      [day (count fishes)]
      (do
        (recur (inc day)
               (calc-next-day-naive fishes))))))

;;;
;;; based on counters
;;; by keeping track of how many fishes of a specific age exists
;;;

;          0 1 2 3 4 5 6 7 8
; fishes:  0 0 0 0 0 0 1 0 0 "6"
; day 1:   0 0 0 0 0 1 0 0 0 "5"
;     5:   0 1 0 0 0 0 0 0 0 "1"
;     6:   1 0 0 0 0 0 0 0 0 "0"
;     7:   0 0 0 0 0 0 1 0 1 "6,8"
;     8:   0 0 0 0 0 1 0 1 0 "5,7"
;     9:   0 0 0 0 1 0 1 0 0 "4,6"
(defn calc-next-day-vec
  [fishes]
  (let [z (first fishes)]
    (-> fishes
        (subvec 1)
        (update 6 + z)
        (conj z))))

(defn loop-days-vec
  [initial-fishes last-day]
  (loop [day 0
         fishes (reduce (fn [f n] (update f n inc))
                        (vec (repeat 9 0))
                        initial-fishes)]
    (if (= day last-day)
      [day fishes (apply + fishes)]
      (do
        (println day fishes (apply + fishes))
        (recur (inc day)
               (calc-next-day-vec fishes))))))

;;;
;;; part 1
;;;

(defn solve-part-1
  "80 days of lanternfish"
  [lines]
  ;(loop-days-naive
  (loop-days-vec
    (parse-input lines)
    ; second line can be the number of days
    (let [n (second lines)]
      (if n (Integer/parseInt n) 80))))

;;;
;;; part 2
;;;

(defn solve-part-2
  "256 days of lanternfish"
  [lines]
  (loop-days-vec
    (parse-input lines)
    ; second line can be the number of days
    (let [n (second lines)]
      (if n (Integer/parseInt n) 256))))

