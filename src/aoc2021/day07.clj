(ns aoc2021.day07
  (:require [aoc2021.util :as u]))

;;;
;;; part 1
;;;

(defn calc-fuel
  ([input target]
   (calc-fuel input target identity)) ; costs equal distance
  ([input target cost-f]
   (->> input
        ; calc absolute distance:
        (map #(let [v (- % target)] (max v (- v))))
        ; calc costs for distance:
        (map cost-f)
        ; sum all costs up:
        (reduce +))))

(defn solve-part-1
  "crabs with constant fuel consumption, best-pos is median"
  [data]
  (let [input (first (u/parse-numbers #"," data))
        best-pos (as-> input $ ; calc median
                    (count $)
                    (/ $ 2)
                    (nth (sort input) $))]
    {:pos best-pos
     :fuel (calc-fuel input best-pos)}))

;;;
;;; part 2
;;;

(defn gausss
  "Gauss sum"
  [n]
  (/ (* n (inc n)) 2))

(defn solve-part-2
  "crabs but increasing fuel, best-pos is near the mean.
  (gauss says sum(1..n) = n * (n + 1) / 2), in our situation the
  mean value +/-0.5 is where the minimum lays.)
  (Why +/-0.5? See here: `https://www.reddit.com/r/adventofcode/comments/rawxad/2021_day_7_part_2_i_wrote_a_paper_on_todays/`)"
  [data]
  (let [input (first (u/parse-numbers #"," data))
        best-pos (/ (reduce + input) (count input))]
    (map #(hash-map :pos % :fuel (calc-fuel input % gausss))
         (list (int best-pos) (int (+ best-pos 0.5))))))
