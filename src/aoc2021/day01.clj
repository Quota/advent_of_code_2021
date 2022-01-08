(ns aoc2021.day01
  (:require [aoc2021.util :as u]))

(defn calc-depth-score
  ([data]
   (u/log "calc-depth-score[1]: data:" data)
   (calc-depth-score (first data) (rest data) 0))
  ([depth-curr depth-rest counter]
   (u/log "calc-depth-score[3]: depth-curr:" depth-curr "depth-rest:" depth-rest "counter:" counter)
   (if (empty? depth-rest)
     counter
     (let [depth-next (first depth-rest)]
       (recur depth-next
              (rest depth-rest)
              (if
                (> depth-next depth-curr)
                (inc counter)
                counter))))))

(defn solve-part-1
  "Input: List of depths (each one as string)"
  [data]
  (calc-depth-score (map #(Integer/parseInt %) data)))

(defn solve-part-2
  "Input: List of deps (but numbers as strings)"
  [data]
  (let [depth-list (map #(Integer/parseInt %) data)]
    (calc-depth-score
      (map
        (fn[x y z]
          (u/log "solve-part-2: map fn gets: " x y z)
          (+ x y z))
        depth-list (rest depth-list) (rest (rest depth-list))))))
