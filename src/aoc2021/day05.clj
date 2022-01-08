(ns aoc2021.day05
  (:require [clojure.string :as str]
            [aoc2021.util :as u]))

(defn inc-map-at-point
  "Input: map [x y]
  Output: map with item at [x y] inc'ed"
  [m [x y]]
  (u/log "### inc-map-at-point: " x y)
  (assoc m [x y] (inc (m [x y] 0))))

(defn auto-range
  "Similar to range but both start and end are included and it automatically uses
  step -1 if end less then start."
  [start end]
  (if (> end start)
    (range start (inc end))
    (range start (dec end) -1)))

(defn fill-map
  "Input: <boolean: include diagonals> ((x1 y1 x2 y2) (...) ...)
  Output: { [x y] count, ...}"
  [diag data]
  (reduce (fn [m [x1 y1 x2 y2]]
            (u/log "### next vector: " x1 y1 " to " x2 y2)
            (cond
              ; single point
              (and (= x1 x2) (= y1 y2))
              (inc-map-at-point m [x1 y1])
              ; horizontal
              (= y1 y2)
              (reduce #(inc-map-at-point %1 [%2 y1])
                      m
                      (auto-range x1 x2))
              ; vertical
              (= x1 x2)
              (reduce #(inc-map-at-point %1 [x1 %2])
                      m
                      (auto-range y1 y2))
              ; diagonal
              :else
              (if diag
                (reduce #(inc-map-at-point %1 %2)
                        m
                        (partition 2 (interleave
                                       (auto-range x1 x2)
                                       (auto-range y1 y2))))
                (do
                  (u/log "E: Diagonal vectors not supported yet:" x1 y1 "to" x2 y2)
                  m))))
          {}
          data))

(defn print-map
  "Input: map
  Output: map"
  [m]
  (println "ocean floor:")
  (let [y-max (->> m keys (map #(second %)) (apply max) inc)
        x-max (->> m keys (map #(first %)) (apply max) inc)]
    (doseq [y (range 0 y-max)]
      (doseq [x (range 0 x-max)]
        ;(printf " %2s" (m [x y] \.)))
        (print (m [x y] \.)))
      (println))
    m))

(defn count-dangerous-points
  "Input: <boolean: include diagonals> (\"n,n -> n,n\" \"...\")
  Output: Number of dangerous points (where 2 or more vectors overlap)"
  [include-diag data]
  (->> data
       (u/parse-numbers #"[-> ,]+")
       (fill-map include-diag)
       ;print-map
       (reduce (fn [counter [_ value]] (if (> value 1) (inc counter) counter))
               0)
       (assoc {} :dangerous_points)))

(defn solve-part-1
  "Calls (count-dangerous-points false data)"
  [data]
  (count-dangerous-points false data))


(defn solve-part-2
  "Calls (count-dangerous-points true data)"
  [data]
  (count-dangerous-points true data))
