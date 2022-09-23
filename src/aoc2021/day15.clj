(ns aoc2021.day15
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u])
  (:require [clojure.data.priority-map :refer [priority-map]]))

;;;
;;; misc
;;;

(defn read-data
  "Reads ..."
  [data]
  (let [width (count (first data))
        height (count data)
        edges (->> data ; list [i=x+width*y]->edge_costs_to_xy /*from any neighbor*/
                   (apply concat)
                   (map #(- (int %) 48))
                   (into []))]
    {:edges (fn get-edge [[x y]] (edges (+ x (* y width))))
     :values {[0 0] 0} ; map [x,y]->total_costs_to_xy /*from the start node*/
     :parents {[0 0] [0 0]} ; map [x,y]->[x_p,y_p]
     :queue (priority-map [0 0] 0) ; map [x,y]->total_costs_to_xy
     :ewidth width
     :eheight height}))

(defn plot-cave
  [data]
  (let [x-max (dec (:ewidth data))]
    (doseq [y (range (:eheight data))
            x (range (inc x-max))]
      (print ((:edges data) [x y]))
      (if (= x x-max) (println)))))

(defn dijkstra-reducer
  [curr data nbr]  ; nbr: neighbor (of curr) to check
  (let [new-value (+ ((:values data) curr)
                     ((:edges data) nbr))]
    ; if in queue but new-value is better (lower),
    ; or not visited yet (no parent)?
    (if (or (and (contains? (:queue data) nbr)
                 (< new-value ((:values data) nbr)))
            (not (contains? (:parents data) nbr)))
      ; then enqueue (or update queue priority) and
      ; update value and parent
      (-> data
          (update :values assoc nbr new-value)
          (update :parents assoc nbr curr)
          (update :queue assoc nbr new-value))
      ; else no changes
      data)))

(defn get-neighbors
  "Returns above/left/below/right neighbors."
  [[x y] w h]
  (if (or (< x 0) (< y 0) (>= x w) (>= y h))
    '() ; out of bounds
    (remove nil? (list (if (> y 0) [x (dec y)]) ; above
                       (if (> x 0) [(dec x) y]) ; left
                       (if (< y (dec h)) [x (inc y)]) ; below
                       (if (< x (dec w)) [(inc x) y]))))) ; right

(defn run-dijkstra
  [data]
  (let [curr (first (peek (:queue data)))
        data-next (reduce (partial dijkstra-reducer curr)
                          (assoc data :queue (pop (:queue data)))
                          (get-neighbors curr (:ewidth data) (:eheight data)))]
    (if (empty? (:queue data-next))
      data-next
      (recur data-next))))

;;;
;;; part 1
;;;

(defn solve-part-1
  "day 15 part 1"
  [input]
  (-> input
       read-data 
       ;run-dijkstra
       (#(time (run-dijkstra %)))
       (#((:values %) [(dec (:ewidth %)) (dec (:eheight %))]))))

;;;
;;; part 2
;;;

(defn resized-edge
  [edges-x1 width-x1 height-x1 [x y]]
  ; we need numbers 1-9 but mod 10 gives us 0-9. so before mod we dec
  ; then do mod 9 (instead of 10) and then inc again
  (-> (+ (edges-x1 [(mod x width-x1) (mod y height-x1)])
         (int (/ x width-x1))
         (int (/ y height-x1)))
      dec
      (mod 9)
      inc))

(defn resize-cave
  [data fac]
  (let [edges-x1 (:edges data)
        width-x1 (:ewidth data)
        height-x1 (:eheight data)]
    (-> data 
        (assoc :edges (partial resized-edge edges-x1 width-x1 height-x1))
        (assoc :ewidth (* width-x1 fac))
        (assoc :eheight (* height-x1 fac)))))

(defn solve-part-2
  "day 15 part 2"
  [input]
    (-> input
        read-data
        (resize-cave 5)
        ;run-dijkstra
        (#(time (run-dijkstra %)))
        (#((:values %) [(dec (:ewidth %)) (dec (:eheight %))]))))
                                

;;;
;;; dev space
;;;

(comment
  (solve-part-1
    (clojure.string/split-lines (slurp "resources/day15_input_real.txt")))

  (solve-part-2
    (clojure.string/split-lines (slurp "resources/day15_input_real.txt")))

  (plot-cave
    (resize-cave
      (read-data (clojure.string/split-lines (slurp "resources/day15_input_sample.txt")))
      3))

)
