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
        edges (->> data ; list [i=x+width*y]->edge_costs_to_xy /*from all neighbors*/
                   (apply concat)
                   (map #(- (int %) 48))
                   (into []))]
    {:edges (fn get-edge [[x y]] (edges (+ x (* y width))))
     :values {[0 0] 0} ; map [x,y]->total_costs_to_xy /*from the start node*/
     :parents {[0 0] [0 0]} ; map [x,y]->direction /*one of: :up, :left, :down, :right*/
     :queue (priority-map [0 0] 0) ; map [x,y]->total_costs_to_xy
     :ewidth width
     :eheight height}))

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
       run-dijkstra
       (#((:values %) [(dec (:ewidth %)) (dec (:eheight %))]))))

;;;
;;; part 2
;;;

(defn solve-part-2
  "day 15 part 2"
  [input]
  "todo")

;;;
;;; dev space
;;;

(comment
  (solve-part-1
    (clojure.string/split-lines (slurp "resources/day15_input_real.txt")))

  (solve-part-2
    (clojure.string/split-lines (slurp "resources/day15_input_sample.txt")))

)
