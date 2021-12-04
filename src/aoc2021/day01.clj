(ns aoc2021.day01
  (:require [aoc2021.util :as u]))

(defn solve-part-1
  ([depth-list] 
   (u/log "solve-part-1[1]: depth-list:" depth-list)
   (solve-part-1 (first depth-list) (rest depth-list) 0))
  ([depth-curr depth-rest counter]
   (u/log "solve-part-1[3]: depth-curr:" depth-curr "depth-rest:" depth-rest "counter:" counter)
   (if (empty? depth-rest)
     counter
     (let [depth-next (first depth-rest)]
       (recur depth-next
              (rest depth-rest)
              (if
                (> depth-next depth-curr)
                (inc counter)
                counter))))))

(defn solve-part-2
  [depth-list]
  (solve-part-1
    (map
      (fn[x y z]
        (u/log "solve-part-2: map fn gets: " x y z)
        (+ x y z))
      depth-list (rest depth-list) (rest (rest depth-list)))))
