(ns aoc2021.day09
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; part 1
;;;

(defn find-low-points-h
  [x y v1 v2 v3 lpm]
  (if (< x (count v2))
    (let [value (get v2 x)]
      (recur (inc x) y
             v1 v2 v3
             (if (and (< value (get v2 (dec x) 10)) ; left
                      (< value (get v2 (inc x) 10)) ; right
                      (< value (get v1 x 10)) ; above
                      (< value (get v3 x 10)))
               (assoc lpm [x y] value)
               lpm)))
    lpm))

(defn find-low-points
  "Input: ( \"nnn..\" \"..\") ; list of lines of strings
  Output: {[x y] v, ..} ; map of coordinate to value"
  ([lines]
   (find-low-points 0 ; y
                    nil ; v1
                    (vec (u/parse-digits (first lines))) ; v2
                    (vec (u/parse-digits (second lines))) ; v3
                    (rest (rest lines)) ; vs
                    nil)) ; low-point-map
  ([y v1 v2 v3 vs low-point-map] ; low-point-map: {[x y] v, ..}
   (u/log "y:" y)
   (u/log "v1:" v1)
   (u/log "v2:" v2)
   (u/log "v3:" v3)
   (let [new-lpm (find-low-points-h 0 y v1 v2 v3 low-point-map)]
     (if (seq v3)
       (recur (inc y) ; y
              v2 v3 (vec (u/parse-digits (first vs))) ; v1-3
              (rest vs) ; vs
              new-lpm) ; low-point-map
       new-lpm))))

(defn solve-part-1
  "calc risks of low points"
  [data]
  (let [low-point-values (vals (find-low-points data))]
    {:low-points-found (count low-point-values)
     :calculated-risk (+ (count low-point-values)
                         (apply + low-point-values))}))

;;;
;;; part 2
;;;

(defn lines-to-2d-ints
  "Input: (\"nnn..\" \"..\") ;  list of strings with digits
  Output: [[n n n ..][..]] ; vector of vectors with integers"
  [data]
  (vec (map (fn[l] (vec (u/parse-digits l))) data)))

(defn get-value
  "Input: [[n ..][..]] x y ; 2d vector and two indexes
  Output: n ; value at x y in the 2d vector, or 9"
  [data x y]
  (get (get data y) x 9))

(defn research-basin
  "Input: [[n ..]..] [x y] ; height map and one low point
  Output: ([x y]..) ; all points of the basin around (incl. low point)"
  ([hm [x y]]
   (research-basin #{} hm [x y]))
  ([res hm [x y]]
   ; skip if already checked/added or if 9s (or outside the height map)
   (if (or (get res [x y])
           (= (get-value hm x y) 9))
     res
     ; otherwise add [x y] and check surrounding points
     (-> res
         (conj [x y])
         (research-basin hm [(dec x) y])
         (research-basin hm [(inc x) y])
         (research-basin hm [x (dec y)])
         (research-basin hm [x (inc y)])))))

(defn collect-basins
  "Input: [[n ..]..] {[x y] v, ..} ; height map and low points
  Output: {[x y] ([x y] ..), ..} ; basins"
  [hm lp]
  (map (fn[[k _]] [k (research-basin hm k)]) lp))

(defn solve-part-2
  "find largest basins"
  [data]
  (let [lp (find-low-points data)
        hm (lines-to-2d-ints data)
        bs (collect-basins hm lp)]
    {:basins bs
     :result (->> bs
                  (map (comp count second))
                  (sort-by -)
                  (take 3)
                  (apply *))}))
