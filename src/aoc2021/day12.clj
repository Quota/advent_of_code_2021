(ns aoc2021.day12
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; misc
;;;

;;; functions regarding our data

(defn assoc-when-not 
 "assoc args into m (like `assoc`) if `(not c)`"
 [m c & args]
 (if c
  m
  (apply assoc m args)))

(defn lower-case-kw?
  [keyw]
  (and (keyword? keyw)
  (Character/isLowerCase (first (name keyw)))))

(defn read-data
 "Reads seq with cave connections, returns map cave -> neighbor-caves.
  Note the special treatment for :start and :end
  - :start will not appear in neighbor lists 
  - :end will not appear as key in the map
  Input: (\"start-a\" \"start-b\" \"a-b\" \"a-c\" ...)
  Output: {:start #{:a :b} :a #{:b :c ...} ...}"
 [data]
 (->> data
  ;; "start-x" -> ["start" "x"]
  (map #(clojure.string/split % #"-")) 
  ;; ["x" "y"] -> [:x :y]
  (map (fn [[x y]] [(keyword x) (keyword y)]))
  ;; [:x :y] -> {:x #{:y ...} }
  (reduce (fn [m [c1 c2]]
           (-> m
            ; don't let :start get into the neighbors lists
            (assoc-when-not (#{:start} c2) c1 (conj (m c1 #{}) c2))
            (assoc-when-not (#{:start} c1) c2 (conj (m c2 #{}) c1))))
   {})
  ;; remove :end and its neighbors
  ((fn[m] (dissoc m :end)))))

;
; {:start #{:A :b}, :A #{:c :b :end}, :c #{:A}, :b #{:A :d :end}, :d #{:b}}
;

(defn find-end
 "Input: neighbors
 Output: ([:a :b ...] [...] ...)"
 ([neighbors]
  (find-end neighbors #{} :end :start (transient []) '()))
 ([neighbors visited end curr path result]
   (if (= curr end)
     (persistent! path)
     (reduce (fn [paths nxt] (concat paths (find-end
                                            neighbors 
                                            (if (lower-case-kw? nxt) (conj visited nxt) visited)
                                            end
                                            nxt
                                            (conj! path nxt) nil) 
             


                   ) #{} (neighbors curr))))




(comment
  (find-end nb)
)

;;;
;;; part 1
;;;

(defn solve-part-1
  "day 12 part 1"
  [data]
  (let [neighbors (read-data data)]
    (loop [left :start]
      (if (= left :end))
        (ende)
        (for [right (neighbors left)]
          (recur right))
        


;;;
;;; part 2
;;;

(defn solve-part-2
  "day 12 part 2"
  [data]
  nil)


;;;
;;; dev space
;;;
(comment
  (solve-part-1
    (.split (slurp "resources/day12_input_sample.txt") "\\n"))

  (solve-part-2
    (.split (slurp "resources/day12_input_sample.txt") "\\n"))


  (read-data 
    (.split (slurp "resources/day12_input_sample.txt") "\\n"))

  (def nb {:start #{:A :b}, :A #{:c :b :end}, :c #{:A}, :b #{:A :d :end}, :d #{:b}})

  (defn f1 [c v z]
   (if (or (= c :end) (= z 0))
    v
    (for [nxt (nb c)]
     (f1 nxt (conj v nxt) (dec z))))

  (f1 :start [] 1)

  (for [nxt (nb :start)]
    nxt)
)
