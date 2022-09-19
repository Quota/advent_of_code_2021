(ns aoc2021.day13
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; misc
;;;

(defn read-dots 
  "Input: (\"x0,y0\" \"x1,y1\" .. \"xn,yn\")
  Outpu: #{[x0 y0] [x1 y1] .. [xn yn]}"
  [dot-lines]
  (->> dot-lines
       (map #(clojure.string/split % #","))
       (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]))
       (into #{})))

(defn read-foldings
  "Input: (\"fold along c0=n0\" \"fold along c1=n1\" ..)
  Output: [[:c0 n0] [:c1 n1] ..]"
  [folding-lines]
  (->> folding-lines
       (map #(.substring % 11))
       (map #(clojure.string/split % #"="))
       (map (fn[[xy n]] [(keyword xy) (Integer/parseInt n)]))))

(defn dots-size
  "Find width and height of given dot sequence.
  Input: #{[x y] [...] ...}
  Output: {:width w :heigh h}"
  [dots]
  {:width (->> dots (map first) (reduce max) inc)
   :height (->> dots (map second) (reduce max) inc)})

(defn read-data
 "Read input of day 12. Uses `read-dots`, `read-foldings` and `dots-size`.
 Input: (\"x0,y0\" \"x1,y1\" .. \"xn,yn\" \"fold along [xy]=n\" \"fold..\" ..)
 Output: {:dots #{[x0 y0] [x1 y1] .. [xn yn]} 
          :foldings ([:x n1] [:y n2] ..)
          :width w
          :height h}"
 [data]
 (let [[dot-lines folding-lines] (split-with (complement clojure.string/blank?) data)
       dots (read-dots dot-lines)
       foldings (read-foldings (drop 1 folding-lines))] ; drop 1 empty line
   (into {:dots dots
          :foldings foldings}
         (dots-size dots))))

(def ^:dynamic *dot-sym* "#")
(def ^:dynamic *space-sym* " ")

(defn plot-dots
  "Prints a graphical representation of the dots to stdout.
  Returns nil."
  [dots width height]
  (println "===" width "x" height "===")
  (doseq [y (range height)
          x (range width)]
    (print (if (dots [x y]) *dot-sym* *space-sym*))
    (when (= x (dec width)) (println))))

(defn fold-dots
  "Executes the given fold on the given dots.
  Returns the folded dots.
  Arguments:
  - dots: see `read-dots`.
  - [axis n]: axis is either `:x` or `:y`, n is an int.
  Example: #{[2 4]...} [:x 5]"
  [dots [axis n]]
  (persistent!
    (reduce (fn fold-dots-reducer [m [x y]] 
              (if (= axis :x)
                (conj! m [(- n (Math/abs (- x n))) y])
                (conj! m [x (- n (Math/abs (- y n)))])))
            (transient #{})
            dots)))

(defn apply-one-folding
  "Executes one fold according to the given data which is
  expected to be like the output of `read-data`.
  Returns the modified input, i.e. with the folded dots and 
  the first folding rule removed."
  [data]
  (let [curr-folding (first (data :foldings))
        next-foldings (next (data :foldings))
        next-dots (fold-dots (data :dots)  curr-folding)]
    (-> {:dots next-dots :foldings next-foldings}
        (into (dots-size next-dots))
        (assoc :dot-count (count next-dots)))))

;;;
;;; part 1
;;;

(defn solve-part-1
  "day 13 part 1"
  [input]
  (let [data (read-data input)]
    (apply-one-folding data)))

;;;
;;; part 2
;;;

(defn solve-part-2
  "day 13 part 2"
  [input]
  (loop [data (read-data input)]
    (let [data2 (apply-one-folding data)]
      (if (:foldings data2)
        (recur data2)
        (plot-dots (:dots data2) (:width data2) (:height data2))))))


;;;
;;; dev space
;;;
(comment
  (solve-part-1
    (clojure.string/split-lines (slurp "resources/day13_input_real.txt")))

  (solve-part-2
    (clojure.string/split-lines (slurp "resources/day13_input_real.txt")))

  (read-data
    (clojure.string/split-lines (slurp "resources/day13_input_sample.txt")))

  (with-bindings {#'*space-sym* \.}
    (let [data (read-data (clojure.string/split-lines (slurp "resources/day13_input_sample.txt")))]
      (plot-dots (data :dots)(data :width)(data :height))))

)
