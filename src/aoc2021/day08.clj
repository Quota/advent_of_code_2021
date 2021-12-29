(ns aoc2021.day08
  (:require [clojure.string :as s])
  (:require [clojure.set :refer [map-invert]])
  (:require [aoc2021.util :as u]))

;;;
;;; part 1
;;;

(defn solve-part-1
  "simple segments"
  [data]
  '("Execute in bash:"
   "cat resources/day08_input_real.txt | sed -e 's/.* | //' | sed -e 's/ /\n/g' | grep -v '^......\\?$' | wc -l"
   "See also: src/aoc2021/day08.sh"))

;;;
;;; part 2
;;;

(comment "deduction of segments:

         only segment b appears 6x in 0123456789
         only segment e appears 4x in 0123456789
         only segment f appears 9x in 0123456789

         remove segments b, e, f from 1478:

         only segment c appears 4x in 1478\\bef
         only segment g appears 1x in 1478\\bef

         remove segments c, g from 0123456789:

         only segment d appears 7x in 0123456789\\cg
         only segment a appears 8x in 0123456789\\cg

         see also: day08.txt")

(defn count-segments
  "Input: (\"ab\" \"bc\" ...)
  Output: {\\a 1, \\b 2, \\c 1 ...}"
  [segment-patterns]
  (frequencies (s/join segment-patterns)))

(defn map-val-drop-nil
  "Maps the values of the given map but drops keys for nil values.
  Example: {:a 1 :b 5 :c 10 :d 15} #(if (< % 10) (* % 10) nil)
  yields {:a 10, :b 50}"
  [in-map value-map-fn]
  (reduce (fn[m [k v]]
            (let [nv (value-map-fn v)]
              (if nv
                (assoc m k nv)
                m)))
          {}
          in-map))

(defn demystify-segments
  "Analyzes non-standard (mystified) segment patterns and returns a mapping
  describing the relation to standard segments.
  Input: unique signal patterns sequence
  Output: segment relation map (mystery seg char -> standard seg keyword)
  Example: (\"acedgfb\" \"cdfbe\" \"gcdfa\" \"fbcad\" \"dab\"
            \"cefabd\" \"cdfgeb\" \"eafb\" \"cagedb\" \"ab\")
  yields: {\\e :b, \\g :e, \\b :f, \\a :c, \\c :g, \\d :a, \\f :d}"
  [sig-pats]
  (let [frq-all (count-segments sig-pats)
        frq-1478 (count-segments (remove #(#{5 6} (.length %)) sig-pats))
        step-1 (map-val-drop-nil
                 frq-all
                 #(case % 6 :b 4 :e 9 :f nil))
        step-2 (map-val-drop-nil
                 (apply dissoc frq-1478 (map (map-invert step-1) '(:b :e :f)))
                 #(case % 4 :c 1 :g nil))
        step-3 (map-val-drop-nil
                 (apply dissoc frq-all (map (map-invert step-2) '(:c :g)))
                 #(case % 7 :d 8 :a nil))]
    (merge step-1 step-2 step-3)))

(defn transcode-segments
  "Convert a mystery segment seq into a standard segment set (using the
  segment relation map as produced by `demystify-segments`).
  Input: {\\b :a \\c :f ...} (\\b \\x  ...)
  Output: #{:a :y ...}"
  [seg-rel-map seg]
  (set (map #(get seg-rel-map %) seg)))

; map of segments -> digits
(def segment-digits {#{:a :b :c :e :f :g} 0
                     #{:c :f} 1
                     #{:a :c :d :e :g} 2
                     #{:a :c :d :f :g} 3
                     #{:b :c :d :f} 4
                     #{:a :b :d :f :g} 5
                     #{:a :b :d :e :f :g} 6
                     #{:a :c :f} 7
                     #{:a :b :c :d :e :f :g} 8
                     #{:a :b :c :d :f :g} 9})

(defn digits-to-num
  "Input: (i j k ...) ; single digits
  Output: ijk... ; as number
  Example: (1 2 5) yields 125"
  [digits]
  (reduce #(+ (* 10 %1) %2) 0 digits))

(defn decode-line
  "Input: \"nnn... ... | nnn... ...\"
  Output: seq of four numbers the segments after the pipe mean."
  [^String line]
  (let [[uniq-seg-pat mystery-vals] (s/split line #" *\| *")
        seg-rel-map (demystify-segments (s/split uniq-seg-pat #" +"))]
    (->> (s/split mystery-vals #" +") ; split string at spaces
         (map seq) ; convert string -> seq of chars
         (map (partial transcode-segments seg-rel-map)) ; chars -> real seg's
         (map segment-digits) ; segment sets -> digits
         (digits-to-num))))

(defn solve-part-2
  "all segments"
  [data]
  (->> data
       (map decode-line)
       (apply +)))
