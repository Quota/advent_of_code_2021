(ns aoc2021.day12
  (:require [clojure.string :as s])
  (:require [aoc2021.util :as u]))

;;;
;;; misc
;;;

(defn assoc-when-not
 "assoc args into m (like `assoc`) if `(not c)`"
 [m c & args]
 (if c
  m
  (apply assoc m args)))

(defn lower-case-kw?
  "Returns true if the first character of the given keyword is in lower-case.
   Otherwise and also for non-keywords it returns false."
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
  (map (fn [line] (clojure.string/split line #"-"))) ; "x-y" -> ["x" "y"]
  (map (fn [[x y]] [(keyword x) (keyword y)])) ; ["x" "y"] -> [:x :y]
  (reduce (fn [m [c1 c2]] ; [:x :y] -> {:x #{:y ...} }
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

(defn calc-paths
  "Input: neighbors visitor-fn
  Output: #{[:a :b ...] [...] ...}
  Notes on the visitor functions: It will be called with itself as the
  first argument and the current cave. Its result is expected to be another
  (or the same) visitor function, which then gets passed to subsequent calls
  of the `calc-paths` function (processing the neighbors).
  However if the returnd value is nil then processing stopps and this function
  returns nil as well.
  "
  ([neighbors visitor-fn]
   (calc-paths neighbors visitor-fn :start []))
  ([neighbors visitor-fn curr path]
   (when-let [next-visitor-fn (visitor-fn visitor-fn curr)]
     (if (= curr :end) #{path}
       (into #{}
                 (mapcat #(calc-paths neighbors
                                      next-visitor-fn
                                      %
                                      (conj path %)))
                 (neighbors curr))))))


;;;
;;; part 1
;;;

(defn make-single-visitor
  "Returns a visitor-function `vf` for `calc-paths`.
  This implementation allows small caves to be visited only once.
  .
  In other words it returns nil if the given cave is a small cave
  and has already been visited by that instance of the function.
  Otherwise it returns itself (for big caves) or a new version
  of the visitor-function with the given (small) caved stored
  as 'visited'.
  .
  In compliance with the visitor-function definition of calc-paths
  the returned function has the following properties:
  Its arguments are [self cave], thus it must be called like this:
  `(vf vf current-cave)` (i.e. passing itself as the first argument)."
  ([]
   (make-single-visitor #{}))
  ([initial-set]
   (let [visited initial-set]
     (fn visitor-fn [self cave] ; ugly hack for "self" reference
       (cond
         (visited cave) nil
         (lower-case-kw? cave) (make-single-visitor (conj visited cave))
         :else self)))))

(defn solve-part-1
  "day 12 part 1"
  [data]
  (let [neighbors (read-data data)
        res (calc-paths neighbors (make-single-visitor))]
    {:count (count res)
     :input neighbors
     :result res}))


;;;
;;; part 2
;;;

(defn make-one-twice-visitor
  "Returns a visitor-function `vf` for `calc-paths`.
  This implementation allows one single small cave to be visited twice
  and all other small caves to be visited only once.
  .
  In other words it returns nil if the given cave is a small cave
  and has already been visited by that instance of the function.
  Otherwise it returns itself (for big caves) or a new version
  of the visitor-function with the given (small) caved stored
  as 'visited'.
  .
  In compliance with the visitor-function definition of calc-paths
  the returned function has the following properties:
  Its arguments are [self cave], thus it must be called like this:
  `(vf vf current-cave)` (i.e. passing itself as the first argument)."
  ([twice-cave]
   (make-one-twice-visitor twice-cave #{} nil))
  ([twice-cave initial-set twice?]
   (let [visited initial-set]
     (fn visitor-fn [self cave] ; ugly hack for "self" reference
       (if (= twice-cave cave)
         (cond
           (and (visited cave) twice?) nil
           (lower-case-kw? cave) (make-one-twice-visitor twice-cave (conj visited cave) (visited cave))
           :else self)
         (cond
           (visited cave) nil
           (lower-case-kw? cave)
             (make-one-twice-visitor twice-cave (conj visited cave) twice?)
           :else self))))))

(defn get-small-caves
  "Returns the small caves (except :start and :end) from the neighbors map
  as a list.
  Input: {:a val1 :B val2 :start val3 :c ...}
  Output: (:a :c ...)"
  [neighbors]
  (->> (keys neighbors)
       (filter lower-case-kw?)
       (remove #{:start :end})))

(defn solve-part-2
  "day 12 part 2"
  [data]
  (let [neighbors (read-data data)
        small-caves (get-small-caves neighbors)
        res (reduce #(into %1 (calc-paths neighbors (make-one-twice-visitor %2))) #{} small-caves)]
    {:count (count res)
     :input neighbors
     :result res}))

;;;
;;; dev space
;;;
(comment
  (time
    (solve-part-1
      (.split (slurp "resources/day12_input_real.txt") "[\\n\\r]+")))

  (time
    (solve-part-2
      (.split (slurp "resources/day12_input_real.txt") "[\\n\\r]+")))

)
