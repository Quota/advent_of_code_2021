(ns aoc2021.day02
  (:require [aoc2021.util :as u]))

(defn string-to-cmd
  [str]
  (let [name-and-number (clojure.string/split str #" ")]
    {:cmd-name (get name-and-number 0)
     :cmd-value (Integer/parseInt (get name-and-number 1))}))

(defn solve-part-1
  "forward down up"
  ([cmd-strings]
   (solve-part-1 cmd-strings 0 0))
  ([cmd-strings horiz-pos depth] 
   (if (empty? cmd-strings)
     {:horizontal-pos horiz-pos :depth depth :product (* horiz-pos depth)}
     (let [cmd (string-to-cmd (first cmd-strings))]
       (case (:cmd-name cmd)
         "forward" (recur (rest cmd-strings) (+ horiz-pos (:cmd-value cmd)) depth)
         "down"    (recur (rest cmd-strings) horiz-pos (+ depth (:cmd-value cmd)))
         "up"      (recur (rest cmd-strings) horiz-pos (- depth (:cmd-value cmd))))))))

(defn solve-part-2
  "forward down up but with aim"
  ([cmd-strings]
   (solve-part-2 cmd-strings 0 0 0))
  ([cmd-strings horiz-pos depth aim] 
   (if (empty? cmd-strings)
     {:horizontal-pos horiz-pos :depth depth :aim aim :product (* horiz-pos depth)}
     (let [cmd (string-to-cmd (first cmd-strings))]
       (case (:cmd-name cmd)
         "forward" (recur (rest cmd-strings) (+ horiz-pos (:cmd-value cmd)) 
                                             (+ depth (* aim (:cmd-value cmd)))
                                             aim)
         "down"    (recur (rest cmd-strings) horiz-pos depth (+ aim (:cmd-value cmd)))
         "up"      (recur (rest cmd-strings) horiz-pos depth (- aim (:cmd-value cmd))))))))
