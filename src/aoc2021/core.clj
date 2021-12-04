(ns aoc2021.core
  (:gen-class)
  (:require [aoc2021.util :as u])
  (:require [aoc2021.day01 :as day01])
  (:require [aoc2021.day02 :as day02])
  )

(defn usage
  []
  (str "(-main <day> [data])\n"
       "<day>   day (integer) of which to run the solution\n"
       "[data]  input data for <day>. if missing read from stdin."))

(defn- skip-options
  [x]
  (and (string? x) (.startsWith x "-")))

(defn -main
  [& args]
  (u/log "main args:" args)
  (u/log "main first args:" (first args))
  (u/log "main type first args:" (type (first args)))
  (println 
    (let [day (str (first args))
          data (or (seq (drop-while skip-options (rest args)))
                   (line-seq (java.io.BufferedReader. *in*)))]
      (u/log "main data:" data)
      (case day
        "1.1" (day01/solve-part-1 (map #(Integer/parseInt %) data))
        "1.2" (day01/solve-part-2 (map #(Integer/parseInt %) data))
        "2.1" (day02/solve-part-1 data)
        "2.2" (day02/solve-part-2 data)
        (usage)))))
