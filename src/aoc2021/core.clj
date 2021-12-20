(ns aoc2021.core
  (:gen-class)
  (:require [clojure.pprint :as pp])
  (:require [aoc2021.util :as u])
  (:require [aoc2021.day01 :as day01])
  (:require [aoc2021.day02 :as day02])
  (:require [aoc2021.day03 :as day03])
  (:require [aoc2021.day04 :as day04])
  (:require [aoc2021.day05 :as day05])
  (:require [aoc2021.day06 :as day06])
  (:require [aoc2021.day07 :as day07])
  )

(defn usage
  []
  (str "Usage: (-main <day> [data])\n"
       "       <day>   day (integer) of which to run the solution\n"
       "       [data]  input data for <day>. if missing read from stdin."))

(defn- skip-options
  [x]
  (and (string? x) (.startsWith x "-")))

(defn -main
  [& args]
  (let [day (str (first args))
        data (or (seq (drop-while skip-options (rest args)))
                 (line-seq (java.io.BufferedReader. *in*)))
        result (case day
                 "1.1" (day01/solve-part-1 (map #(Integer/parseInt %) data))
                 "1.2" (day01/solve-part-2 (map #(Integer/parseInt %) data))
                 "2.1" (day02/solve-part-1 data)
                 "2.2" (day02/solve-part-2 data)
                 "3.1" (day03/solve-part-1 data)
                 "3.2" (day03/solve-part-2 data)
                 "4.1" (day04/solve-part-1 data)
                 "4.2" (day04/solve-part-2 data)
                 "5.1" (day05/solve-part-1 data)
                 "5.2" (day05/solve-part-2 data)
                 "6.1" (day06/solve-part-1 data)
                 "6.2" (day06/solve-part-2 data)
                 "7.1" (day07/solve-part-1 data)
                 "7.2" (day07/solve-part-2 data)
                 nil)]
    (if (some? result)
      (pp/pprint result)
      (println (usage)))))
