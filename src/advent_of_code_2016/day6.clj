(ns advent-of-code-2016.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import (java.io BufferedReader StringReader))))

(def input
  (-> "day6.txt"
      io/resource
      slurp
      s/trim
      s/split-lines))


(defn day6-part1
  []
  (->> input
       (map seq)
       (apply map vector)
       (map #(->> %
                  frequencies
                  (sort-by val >)
                  first
                  first))
       (apply str)))
