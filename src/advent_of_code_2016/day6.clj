(ns advent-of-code-2016.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (-> "day6.txt"
      io/resource
      slurp
      s/trim
      s/split-lines))


(defn error-correct [cmp]
  (->> input
       (map seq)
       (apply map vector)
       (map #(->> %
                  frequencies
                  (sort-by val cmp)
                  first
                  first))
       (apply str)))

(defn day6-part1
  []
  (error-correct >))

(defn day6-part2
  []
  (error-correct <))
