(ns advent-of-code-2016.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import (java.io BufferedReader StringReader)))

(def input
  (-> "day3.txt"
      io/resource
      slurp
      s/trim
      s/split-lines))

(defn parse-input [s]
  (map #(->> (re-seq #"[0-9]+" %)
             (map read-string)) s))

(defn triangle?
  [[x y z]] (and  (> (+ x y) z)
                  (> (+ y z) x)
                  (> (+ z x) y)))

(defn day3-part1 [s]
  (->> (parse-input s)
       (filter triangle?)
       (count)))


(defn day3-part2 [s]
  (->> (parse-input s)
       (reduce (fn [[c1 c2 c3] [x y z]]
                 [(conj c1 x) (conj c2 y) (conj c3 z)]) [[] [] []] )
       (reduce concat)
       (partition 3)
       (filter triangle?)
       (count)))
