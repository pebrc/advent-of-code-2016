(ns advent-of-code-2016.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(def init-regex #"value (\d+) goes to (\w+ \d+)")
(def rule-regex #"(\w+ \d+) gives low to (\w+ \d+) and high to (\w+ \d+)")


(def input (line-seq (io/reader (io/resource "day10.txt"))))

(defn to-int [s] (Integer/parseInt s))
(defn to-kw [name] (keyword (s/replace name " " "")))

(defn update-state [state bot v]
  (update state bot (fn [vs] (conj (if vs vs []) v))))

(defn parse-input [i]
  (let [[rules state] (partition-by #(s/starts-with? % "value" ) (sort i))]
    [(reduce (fn [acc [s v b]]
               (update-state acc (to-kw b) (to-int v)))
             {}
             (map #(re-find init-regex %) state))
     (reduce (fn [acc [_ b low high]]
               (assoc acc (to-kw b) (mapv to-kw [low high])))
             {}
             (map #(re-find rule-regex %) rules))]))


(defn apply-rules [rules state [bot values]]
  (let [[low-t high-t] (bot rules)]
    (-> state
        (update-state low-t (apply min values))
        (update-state high-t (apply max values)))))

(declare rules)

(defn simulate [state res-extractor]
  (let [{waiting 1 next 2} (group-by #(count (second %)) state)
        res (res-extractor next)]
    (cond
      res res
      (empty? next) state
      :else (recur (reduce (partial apply-rules rules) (into {} waiting)  next) res-extractor))))

(defn day10-part1 [input]
  (let [[state rules] (parse-input input)]
    (simulate state #(some (fn [[b v]] (if (every? #{61 17} v) b nil)) %))))

#_(day10-part1 input)

(defn day10-part2 [input]
  (let [[state rules] (parse-input input)
        final-state (simulate state (constantly false))]
    (->> (select-keys final-state [:output0 :output1 :output2])
         vals
         flatten
         (reduce *))))


#_(day10-part2 input)
