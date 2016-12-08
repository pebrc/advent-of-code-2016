(ns advent-of-code-2016.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn display [cols rows] (vec  (repeat rows (vec (repeat cols :off)))))


(defn rotatev [v n]
  (let [l (count v)]
    (vec (take l (drop (- l n) (cycle v))))))


(defn transposev [vs]
  (vec (apply map vector vs)))

(defn rotate-row [y offset d]
  (update d  y rotatev offset ))

(defn rotate-col [x offset d]
  (transposev (rotate-row  x offset (transposev d))))

(defn rect [x y d]
  (->> (for [x (range x) y (range y)] [y x])
       (reduce (fn [d coords] (update-in d coords (constantly :on))) d)))


(def rect-r #"rect (\d+)x(\d+)")
(def rotate-row-r #"rotate row y=(\d+) by (\d+)")
(def rotate-col-r #"rotate column x=(\d+) by (\d+)")


(defn parse-commands [input]
  (map 
   #(condp re-find %
      rect-r :>> (fn [[_ x y]] (partial rect (read-string x) (read-string y)))
      rotate-row-r :>> (fn [[_ y os]] (partial rotate-row (read-string y) (read-string os)))
      rotate-col-r :>> (fn [[_ x os]] (partial rotate-col (read-string x) (read-string os))))
   input))

(def input
  (-> (-> "day8.txt"
      io/resource
      slurp
      s/trim
      s/split-lines)))

(defn swipe []
  (reduce (fn [grid cmd] (cmd grid)) (display 50 6) (parse-commands input)))

(defn day8-part1 []
  (->> (swipe)
       flatten
       (filter #(= % :on))
       count))

(defn print [display]
  (letfn [(print-row [r]
            (apply str (map #(case %
                               :on "#"
                               :off " ") r)))]
    (->> display
         (map print-row)
         (s/join "\n"))))

(defn day8-part2 []
  (println (->> (swipe)
                print)))







