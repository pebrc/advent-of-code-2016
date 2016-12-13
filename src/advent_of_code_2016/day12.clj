(ns advent-of-code-2016.day12
  (:require [clojure.java.io :as io]))

(defmacro spy [f]
  `(let [res# ~f]
     (println (apply str (repeat 10 "-" )))
     (println  '~f "->" res#)
     res#))


(set! *warn-on-reflection* true)

(def input (mapv #(re-seq #"-*\w+" %) (line-seq (io/reader (io/resource "day12.txt")))))


(defn parse-args [x]
  (if (>= (int (first x)) (int \a))
    (first x)
    (Integer/parseInt x)))

(defn parse-input [in]
  (mapv (fn [[cmd & r]] (into [cmd] (map parse-args r))) in))


(defn reg [^long c] (mod c 96))

(defn ^Long reg-or-val [^longs s x]
  (if (number? x) x (aget s (reg (int x)))))

(defn eval-instr [^longs s [cmd x y]]
  (case cmd
    "cpy" (do (aset s (reg (int y)) (reg-or-val s x) )
              (aset s 0 (inc (aget s 0))))
    "inc" (do (aset s (reg (int x)) (inc (aget s (reg (int x)))))
              (aset s 0  (inc (aget s 0))))
    "dec" (do (aset s (reg (int x)) (dec (aget s (reg (int x)))))
              (aset s 0 (inc (aget s 0))))
    "jnz" (if (not= (aget s (reg (int x))) 0)
            (aset s 0  (int (+ y (aget s 0))))
            (aset s 0 (inc (aget s 0))))))

(defn compute [input start-state]
  (let [s (long-array (into [0] (vals (sort start-state))))]
    (loop [instr (get input 0)]
      (if instr
        (do (eval-instr s instr)
            (recur (get input (aget s 0))))
        (vec s)))))


(defn day12-part1 []
  (nth (compute (parse-input input) {:a 0 :b 0 :c 0 :d 0}) 1))

#_(day12-part1) ;;1193ms

(defn day12-part2 []
  (nth (compute (parse-input input) {:a 0 :b 0 :c 1 :d 0}) 1))

#_(day12-part2) ;;38579ms


