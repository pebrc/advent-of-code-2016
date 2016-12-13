(ns advent-of-code-2016.day12
  (:require [clojure.java.io :as io]))

(defmacro spy [f]
  `(let [res# ~f]
     (println (apply str (repeat 10 "-" )))
     (println  '~f "->" res#)
     res#))


(def input (mapv #(re-seq #"-*\w+" %) (line-seq (io/reader (io/resource "day12.txt")))))

;(defn to-int [s] (Integer/parseInt s))

(defn eval-instr [s [cmd x y]]
  (case cmd
    "cpy" (-> (assoc s y (get s x (read-string x)))
               (update :sp inc))
    "inc" (-> (update s x inc)
               (update :sp inc))
    "dec" (-> (update s x dec)
               (update :sp inc))
    "jnz" (if (not= (get s x) 0)
            (update s :sp + (to-int y))
            (update s :sp inc))))

(defn day12-part1 [input]
  (loop [s {:sp 0 "a" 0 "b" 0 "d" 0}]
    (if-let [instr (get input (:sp s))]
      (recur (spy (eval-instr s (spy instr))))
      s)))


#_(day12-part1 input)
