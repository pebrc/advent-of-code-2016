(ns advent-of-code-2016.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (-> "day9.txt"
                io/resource
                slurp
                s/trim))


(defn to-int [s] (Integer/parseInt s))

(defn chars-to-int [chars]
  (to-int (apply str chars)))


(defn parse-marker [m]
  (let [[l r] (s/split m #"x")]
    (mapv chars-to-int [l r])))


(defn decompress-marker [[i x] s]
  (let [[h t] (split-at i s)]
    [(flatten (repeat x h)) t]))


(defn decompress [in]
  (letfn
      [(init [acc [_ & r :as input]]
         #(case _
            \( (decomp acc input)            
            nil acc
            (consume acc input)))
       (consume [acc input]
         (let [[uchars r] (split-with #(not= % \() input)
               uc (apply str uchars)
               new-acc (str acc uc)]
           (if (= (count uc) (count input))
             new-acc
             #(decomp new-acc r))))
       (decomp [acc input]
         (let [[rmk r] (split-with #(not= % \)) input)
               mk (parse-marker (rest rmk))
               [uc t] (decompress-marker mk (rest r))]
           #(init (str acc (apply str uc)) t)))]
    (trampoline init "" in)))

(defn day9-part1 [] (count  (decompress input)))


(defn starts-with-marker? [s] (= \( (first input)))

(defn has-marker? [s] (s/index-of s \())

(has-marker? "lsk(jf)")


(defn marker-expand-1 [s]
  (let [[rmk r] (s/split s #"\)" 2)
        [idx rpt] (parse-marker rmk)]
    [(apply str (repeat rpt (subs r 0 idx))) (subs r idx)]))

(declare memoized-part2)

(defn day9-part2 [in]
  (loop [acc 0 input in]
    (cond
      (empty? input) acc
      (starts-with-marker? input) (let [[exp r] (marker-expand-1 (subs input 1))]
                                    (if (has-marker? exp)
                                      (recur (+ acc (memoized-part2 exp)) r)
                                      (recur (+ acc (count exp)) r)))
      :else (recur (inc acc) (rest input)))))

(def memoized-part2 (memoize day9-part2))

(day9-part2 input)
