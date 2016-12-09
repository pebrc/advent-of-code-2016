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
  (let [[l r] (split-with #(not= % \x) m)]
    (mapv chars-to-int [l (rest r)])))


(defn decompress-marker [[i x] s]
  (let [[h t] (split-at i s)]
    [(flatten (repeat x h)) t]))

(defn decompress [in]
  (letfn
      [(init [acc [_ & r :as input]]
         #(case _
            \( (decomp acc input)            
            nil acc
            (consume acc r)))
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

(count  (decompress input))
