(ns advent-of-code-2016.day4
  (:require [clojure.string :as s]
            [clojure.java.io :as io])
  (:import (java.io BufferedReader StringReader)))

(def input (-> "day4.txt"
               io/resource
               slurp
               s/trim
               s/split-lines))


(defn parse-input [s]
  (map (comp
        (fn [[_ enc id chks]]
          (assoc {}
                 :enc-name (s/replace enc "-" "" )
                 :sector-id (read-string id) :checksum chks))
        #(re-find  #"((?:[a-z]+-*)+)-(\d+)\[([a-z]+)\]" %))
       s))

(defn sorted-map-by-value [x]
  (into (sorted-map-by
         (fn [k1 k2]
           (let [cr (compare (get x k2) 
                             (get x k1))]
             (if (= 0 cr)
               (compare k1 k2)
               cr))))
        x))

(defn without-decoys [input]
  (->> (parse-input input)
       (map (fn [r] (assoc r :freq (sorted-map-by-value (frequencies (:enc-name r))) )))
       (filter #(= (:checksum %) (apply str (take 5 (keys (:freq %))))))))

(defn part1 [input]
  (->> (without-decoys input)
       (map  :sector-id)
       (reduce +)))

(defn decrypt [enc offset]
  (apply str (map #(char (+ (mod (+ (- (int %) (int \a)) offset) 26) (int \a) )) enc)))

(defn part2 [input]
  (->> (without-decoys input)
       (map (fn [r] (assoc r :clear-name (decrypt (:enc-name r) (:sector-id r)))))
       (filter #(s/starts-with? (:clear-name  %) "northpoleobjects" ))
       (map :sector-id)))
