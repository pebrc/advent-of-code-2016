(ns advent-of-code-2016.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn sections
  [s]
  (->> s
     (re-seq #"([^\[\]]+)(\[[^\[\]]+\])?")
     (mapcat next)
     (remove nil?)))

(def abbas #"([a-z])(?!\1)([a-z])\2\1")

(def input
  (-> "day7.txt"
      io/resource
      slurp
      s/trim
      s/split-lines))


(defn supports-tls? [secs]
  (reduce (fn [r s]
            (cond
              (and (s/starts-with? s "[" )
                   (re-find abbas s))       (reduced false)
              (re-find abbas s)             true
              :else (or r  false)
              ))
          false
          secs))

(defn day7-part1
 []
 (->> input
         (map sections)
         (filter supports-tls?)     
         count))


(defn aba [s]
  (map second (re-seq #"(?=((.)(?!\2).\2))" s)))


(defn ->bab [aba]
  (str (apply str (rest aba)) (second aba)))


(defn day7-part2
  []
  (->> input
       (map (comp
             #(reduce-kv (fn [m k v]
                           (assoc m k (set (mapcat aba v)))) {} %)
             (fn [addr] (group-by #(if (s/starts-with? % "[") :hypernet :supernet) addr ))
             sections))
       (filter #(not-empty (set/intersection (:supernet %)  (set (map ->bab (:hypernet %))))))
       count))
