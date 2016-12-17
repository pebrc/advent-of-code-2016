(ns advent-of-code-2016.day14
  (:require [advent-of-code-2016.core :refer :all]
            [clojure.string :as s]))

(def ^:dynamic *salt* "ahsbgdzn")

(defn salted-hash [idx]
  (md5 (str *salt* idx)))

(def hashes (->>  (range)
                  (map salted-hash)))

(def triplet #"(.)\1\1")

(some  #(s/includes? % "aaa") ["alskjf" "slkjfsfaaa"])

(defn is-key? [others hash]
  (when-let [match (re-find triplet hash)]
    (let [confirm (apply str (repeat 5 (second match)))]
      (some #(s/includes? % confirm) others))))

(defn one-time-keypad [hashes]
  (loop [idx 0 num-keys 0 hs hashes]
    (if (= 64 num-keys)
      (dec idx)
      (let [new-key? (is-key? (take 1000 (rest hs)) (first hs))]
        (recur (inc idx) (if new-key? (inc num-keys) num-keys) (rest hs))))))


;; part1
#_(one-time-keypad hashes)


(defn stretched-hash [idx]
  (last (take 2017 (iterate md5 (salted-hash idx)))))

;; part2
#_(one-time-keypad (map stretched-hash (range)))





