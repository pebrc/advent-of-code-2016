(ns advent-of-code-2016.day17
  (:require [advent-of-code-2016.core :refer :all]
            [advent-of-code-2016.algo :as a]))


(def test-input "hijkl")

(defn open? [c]
  (case c
    (\b \c \d \e \f) true
    false))

(defn possible-moves [secret]
  (->>(map vector (map open? (take 4 (md5 secret))) [[0 1] [0 -1] [-1 0] [1 0]]  (map (partial str secret)  [\U \D \L \R]))
      (filter first)
      (map rest)))

(defn neighbors [size [xy state]]
  (->> (possible-moves state)
       (map (fn [[offset state]] (vector (mapv + xy offset) state)))
       (filter (fn [new-xy] (every? #(< -1 % size) (first new-xy))) )))


(defn day17-part1 [secret]
  (-> (a/bfs [[0 3] secret] (fn [[xy _] _] (= xy [3 0])) (partial neighbors 4) first)
      (get-in [3 0])
      :xys
      last
      second
      (subs (count secret))))

#_(day17-part1 "kglvqrro")


