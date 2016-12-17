(ns advent-of-code-2016.day13
  (:require [advent-of-code-2016.core :refer :all]
            [advent-of-code-2016.algo :as a]))

(def input 1358)


(defn wall? [fav [x y]]
  (not= 0 (mod (Integer/bitCount (+ fav (* y y) y (* 2 x y) (* 3 x) (* x x))) 2)))

(defn cell-cost [fav xy]
  (if (wall? fav xy)
    10000
    1))

(defn neighbors [xy]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map #(vec (map + xy %)))
       (filter (fn [new-xy] (every? #(< -1 %) new-xy)) )))

(defn non-wall-neighbors [fav xy]
  (->> (neighbors xy)
       (filter (complement (partial wall? fav)))))

(defn estimate-cost [step-cost target [x y]]
  (Math/abs (* step-cost (- (apply + target ) x y))))



#_(a/bfs [1 1] #(= % [7 4]) (partial non-wall-neighbors 10))

#_(a/bfs [1 1] #(= % [31 39]) (partial non-wall-neighbors input))

#_ (a/astar [1 1] [31 39] neighbors  estimate-cost 900 (partial cell-cost input))

#_ (a/astar [1 1] [7 4] neighbors estimate-cost 900 (partial cell-cost 10))


