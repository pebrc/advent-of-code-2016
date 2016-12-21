(ns advent-of-code-2016.day17
  (:require [advent-of-code-2016.core :refer :all]
            [advent-of-code-2016.algo :as a]))


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

(def target [3 0])

(defn day17-part1 [secret]
  (-> (a/bfs [[0 3] secret] (fn [[xy _] _] (= xy target)) (partial neighbors 4) flatten)
      (get-in target)
      keys
      first
     (subs (count secret))))


#_ (= "DDUDRLRRUDRD" (day17-part1 "kglvqrro"))

#_ (= "DRURDRUDDLLDLUURRDULRLDUUDDDRR" (day17-part1 "ulqzkmiv"))

#_ (day17-part1 "rrrbmfta")


(defn stop-target [nb [xy state]]
  (if (= xy target)
    []
    (nb [xy state])))

(defn day17-part2 [secret]
  (->> (keys (get-in (a/bfs
                      [[0 3] secret]
                      (constantly false)
                      (partial stop-target
                               (partial neighbors 4))
                      flatten) target))
       (map #(count (subs % (count secret))))
       (reduce max)))

#_(day17-part2 "rrrbmfta")
