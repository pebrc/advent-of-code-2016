(ns advent-of-code-2016.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import (java.io BufferedReader StringReader)))


(def ^:dynamic grid [[1 4 7] [2 5 8] [3 6 9]])

(def actual-keypad '[[nil nil 5 nil nil] [nil 2 6 A nil] [1 3 7 B D] [nil 4 8 C nil] [nil nil 9 nil nil]])

(def input (-> "day2.txt"
               io/resource
               slurp
               s/trim
               s/split-lines))


(defn move [[v x y] m]
  (letfn [(new-state [mx my]
            (if-let [nv (get-in grid [(mx x) (my y)])]
              [(conj v nv) (mx x) (my y)]
              [v x y]))]
    (case m
      \U (new-state identity dec)
      \D (new-state identity inc)
      \L (new-state dec identity)
      \R (new-state inc identity))))

(defn one-digit [[ds _ _ :as s] ms]
  (let [[nds _ _ :as r] (reduce move s (seq ms))
        newdigits (conj ds (last nds))]
    (update-in r [0] (constantly newdigits))))


(defn day2-part1 [s]
  (reduce one-digit [[] 1 1]  s))

(defn day2-part2 [s]
  (with-bindings {#'grid actual-keypad}
    (reduce one-digit [[] 0 2] s)))



