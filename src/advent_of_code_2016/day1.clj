(ns advent-of-code-2016.day1
  (:require [clojure.string :as s]))


(def dirs  [:n :e :s :w])

(def movements {:e (fn [{:keys [x y]} offset] {:x (+ x offset) :y y})
                :w (fn [{:keys [x y]} offset] {:x (- x offset) :y y})
                :n (fn [{:keys [x y]} offset] {:x x :y (+ y offset)})
                :s (fn [{:keys [x y]} offset] {:x x :y (- y offset)})})

(def test-input1 "R5, L5, R5, R3")

(def test-input2 "R2, R2, R2")

(def test-input3 "R2, L3")

(def input "L2, L3, L3, L4, R1, R2, L3, R3, R3, L1, L3, R2, R3, L3, R4, R3, R3, L1, L4, R4, L2, R5, R1, L5, R1, R3, L5, R2, L2, R2, R1, L1, L3, L3, R4, R5, R4, L1, L189, L2, R2, L5, R5, R45, L3, R4, R77, L1, R1, R194, R2, L5, L3, L2, L1, R5, L3, L3, L5, L5, L5, R2, L1, L2, L3, R2, R5, R4, L2, R3, R5, L2, L2, R3, L3, L2, L1, L3, R5, R4, R3, R2, L1, R2, L5, R4, L5, L4, R4, L2, R5, L3, L2, R4, L1, L2, R2, R3, L2, L5, R1, R1, R3, R4, R1, R2, R4, R5, L3, L5, L3, L3, R5, R4, R1, L3, R1, L3, R3, R3, R3, L1, R3, R4, L5, L3, L1, L5, L4, R4, R1, L4, R3, R3, R5, R4, R3, R3, L1, L2, R1, L4, L4, L3, L4, L3, L5, R2, R4, L2")

(defn new-dir-idx [s c]
  (let [mut (if (= c :l) dec inc)]
    (mod (mut s) (count dirs))))

(defn read-instr [i]
  (let [trimmed (s/trim i)]
    [(keyword (s/lower-case (subs trimmed 0 1)))
     (read-string (subs trimmed 1))]))

(defn eval-instr [{:keys [dir] :as state} [reldir offset]]
  (let [new-idx (new-dir-idx dir reldir)
        new-dir (get dirs new-idx)
        new-state ((new-dir movements) state offset)]
    (assoc new-state :dir new-idx)))


(defn taxicab-distance [coords]
  (+ (Math/abs (:x coords)) (Math/abs (:y coords))))

(defn day1 [coords]
  (->> coords
       (reduce eval-instr {:dir 0 :x 0 :y 0})
       (taxicab-distance)))


(defn parse-input-str [i]
  (->>
   (clojure.string/split i  #",")
   (map read-instr)))


(comment (day1 (parse-input-str input)))



(defn interpolate-steps [acc {:keys [x y] :as next}]
  (if (seq acc)
    (let [{px :x py :y } (last acc)
          dx (- x px)
          dy (- y py)
          step (fn [k f acc n]
                 (loop [res acc c (last acc) n n]
                   (if (>= 0 n) res
                       (let [ns (update c k f)]
                         (recur (conj res ns) ns (dec n)))
                       )))
          step-fn (fn [d] (if (> 0 d) dec inc))]
      (if (= 0 dx)
        (step :y (step-fn dy) acc (Math/abs dy))
        (step :x (step-fn dx) acc (Math/abs dx))))
    (conj acc next)))


(defn day1-part2 [input]
            (->> (parse-input-str input)    
                 (reductions eval-instr {:dir 0 :x 0 :y 0})
                 (reduce interpolate-steps [])
                 (reduce (fn [acc state] (let [pos (select-keys state [:x :y])]
                                           (if (contains? acc pos)
                                             (reduced pos)
                                             (conj acc pos)))) #{})
                 (taxicab-distance)))




(comment (day1-part2 input))

