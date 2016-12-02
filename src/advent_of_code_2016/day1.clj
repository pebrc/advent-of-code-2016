(ns advent-of-code-2016.day1)


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
  [(keyword (clojure.string/lower-case (subs i 0 1))) (read-string (subs i 1))])

(defn eval-instr [{:keys [dir] :as state} i]
  (let [[reldir offset] (read-instr i)
        new-idx (new-dir-idx dir reldir)
        new-dir (get dirs new-idx)
        new-state ((new-dir movements) state offset)]
    (assoc new-state :dir new-idx)))



(defn day1 [coords]
  (->> coords
       (reduce eval-instr {:dir 0 :x 0 :y 0})
       (#(+ (Math/abs (:x %)) (Math/abs (:y %))))))

(defn parse-input-str [i]
  (->>
       (clojure.string/split i  #",")
       (map clojure.string/trim)))

(day1 (parse-input-str input))
