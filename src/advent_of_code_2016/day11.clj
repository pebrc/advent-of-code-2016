(ns advent-of-code-2016.day11
  (:require [clojure.set :as set]
            [clojure.core.reducers :as r]))

(defrecord Micro [e])
(defrecord Gen [e])

(def part1 {:elevator 1
            4 #{}
            3 #{}
            2 #{(Micro. :po)  (Micro. :pm)}
            1 #{(Gen. :po) (Gen. :tm) (Micro. :tm) (Gen. :pm) (Micro. :ru) (Gen. :ru) (Gen. :co) (Micro. :co)} })


(def part2 {:elevator 1
            4 #{}
            3 #{}
            2 #{(Micro. :po)  (Micro. :pm)}
            1 #{(Gen. :el) (Micro. :el) (Gen. :dil) (Micro. :dil) (Gen. :po) (Gen. :tm) (Micro. :tm) (Gen. :pm) (Micro. :ru) (Gen. :ru) (Gen. :co) (Micro. :co)} })


(defn next-floors [f]
  (filter #(and (> % 0) (> 5 %)) ((juxt inc dec) f)))

(defn is-a? [r]
  (partial instance? r))

(defn every-micro-safe? [xs]
  (every? (fn [[k v]] (or (= 2 (count v))
                       (every? (is-a? Gen) v))) (group-by :e xs)))

(defn safe-together? [f]
  (or (not-any? (is-a? Gen) f)
      (every-micro-safe? f)))

(defn combinations [k n]
  (cond (= k 0) '(nil)
        (empty? n) nil
        :else (concat (map #(conj % (first n))
                           (combinations (dec k) (rest n)))
                      (combinations k (rest n)))))

(defn elevator-loads [xs]
  (->> (concat (combinations 1 xs)        
               (combinations 2 xs))
       (filter safe-together?)
       (map set)))

(defn solution? [s]
  (every? empty? (vals (select-keys s [1 2 3]))))

(defn gen-goes-down? [cur-floor n-floor load]
  (and (> cur-floor n-floor)
       (some (is-a? Gen) load)))

(defn next-states [s]
  (let [cur-floor (:elevator s)]
    (for [floor (next-floors cur-floor)
          load (elevator-loads (get s cur-floor))
          :when (safe-together? (into (get s floor) load))]
      (-> (update s floor into load)
          (update cur-floor set/difference load)
          (assoc :elevator floor)))))

(def next-states-mem  (memoize next-states))

(defn result-state [prev s]
  (cond
    (contains? prev s) :loop
    (solution? s) :solved
    :else :running))

(defn solutions [init max-steps]
  (loop [sols [] states #{init} history #{} steps 1]
    (if (or  (not-empty sols) (empty? states) (> steps max-steps))
      sols
      (let [r (group-by (partial result-state history) (into #{} (r/mapcat next-states states)))
            _ (println steps (count (:running r)))]
        (recur (into sols (map #(assoc % :steps steps) (:solved r))) (set (:running r)) (set/union history states) (inc steps))))))


(def simple-game {:steps 0 :elevator 1 1 #{(Gen. :po) (Micro. :po)} 2 #{} 3 #{} 4 #{}})


#_(time (solutions part2 100))

