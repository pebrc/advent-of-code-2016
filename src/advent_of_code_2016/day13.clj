(ns advent-of-code-2016.day13
  "As seen in Joy of Clojure"
  (:require [advent-of-code-2016.core :refer :all]))

(def input 1358)

(def target [31 39])

(defn wall? [[x y] magic]
  (not= 0 (mod (Integer/bitCount (+ magic (* y y) y (* 2 x y) (* 3 x) (* x x))) 2)))

(defn cell-cost [magic xy]
  (if (wall? xy magic)
    1000
    1))

(defn neighbors [xy]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map #(vec (map + xy %)))
       (filter (fn [new-xy] (every? #(< -1 %) new-xy)) )))

(defn estimate-cost [step-cost target [x y]]
  (Math/abs (* step-cost (- (apply + target ) x y))))


(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est target xy]
  (+ newcost
     (estimate-cost step-cost-est target xy)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))


(defn done? [target work-queue]
  (let [_ (println "done?" (:cost target) (first work-queue) (count work-queue))]
    (or
     (empty? work-queue)
     (if-let [goal (:cost target)]
        (< goal (first (min-by first work-queue)))
        false))))

(defn astar [start-xy target step-est cell-cost]
  (loop [steps 0
         routes {}
         work-todo (sorted-set [0 start-xy])]
    (if (done? (get-in routes target) work-todo)
      (let [res (get-in routes target) ]
        [res  :steps (count (rest (:xys res))) :iterations steps])
      (let [[_ xy :as work-item] (first work-todo)
            rest-work-todo (disj work-todo work-item)
            nbr-xys (neighbors xy)
            cheapest-nbr (min-by :cost (keep #(get-in routes %) nbr-xys))
            newcost (path-cost (cell-cost xy) cheapest-nbr)
            oldcost (:cost (get-in routes xy))]
        (if (and oldcost (>= newcost oldcost))
          (recur (inc steps) routes rest-work-todo)
          (recur (inc steps)
                 (assoc-in routes xy
                           {:cost newcost
                            :xys (conj (:xys cheapest-nbr []) xy)})
                 (into rest-work-todo
                       (map
                        (fn [xy]
                          [(total-cost newcost step-est target xy ) xy])
                        nbr-xys))))))))

#_ (astar [1 1] [31 39] 900 (partial cell-cost input))

#_ (astar [1 1] [7 4] 900 (partial cell-cost 10))


