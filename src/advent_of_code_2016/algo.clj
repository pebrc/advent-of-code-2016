(ns advent-of-code-2016.algo
  (:require [advent-of-code-2016.core :refer :all]))


(defn total-cost [estimate-cost newcost step-cost-est target xy]
  (+ newcost
     (estimate-cost step-cost-est target xy)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost (:cost cheapest-nbr 0)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))

(defn done? [target work-queue]
  (or
   (empty? (spy work-queue))
   (when-let [goal (:cost target)]
     (< goal (first (min-by first work-queue))))))

(defn astar
  ([start-xy target neighbors estimate-cost step-est cell-cost]
   (astar start-xy target identity neighbors estimate-cost step-est cell-cost))
  ([start-state target coords neighbors estimate-cost step-est cell-cost]
   (loop [steps 0
          routes {}
          work-todo (sorted-set [0 start-state])]
     (if (done? (get-in routes target) work-todo)
       (let [res (get-in routes target) ]
         [res :steps (count (rest (:xys res))) :iterations steps])
       (let [[_ loc :as work-item] (first work-todo)
             rest-work-todo (disj work-todo work-item)
             nbr-xys (neighbors loc)
             cheapest-nbr (min-by :cost (keep #(get-in routes (coords %)) nbr-xys))
             newcost (path-cost (cell-cost loc) cheapest-nbr)
             oldcost (:cost (get-in routes (coords loc)))]
         (if (and oldcost (>= newcost oldcost))
           (recur (inc steps) routes rest-work-todo)
           (recur (inc steps)
                  (assoc-in routes (coords loc)
                            {:cost newcost
                             :xys (conj (:xys cheapest-nbr []) loc)})
                  (into rest-work-todo
                        (map
                         (fn [loc]
                           [(total-cost estimate-cost newcost step-est target loc ) loc])
                         nbr-xys)))))))))

 

(defn bfs
  ([start-xy goal? neighbors]
   (bfs start-xy goal? neighbors identity))
  ([start-xy goal? neighbors coords]
           (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) start-xy)
                  routes (assoc-in {} (coords start-xy) {:xys [start-xy] :dist 0})]
             (let [cur (peek q)
                   cur-xy (coords cur)
                   cur-node (get-in routes cur-xy)]
               (if (or (nil? cur) (goal? cur cur-node ))
                 routes
                 (let [nbr-xys (neighbors cur)
                       new-nbrs (filter #(nil? (get-in routes (coords %))) nbr-xys)]
                   (recur (into (pop q) new-nbrs)
                          (reduce (fn [rs xy] (assoc-in
                                               rs
                                               (coords xy)
                                               {:xys (conj (:xys cur-node) xy)
                                                :dist (inc (:dist cur-node))})) routes  new-nbrs))))))))
