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
   (empty? work-queue)
   (when-let [goal (:cost target)]
     (< goal (first (min-by first work-queue))))))

(defn astar [start-xy target neighbors estimate-cost step-est cell-cost]
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
                          [(total-cost estimate-cost newcost step-est target xy ) xy])
                        nbr-xys))))))))

 

(defn bfs
  ([start-xy goal? neighbors]
   (bfs start-xy goal? neighbors identity))
  ([start-xy goal? neighbors coords]
           (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) start-xy)
                  routes (assoc-in {} (coords start-xy) {:xys [start-xy] :dist 0})]
             (let [cur (spy (peek q))
                   cur-xy (coords cur)
                   cur-node (get-in routes cur-xy)]
               (if (goal? cur cur-node )
                 routes
                 (let [nbr-xys (neighbors cur)
                       new-nbrs (filter #(nil? (get-in routes (coords %))) nbr-xys)]
                   (recur (into (pop q) new-nbrs)
                          (reduce (fn [rs xy] (assoc-in
                                               rs
                                               (coords xy)
                                               {:xys (conj (:xys cur-node) xy)
                                                :dist (inc (:dist cur-node))})) routes  new-nbrs))))))))
