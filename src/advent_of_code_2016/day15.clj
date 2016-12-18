(ns advent-of-code-2016.day15)

;; 15 + x + 1 % 17 = 0
;; 2 + x + 2 % 3 = 0
;; 4 + x + 3 % 19 = 0
;; 2 + x + 4 % 13 = 0
;; 2 + x + 5 % 7 = 0
;; 0 + x + 6 % 5 = 0

(defn disk [disk num div]
  (fn [x] (= 0 (mod (+ num x disk) div))))


(def disks-part1 [(disk 1 15 17 )
                   (disk 2  2 3)
                   (disk 3 4 19)
                   (disk 4 2 13)
                   (disk 5 2 7)
                   (disk 6 0 5)])

(defn disks-filter [disks]
  (fn [x] (every? #(% x) disks )))

;;part1
#_(take 1 (filter (disks-filter disks-part1) (range)))

(def disks-part2 (conj disks-part1 (disk 7 0 11)))

;;part2
#_(take 1 (filter (disks-filter disks-part2) (range)))

