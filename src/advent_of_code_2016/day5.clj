(ns advent-of-code-2016.day5
  (:require [clojure.string :as s])
  (:import java.security.MessageDigest
           java.math.BigInteger))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(def secret "wtnhxymk")

(defn next-hash [i] (iterate (fn [[idx h]] [(inc idx) (md5 (str secret idx))]) [i ""]))

(defn relevant? [h]
  (s/starts-with? h "00000"))






(defn day5-part1
  []
  (->> (range)
     (map #(md5 (str secret %)))
     (filter relevant?)
     (map #(nth % 5))
     (take 8)
     (apply str)))

(defn parse-char [c]
  (if-let [c c] 
    (read-string (str c))))

(defn valid? [i]
  (and
   (number? i)
   (>= i 0)
   (>= 7 i)))

(defn available? [i p]
  (nil? (get p i)))

(defn day5-part2 []
  (->> (take 9 (iterate
                (fn [[p n]]
                  (let [[new-idx h] (first
                                     (drop-while
                                      (fn [[_ h]]
                                        (let [i (parse-char (get h 5))]
                                          (not
                                           (and
                                            (relevant? h)
                                            (valid? i)
                                            (available? i p)))))
                                      (next-hash n)))]
                    [(assoc p (parse-char (get h 5)) (get h 6)) new-idx])) [{} 0]))
       (last)
       (first)
       (into (sorted-map))
       vals
       (apply str)))


(day5-part2)







