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


(defn day5-part2 [l]
  (->> (range)
     (map #(md5 (str secret %)))
     (filter relevant?)     
     (map #(vector (parse-char (nth % 5)) (nth % 6)))
     (filter (fn [[idx _]] (valid? idx)))
     (reduce (fn [acc [idx v]]
               (let [res (if (available? idx acc)
                         (assoc acc idx v)
                         acc)]
                 (if (= l (count res))
                   (reduced (apply str (vals (sort res))))
                   res))) {})))

(comment (day5-part2 8))







