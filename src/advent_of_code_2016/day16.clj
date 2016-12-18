(ns advent-of-code-2016.day16
  (:require [clojure.string :as s]))

(defn dragon-curve-mod [a]
  (let [a a
        b  (s/replace  (s/replace  (s/replace (s/reverse a) "0" "a") "1" "0") "a" "1")]
    (str a "0" b)))


(defn checksum [s]
  (letfn [(cs [s]
            (apply str (map #(if (apply = %)  "1" "0")  (partition 2 s))))]
    (loop [s (cs s)]
      (if (odd? (count s))
        s
        (recur (cs s))))))

(defn truncate [size s] 
  (subs s 0 (min (count s) size)))

(defn fill-disk [init size]
  (->> (iterate dragon-curve-mod init)
       (drop-while #(< (count %) size))
       first
       (truncate size)
       checksum))

#_(fill-disk "11101000110010100" 35651584)


