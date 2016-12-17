(ns advent-of-code-2016.core
  (:import java.security.MessageDigest
           java.math.BigInteger))

(defmacro spy [f]
  `(let [res# ~f]
     (println (apply str (repeat 10 "-" )))
     (println  '~f "->" res#)
     res#))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

