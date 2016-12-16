(ns advent-of-code-2016.core)

(defmacro spy [f]
  `(let [res# ~f]
     (println (apply str (repeat 10 "-" )))
     (println  '~f "->" res#)
     res#))

