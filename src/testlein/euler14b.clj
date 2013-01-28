(ns testlein.euler14b)

(def collatz-memo
  (memoize
    (fn  
      ([n] (collatz-memo n 1 n))
      ([n c orign]
        #_(print n c \newline)
        (cond
          (= n 1) [orign c]
          (even? n) (recur (/ n 2) (inc c) orign)
          :else (recur (+ (* 3 n) 1) (inc c) orign))))))

#_(collatz 13)
(time (key (apply max-key val (into {} (map collatz-memo (range 1 100000))))))

