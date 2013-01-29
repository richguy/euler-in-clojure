(ns testlein.euler14c)

(defn next-collatz [n]
  (cond
    (even? n) (/ n 2)
    :else (+ (* 3 n) 1)))

(defn collatz-entry
  ([n] (collatz-entry n n 1))
  ([n o c]
    (cond
      (= 1 n) [o c]
      :else (recur (next-collatz n) o (inc c)))))

(defn collatz-coll
  [n]
    (key (apply max-key val (into {} (map collatz-entry (range 1 n))))))

(time (collatz-coll 100000))

