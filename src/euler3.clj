(defn factor? 
  [n d]
  (zero? (rem n d)))

(defn largest-prime-factor-of [num]
  (let [q (long (Math/sqrt num))]
    (loop [n num
           d 2]
      (cond
       (> d q) n                           ; num itself must be prime
       (= n d) n                           ; n must be prime
       (factor? n d) (recur (/ n d) d)     ; factor d out of n
       :else         (recur n (inc d)))))) ; else try again with bigger d

(largest-prime-factor-of 600851475143)