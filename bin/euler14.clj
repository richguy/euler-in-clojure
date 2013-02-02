(ns testlein.euler14)

;n  n/2 (n is even)
;n  3n + 1 (n is odd)

(defn collatz [n]
    (cond
    (= (last n) 1) n
    (even? (last n)) (recur (collatz (conj n (/ (last n) 2))))
    :else (recur (collatz (conj n  (+ (* 3 (last n)) 1))))))

(defn excollatz [n]
  {:count (count n) :start (first n)})

(def z [(map #(excollatz %1) (map #(collatz [%1]) (range 1 100000)))])

;(map #(apply max-key :start %) 
;            (vals (group-by :game game-vec)))

(map #(apply max-key :count %) z)