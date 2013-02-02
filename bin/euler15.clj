(ns testlein.euler15)

(defn factorial [n]
  (reduce *' (range 1 (inc n))))

(defn combination [n k]
  (cond (zero? n) 0
        (zero? k) 1
        :else (/ (factorial n) (*' (factorial (- n k)) (factorial k)))))



