(ns euler23-40
  (:require [clojure.math.combinatorics :as combo]))

(defn lazy-seq-fibo
    ([]
        (concat [0 1] (lazy-seq-fibo 0 1)))
    ([a b]
        (let [n (+' a b)]
            (lazy-seq
                (cons n (lazy-seq-fibo b n))))))

(defn euler23 [] (count (take-while #(if (> 1000 (count (str %))) %)
                              (lazy-seq-fibo))))

;eluer12
(defn triangle [n]
  (reduce + (range 1 (+ 1 n))))

;;; No stack consuming algorithm
(defn factors
  "Return a list of factors of N."
  ([n]
    (factors n 2 ()))
  ([n k acc]
    (if (= 1 n)      
      acc
      (if (= 0 (rem n k))        
        (recur (quot n k) k (cons k acc))
        (recur n (inc k) acc)))))

(defn num-divisors [n]
  ;; uses the factors plus multiplicity to calculate divisors
  (->> (factors n)
       (frequencies)
       (vals)
       (map inc)
       (reduce *))
  )

(defn euler12 []
  (loop [chk 1]    
    (cond
     (< 500  (num-divisors (triangle chk))) (triangle chk)
     :else (recur (inc chk)))))

