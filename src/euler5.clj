;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(ns euler)

(defn gcd
  [a b]
  '(print a b \newline)
  (cond 
    (= 0 b) a
    :else (gcd b (mod a b))))

(defn lcm
  [a b]
  (print a b \newline)
  (/ (Math/abs (* a b)) (gcd a b)))

(reduce #(lcm %1 %2) (range 1 21))
  
  