(ns euler23-40
  (:require [clojure.math.combinatorics :as combo]
            [clj-time.core :as tm]
            [clj-time.format :as tmf]
            [clojure.math.numeric-tower :as math]))

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
       (reduce *)))

(defn euler12 []
  (loop [chk 1]    
    (cond
     (< 500  (num-divisors (triangle chk))) (triangle chk)
     :else (recur (inc chk)))))


;;euler 19

(defn inc-date-by-month [ds] (tm/plus- ds (tm/months 1)))

(defn inc-date-test [start-ds] (iterate inc-date-by-month start-ds))

(defn count-sundays [orig fin]
  (count (filter (fn [a] (= 7 (tm/day-of-week a)))
                 (take-while #(tm/before? % (tm/plus- fin (tm/days 1)))
                             (inc-date-test orig)))))

;;euler 24
(defn euler24-easy []
  (clojure.string/join (nth (combo/permutations [0 1 2 3 4 5 6 7 8 9]) 999999)))

(defn factorial [n]
  (reduce *' (range 1 (+ 1 n))))

(defn permut-fact
  ([targ numset] (permut-fact (- targ 1) numset []))
  ([targ numset res]
     (cond
      (= 1 (count numset)) (print (clojure.string/join
                                   (reverse (cons (first numset) res))))
      :else (let
                [mult (factorial (- (count numset) 1))
                 position  (quot targ mult)
                 digit (nth numset position)
                 next-targ (- targ (* mult position))
                 next-numset (remove #(= digit %) numset)
                 next-result (cons digit res)
                 ]
              (permut-fact next-targ next-numset next-result)))))


;; Euler26 

(defn repeat-dec
  ([n d] (repeat-dec (* 10 (rem n d)) d (conj (vec  []) (rem n d))))
  ([n d rems]
     (cond
      (some #{(rem n d)} rems) (count rems)
      :else (repeat-dec (* 10 (rem n d)) d (conj rems (rem n d))))))

(defn first-occur [sitem sseq]
  (loop [i 0
         n sseq]
    ;;(print sitem " " (first n) \newline)
    (if (= sitem (first n))
      i
      (do #_(print (first n) " " i \newline)
          (recur (inc i) (rest n))))))

(defn euler26 [n]
  (let [list (map #(repeat-dec 1 %) (range 1 n))
        listmax (apply max list) ]
     (+ 1 (first-occur listmax list))))

;; ;;euler27
;; Considering quadratics of the form:

;; n² + an + b, where |a|  1000 and |b|  1000

;; where |n| is the modulus/absolute value of n
;; e.g. |11| = 11 and |4| = 4
;; Find the product of the coefficients, a and b, for the quadratic
;; expression that produces the maximum number of primes for
;; consecutive values of n, starting withn = 0.

;; (defn euler27
;;   (let [nprimes {}]
;;     (for [a (range -1000 1000)
;;            b (range -1000 1000)
;;           n (range 0 40)
;;            :when ])
;;     (cond
;;      (b > 1000) (recur (inc a))
;;      (+ (math/expt n 2) (* a n) ))))

(defn lazy-primes3 []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                  (dissoc candidate)
                  (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                    (lazy-seq (next-primes (next-sieve sieve candidate)
                                           (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(defn isprime? [n]
  (assert (> n 1))
  (let [plt (take-while #(<= % (math/expt n 0.5)) (lazy-primes3))]
    (not-any? #(= 0 (rem n %)) plt)))

(defn prod-conseq-p []
  (let
      [tm {}]
    (for [a (range -1000 1000)
          b (range -1000 1000)
          n (range 0 40) :while (isprime? (+ (math/expt n 2) (* a n) b))]
      [(* a b) n])))

(defn euler27 []
  (key (apply max-key val (into {} (prod-conseq-p)))))

;;euler28

(defn diag-sum []
  (loop [sides 2
         square 1
         diags [1]]
    (let [lastd (last diags)]
      (if (> square 500)
        (reduce + diags)
        (recur (+ sides 2) (inc square)
               (conj diags (+ lastd sides)
                     (+ lastd (* 2 sides))
                     (+ lastd (* 3 sides))
                     (+ lastd (* 4 sides))))))))

;;Euler29
(defn euler29 [] (count (distinct (map #(reduce math/expt %)
                                       (combo/selections (range 2 101) 2)))))

;;Euler30
;; Surprisingly there are only three numbers that can be written
;; as the sum of fourth powers of their digits:

;; 1634 = 14 + 64 + 34 + 44
;; 8208 = 84 + 24 + 04 + 84
;; 9474 = 94 + 44 + 74 + 44
;; As 1 = 14 is not a sum it is not included.

;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;; Find the sum of all the numbers that can be written as the sum
;; of fifth powers of their digits.

(defn char2ints [v] (map #(Integer/parseInt (str %)) v))

(defn fifth-power [n] (map #(math/expt % 5) n))

(defn eq-5th-pow? [n]
  (= n (reduce + (fifth-power (char2ints (str n))))))

(defn euler30b []
  (reduce + (filter #(eq-5th-pow? %) (range 10 355000))))
