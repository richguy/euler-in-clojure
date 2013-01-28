;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(ns euler
  (:require [clojure.math.numeric-tower :as math]))

( - (math/expt (reduce + (range 1 101)) 2) 
    (reduce #(+ %1 (math/expt %2 2)) (range 1 101)))



