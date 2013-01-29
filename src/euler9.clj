(ns euler9)


(defn pytrip
  [test]
  (for [a (range 1 test)
        b (range a test)
        c [(- test a b)]
        :when (= (* c c) (+ (* a a) (* b b)))]
    (* a b c)))

(pytrip 1000)
