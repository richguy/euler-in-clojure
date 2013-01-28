(ns testlein.euler16
  (:require clojure.pprint))

;test update
(defn count-letters [x]
                (count(clojure.pprint/cl-format true "~r" x)))


