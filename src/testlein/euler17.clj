(ns testlein.euler16
  (:require clojure.pprint))


(defn count-letters [x]
                (count(clojure.pprint/cl-format true "~r" x)))


