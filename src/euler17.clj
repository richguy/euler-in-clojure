(ns testlein.euler16
  (:require clojure.pprint))

(defn count-letters [x]
                (reduce + (map #(count 
                        (clojure.string/replace 
                          (clojure.string/replace 
                            (clojure.pprint/cl-format nil "~r" %) 
                          #"hundred[\d\s\w]" "hundred and")
                         #"\s|-" "")) (range 1 (inc x)))))
