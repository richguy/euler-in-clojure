(ns testlein.euler15
  (:require [clojure.math.numeric-tower :as math]))

(defn char2ints [v] (map #(Integer/parseInt (str %)) v))

(reduce + (char2ints(str(math/expt 2 1000))))

