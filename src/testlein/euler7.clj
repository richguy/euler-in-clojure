(ns euler7)

(defn sieveofe
  [cands]            ;list of prime candidates to "take x" wrap "take" around (iterate inc 2)
  (cons (first cands) 
        (lazy-seq (sieveofe (doall(filter #(not= (mod %1 (first cands)) 0) (rest cands)))))))

(try
  (take 2000 (sieveofe (iterate inc 2)))
 (catch java.lang.StackOverflowError e
  (.printStackTrace e)))

