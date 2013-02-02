
; reverse function seems to return list of letters in reverse order that then must be concatenated into a list.
(defn str-reverse [s] (apply str (reverse s)))

(defn ispalindrome?
  [cand]
  (let
    [candstr (str cand)]
     (= candstr (str-reverse candstr))
    )
  )

;(ispalindrome? 10002)

(defn bigpal
  []
  (apply max(filter ispalindrome? (for [x (range 100 999) y (range 100 999)] (* x y)))))




