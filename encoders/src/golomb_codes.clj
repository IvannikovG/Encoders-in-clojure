
;;Golomb Coding. Called Rice coding is m is a power of two
;;Used to encode integers, m is a free parameter

(ns golomb-codes (:require [my-utils :refer :all]))

(defn golomb-encoder [num m]
  (def b (int (Math/ceil (logarithm 2 m))))
  (def q (int (Math/floor (/ num m))))
  (def stored-q (unary-1 q))
  (def r (- num (* q m)))
  (if (< r (- (exp 2 b) m))
    (let [new-r (ary-of-length (decimal-to-ary r 2) (- b 1))]
      (concat stored-q new-r))
    (let [new-r (ary-of-length (decimal-to-ary (- (+ r (exp 2 b)) m) 2) b)]
      (concat stored-q new-r))))

(defn golomb-decoder [num m]
  (def b (int (Math/ceil (logarithm 2 m))))
  (def decoded-q (decode-unary-part-1 num))
  (def r-1 (decode-ary-code (take (- b 1) (drop (+ decoded-q 1) num)) 2))
  (if (< r-1 (- (exp 2 b) m))
    (+ (* decoded-q m) r-1)
    (let [r-2 (decode-ary-code (take b (drop (+ decoded-q 1) num)) 2)
          r-3 (+ (- r-2 (exp 2 b)) m)]
      (+ r-3 (* decoded-q m)))))
