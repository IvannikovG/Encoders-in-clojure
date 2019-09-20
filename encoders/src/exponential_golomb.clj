
(ns exponential-golomb (:require [my-utils :refer :all]))

(defn exp-golomb-encoder [num m]
  (def q (int (Math/floor (logarithm 2 (+ 1 (/ num (exp 2 m)))))))
  (def unary-q (unary-0 q))
  (def r (- num (reduce (fn [acc el] (+ acc (exp 2 (+ el m)))) 0 (range 0 q))))
  (concat unary-q (ary-of-length (decimal-to-ary r 2) (+ m q))))

(defn exp-golomb-decoder [num m]
  (def q (decode-unary-part-0 num))
  (def r (decode-ary-code (ary-of-length (take (count num) (drop (+ 1 q) num)) (+ m q)) 2))
  (reduce (fn [acc el] (+ acc (exp 2 (+ el m)))) r (range 0 q)))

