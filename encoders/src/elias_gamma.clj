(ns elias-gamma (:require [my-utils :as utils]))

(defn elias-gamma-encoder
  [num]
  "Only for non-negative integers"
  (def n (int (utils/logarithm 2 num)))
  (def unary-n (utils/unary-0 n))
  (def integer (utils/decimal-to-ary (- num (utils/exp 2 n)) 2))
  (concat unary-n (utils/ary-of-length integer n)))

(defn elias-gamma-decoder
  [num]
  (def unary-part (utils/decode-unary-part-0 num))
  (def integer (utils/decode-ary-code (take (count num) (drop (+ unary-part 1) num)) 2))
  (let [result (+ (utils/exp 2 unary-part) integer)]
    result))
