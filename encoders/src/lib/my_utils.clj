
(ns my-utils)

(defn logarithm [base, num]
  "yields logarithms of any number and base"
  (/ (Math/log num) (Math/log base)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn unary-1 [n]
  (conj (vec (take n (repeat 1))) 0))

(defn unary-0 [n]
  (conj (vec (take n (repeat 0))) 1))

(defn decode-unary-part-0
  ([coll] (decode-unary-part-0 coll 0))
  ([coll acc] (if (not= (first coll) 0)
                acc
                (decode-unary-part-0 (next coll) (+ acc 1)))))

(defn decode-unary-part-1
  ([coll] (decode-unary-part-1 coll 0))
  ([coll acc] (if (not= (first coll) 1)
                acc
                (decode-unary-part-1 (next coll) (+ acc 1)))))

(defn decimal-to-ary
  "transforms integers to any arity-code"
  ([num arity] (decimal-to-ary num arity []))
  ([num arity acc]
   (cond
     (>= num arity) (decimal-to-ary (int (/ num arity)) arity (conj acc (rem num arity)))
     (< num arity) (reverse (conj acc num)))))

(defn fractional-to-ary
  "transforms fractionals to any arity-code"
  ([num arity times] (fractional-to-ary num arity times []))
  ([num arity times acc] (if (= times 0)
                           acc
                           (fractional-to-ary (* (- num (int num)) arity)
                                              arity (- times 1) (conj acc (int num))))))

(defn decode-ary-code
  ([num arity] (decode-ary-code (reverse num) arity 0 0))
  ([num arity acc counter]
   (if (empty? num)
     acc
     (decode-ary-code (next num) arity 
                      (+ acc (* (exp arity counter)
                                (first num))) (+ counter 1)))))

(defn ary-to-fractional
  ([num arity] (ary-to-fractional (next num) arity 0 1))
  ([num arity acc counter] (if (empty? num)
                             acc
                             (ary-to-fractional 
                              (next num) arity
                              (+ acc (* (first num)
                                        (/ 1 (exp arity counter)))) 
                              (+ counter 1)))))

(defn ary-of-length
  [num length]
  (let [remains (- length (count num))]
    (cond (= remains 0) num
          (< remains 0) (take (* -1 remains) num)
          (> remains 0) (concat (repeat remains 0) num))))
