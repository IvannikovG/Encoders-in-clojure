
(ns shannon-code (:require [my-utils :as utils]))

(def a (vec {"a" 0.2 "b" 0.3 "c" 0.1 "f" 0.3 "s" 0.05 "p" 0.05}))

(defn bx-for-sequence
  "each element of the sequence is the sum of all previous from the sequence"
  ([coll] (bx-for-sequence coll 0 []))
  ([coll count acc]
   (if (empty? coll)
     acc
     (bx-for-sequence (next coll)
                     (+ count (first (next (first coll))))
                     (conj acc count)))))

(defn ary-for-bx [coll arity times]
  (def bx-for-coll (bx-for-sequence coll))
  (into [] (map (fn [el] (utils/fractional-to-ary el arity times)) bx-for-coll)))

(defn shannon-codes [coll arity]
  (def sorted-coll (sort-by val > coll))
  (def lx (map (fn [el] (* -1 (int (Math/ceil 
                                    (utils/logarithm arity (first (next el)))))))
               sorted-coll))
  (defn make-codes 
    ([coll lx-coll] (make-codes coll lx-coll []))
    ([coll lx-coll acc]
     (if (empty? coll)
       acc
       (make-codes (next coll) (next lx-coll)
                   (conj acc (take (+ 1 (first lx-coll))
                         (drop 1 (first coll))))))))
  (def codes (make-codes (ary-for-bx coll arity 10) lx))
  codes)

(defn relator
  "each element of the sequence relates to the letters from the sorted sequence"
  ([shannon-coll sequence] (relator shannon-coll (sort-by val > sequence) []))
  ([shannon-coll sequence acc]
   (if (empty? shannon-coll)
     acc
     (relator (next shannon-coll) (next sequence)
              (conj acc (list (first (first sequence)) (first shannon-coll)))))))
