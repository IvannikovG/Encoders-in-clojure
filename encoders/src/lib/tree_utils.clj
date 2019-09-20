
(ns tree-utils)

(defn make-leaf [sym weight]
  (list 'leaf sym weight))

(defn leaf? [obj] (= (first obj) 'leaf))
(defn symbol-leaf [x] (second x))
(defn weight-leaf [x] (nth x 2))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (nth tree 2)))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))


(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)))

(defn adjoin-set [x s]
  (cond (empty? s) (list x)
        (< (weight x) (weight (first s))) (cons x s)
        :else (cons (first s)
                    (adjoin-set x (rest s)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs) '()
      (let [pair (first pairs)]
        (adjoin-set (make-leaf (first pair) (second pair))
                    (make-leaf-set (rest pairs))))))

(defn encode-symbol [elem tree]
  (letfn [(contains-elem? [syms] (some #(= % elem) syms))]
    (cond (and (leaf? tree)) '()
          (contains-elem? (symbols (right-branch tree)))
          (cons 1 (encode-symbol elem (right-branch tree)))
          (contains-elem? (symbols (left-branch tree)))
          (cons 0 (encode-symbol elem (left-branch tree)))
          :else 'error)))

(defn successive-merge [pairs]
  (if (= 2 (count pairs)) (make-code-tree (first pairs) (second pairs))
      (let [lowest-weight (first pairs)
            second-lowest-weight (second pairs)
            combined-first-two (make-code-tree lowest-weight second-lowest-weight)
            remaining-pairs (rest (rest pairs))
            combined-pairs (adjoin-set combined-first-two remaining-pairs)]
        (successive-merge combined-pairs))))

(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
            (if (empty? bits) '()
                (let [next-branch (choose-branch (first bits) current-branch)]
                  (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (rest bits) tree))
                    (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(defn encode [message tree]
  (if (empty? message)
    '()
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))