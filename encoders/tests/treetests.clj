
(ns tree-tests (:require [tree-utils :as t-u]
                           [clojure.test :as c-t :refer [is deftest]]))

(def sample-tree
  (t-u/make-code-tree (t-u/make-leaf 'A 4)
                   (t-u/make-code-tree
                   (t-u/make-leaf 'B 2)
                   (t-u/make-code-tree (t-u/make-leaf 'D 1)
                                       (t-u/make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(deftest huffman-tree-scheme-test
  (is (= (t-u/encode '(A D A B B C A) sample-tree) '(0 1 1 0 0 1 0 1 0 1 1 1 0)))
  (is (= (t-u/decode sample-message sample-tree) '(A D A B B C A))))

(deftest huffman-tree-generator-test
  (is (= (t-u/generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))) 
         '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))))

(comment
  (clojure.test/run-tests))

