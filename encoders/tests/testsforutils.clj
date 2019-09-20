
(ns tests-for-utils (:require [my-utils :as utils]
                            [clojure.test :as c-t :refer [is deftest]]))

(deftest log-test
  (is (= (int (utils/logarithm 2 8)) 3))
  (is (= (int (utils/logarithm 5 125)) 3))
  (is (= (utils/logarithm 14 1) 0.0)))

(deftest exp-test 
  (is (= (utils/exp 2 3) 8))
  (is (= (utils/exp 5 6) 15625))
  (is (= (/ 1 (utils/exp 2 10)) 1/1024)))

(deftest unary-0-test
  (is (= (utils/unary-0 5) [0 0 0 0 0 1]))
  (is (= (utils/decode-unary-part-0 [0 0 0 0 0 1]) 5)))

(deftest unary-1-test
  (is (= (utils/unary-1 5) [1 1 1 1 1 0]))
  (is (= (utils/decode-unary-part-1 [1 1 1 1 1 0]) 5)))

(deftest transform-test
  (is (= (utils/decimal-to-ary 10 2) [1 0 1 0]))
  (is (= (utils/decimal-to-ary 15 3) [1 2 0]))
  (is (= (utils/decimal-to-ary 80 8) [1 2 0]))
  (is (= (utils/decimal-to-ary 1243 5) [1 4 4 3 3]))
  (is (= (utils/fractional-to-ary 0.5 2 2) [0 1]))
  (is (= (utils/fractional-to-ary 0.8 3 4) [0 2 1 0]))
  (is (= (utils/decode-ary-code [1 2 5 3 6 2] 7) 23515))
  (is (= (utils/decode-ary-code [1 2 4 3] 5) 198))
  (is (= (utils/ary-to-fractional [0 1 2 3] 4) 27/64)))

(deftest length-test
  (is (= (utils/ary-of-length [0 1 2 3 4 2 1 7] 4) [0 1 2 3]))
  (is (= (utils/ary-of-length [0 1 2] 5) [0 0 0 1 2])))

(comment
  (clojure.test/run-tests))