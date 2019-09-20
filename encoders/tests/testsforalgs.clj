
(ns tests-for-algs (:require [elias-gamma :as e-g]
                             [clojure.test :as c-t :refer [is deftest]]
                             [golomb-codes :as g-c]
                             [exponential-golomb :as exp-g]
                             [shannon-code :as s-c]))

(deftest elias-gamma-test
  (is (= (e-g/elias-gamma-decoder (e-g/elias-gamma-encoder 15)) 15))
  (is (= (e-g/elias-gamma-decoder (e-g/elias-gamma-encoder 1)) 1))
  (is (= (e-g/elias-gamma-decoder (e-g/elias-gamma-encoder 3571)) 3571))
  (is (= (e-g/elias-gamma-encoder 17) [0 0 0 0 1 0 0 0 1]))
  (is (= (e-g/elias-gamma-decoder [0 0 0 0 1 0 0 0 0]) 16)))

 (deftest shannon-codes-test
   (is (= (s-c/relator
           (vec {"a" 0.2 "b" 0.3 "c" 0.1 "f" 0.3 "s" 0.05 "p" 0.05}) 2)
          [("b" (0 0)) ("f" (0 0)) ("a" (1 0 0)) ("c" (1 0 0 1)) ("s" (1 1 1 0 0)) ("p" (1 1 1 1 0))]))
   (is (= (s-c/relator
           (vec {"a" 0.2 "b" 0.3 "c" 0.1 "f" 0.3 "s" 0.05 "p" 0.05}) 4)
          [("b" (0)) ("f" (0)) ("a" (2 0)) ("c" (2 1)) ("s" (3 2 1)) ("p" (3 3 0))])))

(deftest golomb-test
  (is (= (exp-g/exp-golomb-encoder 15 5) [1 0 1 1 1 1]))
  (is (= (exp-g/exp-golomb-decoder [0 1 0 0 0 1] 3) 9))
  (is (= (g-c/golomb-encoder 10 3) [1 1 1 0 1 0]))
  (is (= (g-c/golomb-encoder 500 48) [1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0]))
  (is (= (g-c/golomb-decoder (g-c/golomb-encoder 27 6) 6) 27)))

(comment
  (clojure.test/run-tests))