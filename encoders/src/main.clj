
(ns main (:require [sardinas_patterson :as s_p]
                   [shannon_code :as s_c]
                   [elias_gamma :as e_g]
                   [shannon_code :as s_c]
                   [testsforutils :as tfu]
                   [testsforalgs :as tfa]
                   [tree_utils :as t_u]))

(def a (vec {"a" 0.2 "b" 0.3 "c" 0.1 "f" 0.3 "s" 0.05 "p" 0.05}))

(defn -main []
  (tfu/log-test)
  (tfu/exp-test)
  (tfu/unary-0-test)
  (tfu/unary-1-test)
  (tfu/transform-test)
  (tfu/length-test)
  (println (e_g/elias-gamma-encoder 10))
  (println (s_p/sardinas-patterson [[0 1 1] [1 0 1] [0 0 0] [0 0]]))
  (s_c/relator (s_c/shannon-codes a 4) a))

(-main)

(comment
  
  (clojure.test/run-tests)
  

  )