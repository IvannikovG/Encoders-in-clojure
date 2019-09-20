
(ns sardinas-patterson)

(defn give-dangling [t-coll coll]
  "finds the prefix, returns as vector, if not found returns []"
  (if (>= (count t-coll) (count coll))
    []
    (if (= t-coll (subvec coll 0 (count t-coll)))
      (subvec coll (count t-coll))
      [])))

;;(give-dangling [1 1] [1 1 0 1])

(defn suffix-adder [coll]
  "checks if first element from coll is a prefix of the other elements, returns a vector of dangling suffixes"
  "[100, 10, 1001, 1000] -> [[1][0]]"
  (defn iter [acc first-el rest-els]
    (if (empty? rest-els)
      (remove empty? acc)
      (iter (conj acc (give-dangling first-el (first rest-els))) first-el (next rest-els))))
  (iter [] (first coll) (next coll)))

;;(suffix-adder [[1 0 0] [1 0] [1 0 0 1] [1 0 0 0] [1 0 0 0]])

(defn finder [coll]
  (defn iter-i [c acc]
    (if (empty? c)
      acc
      (iter-i (next c) (into acc (suffix-adder c)))))
  (iter-i (sort-by count coll) []))

;;(into [[1 0] [1] [0 1]] (finder [[1 0] [1] [0 1]]))

(defn sardinas-patterson
  "determines if your codewords are uniquely decodable, returns true or false and prints out the resulting coll"
  [coll]
  (def new-coll (into coll (finder coll)))
  (if (= (count coll) (count new-coll))
    true
    (if (apply distinct? new-coll)
      (sardinas-patterson new-coll)
      false)))

;;(println (apply distinct? [[0 1] [0 1] [0 2]]))

 ;;(sardinas-patterson [[0 1 0] [0 1 0 0] [0 0 1 0] [2 0] [2 1] [2 2]])
 ;;(sardinas-patterson [[1] [1 0 0 0] [1 0 0 1] [1 0 1 0] [1 0 1 1] [1 1 0 0]])
 ;;(sardinas-patterson [[1 1 1] [0] [1 0 0 0 2] [0 2] [1 2 1] [0 1 1]])
 ;;(sardinas-patterson [[1 2 2] [0 0 0] [0 2 0] [1 1 1] [0 1 2] [2 0 1]])
 ;;(sardinas-patterson [[0 1] [0 0 1] [0 0] [0]])