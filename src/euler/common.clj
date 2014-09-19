(ns euler.common)

(use 'clojure.math.numeric-tower)

(defn divides? [number div]
  (= (mod number div) 0))

(defn third [coll]
  (nth coll 2))
