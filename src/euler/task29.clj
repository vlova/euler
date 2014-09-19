(ns euler.task4)

(use 'clojure.math.numeric-tower)

(defn task []
  (count
   (distinct
    (for [a (range (bigint 2) (bigint 101))
          b (range (bigint 2) (bigint 101))]
      (expt a b)))))
