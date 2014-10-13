(ns euler.task55)

(use 'euler.common)
(use 'clojure.math.numeric-tower)

(defn task []
  (apply
   max
   (for [a (range 100)
         b (range 100)]
     (apply + (map (comp bigint str) (str (expt a b)))))))

(future
  (println "answer:" (task)))
