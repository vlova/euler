(ns euler.task49)

(use 'clojure.math.numeric-tower)
(use 'euler.common)

(defn task []
  (sum (->> (range 1 1000001)
     (filter #(isPalindrome? % 10))
     (filter #(isPalindrome? % 2)))))

(time (task))
