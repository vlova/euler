(ns euler.task49)

(use 'clojure.math.numeric-tower)
(use 'euler.common)

(defn- digits [n]
  (let [chars (seq (str n))]
    (->> chars
        (map str)
        (map read-string))))

(defn- factorial-digits-sum [n]
  (sum (map factorial (digits n))))

(defn task []
  (sum
   (filter #(= (factorial-digits-sum %) %)
           (range 3 40600))))

;(time (task))
