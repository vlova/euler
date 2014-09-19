(ns euler.task49)

(use 'clojure.math.numeric-tower)
(use 'euler.common)
(use 'euler.primes)

(defn- arePermutations? [a b c]
  (let [a (sort (str a))
        b (sort (str b))
        c (sort (str c))]
    (= a b c)))

(defn- isArithmeticProgression? [a b c]
  (= (- b a) (- c b)))

(defn task []
  (take 2
    (let [primes (vec (filter #(< 1000 %) (primes 9999)))
          primes-end (count primes)
          indexes (for [x (range 1 primes-end)
                        y (range 1 primes-end)
                        z (range 1 primes-end)
                        :when (< x y z)]
                    [x y z])
          primes (map #(identity [(nth primes (first %))
                                  (nth primes (second %))
                                  (nth primes (third %))])
                      indexes)
          primes (filter #(apply isArithmeticProgression? %) primes)
          primes (filter #(apply arePermutations? %) primes)]
      primes
      )))

(task)
