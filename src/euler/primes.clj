(ns euler.primes)

(use 'clojure.math.numeric-tower)
(use 'euler.common)

(defn- prime-divisors-count [number primes]
  {:doc "count number of prime divisors of given number" }

  (let [limit (inc (bigint (sqrt number)))]
    (count
     (->> primes
          (filter #(< % limit))
          (map #(divides? number %))
          (filter true?)))))

(defn isPrime? [number primes]
  {:doc "checks if given number is prime using precalculated array of primes"}

  (zero? (prime-divisors-count number primes)))

(defn primes
  {:doc "generates array of prime numbers" }

  ([up-to]
   (loop [previous 3
          generated [2 3]]
     (if (>= previous up-to)
       generated
       (let [current (+ previous 2)]
         (if (isPrime? current generated)
           (recur current (conj generated current))
           (recur current generated))))
     )
   ))
