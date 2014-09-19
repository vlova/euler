(ns euler.task12)

(use 'clojure.math.numeric-tower)

(defn- triangle-numbers
  ([]
   (triangle-numbers 1 0))

  ([index previous]
   (let [current (+ previous index)]
     (cons {:number current :index index}
           (lazy-seq (triangle-numbers (inc index) current))))))

(defn- divides? [number div]
  (= (mod number div) 0))

(defn- isPrime? [number primes]
  (let [limit (inc (bigint (sqrt number)))
        divisors-count (count (->> primes
                                   (filter #(< % limit))
                                   (map #(divides? number %))
                                   (filter true?)))]
    (zero? divisors-count)
    ))

(defn- primes
  ([count]
   (loop [previous 3
          generated [2 3]
          iteration 2]
     (if (>= iteration count)
       generated
       (let [current (+ previous 2)
             isPrime? (isPrime? current generated)]
         (if isPrime?
           (recur current (conj generated current) (inc iteration))
           (recur current generated (inc iteration)))))
     )
   ))

(defn- divides-count [number divisor]
  (loop [number number
         count 0]
    (if (divides? number divisor)
      (recur (/ number divisor) (inc count))
      count)))

(defn- factorize [number primes]
  (loop [number number
         prime-index 0
         factors (transient [])]
    (if (or
         (>= prime-index (count primes))
         (= number 1))
      (persistent! factors)
      (let [current-prime (nth primes prime-index)]
        (if (divides? number current-prime)
          (recur
           (/ number current-prime)
           prime-index
           (conj! factors current-prime))
          (recur
           number
           (inc prime-index)
           factors))))))

(defn- subsets [set]
  (if (empty? set) []
    (concat (subsets (drop 1 set))
            (map #(conj % (first set)) (subsets (drop 1 set)))
            [[(first set)]])))


(defn- with-cache [cache key fn]
  (let [cached-result (.get cache key)]
    (if (nil? cached-result)
      (let [computed-result (fn)]
        (do
          (.put cache key computed-result)
          computed-result))
      cached-result)))

(defn- divisors-count
  ([number primes cache]
   (with-cache cache number
     #(-> number
          (factorize primes)
          (subsets)
          (distinct)
          (count)
          (+ 1)))))

(defn- triangle-number-divisors-count [index primes divisors-cache]
  (if (even? index)
    (* (divisors-count (/ index 2) primes divisors-cache)
       (divisors-count (inc index) primes divisors-cache))

    (* (divisors-count (/ (inc index) 2) primes divisors-cache)
       (divisors-count index primes divisors-cache))
    ))

(defn task [n]
  (let [n n
        divisors-cache (java.util.HashMap.)
        primes (primes (double n))]
    (first
     (->>
      (triangle-numbers)
      (map #(identity {:number (:number %)
                       :count (triangle-number-divisors-count (:index %) primes divisors-cache)
                       :index (:index %)}))
      (filter #(> (:count %) n))
      ))))

(time (task 500))
