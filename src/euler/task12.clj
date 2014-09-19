(ns euler.task12)

(use 'clojure.math.numeric-tower)

(defn- triangle-numbers
  ([]
   (triangle-numbers 1 0))

  ([index previous]
   (let [current (+ previous index)]
     (cons current
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
          (let [count (divides-count number current-prime)]
            (recur
             (/ number (expt current-prime count))
             (inc prime-index)
             (conj! factors { :prime current-prime :count count })))
          (recur
           number
           (inc prime-index)
           factors))))))

(defn- divisors-count
  ([number primes]
   (let [factors (factorize number primes)]
     (reduce * (map #(inc (:count %)) factors))
   )))

(defn task [n]
  (let [n n
        primes (primes (double n))]
  (first
     (->>
      (triangle-numbers)
      (map #(identity {:number %
                       :count (divisors-count % primes)}))
      (filter #(>= (:count %) n))
      ))))

(time (task 500))
