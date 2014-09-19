(ns euler.task12)

(use 'clojure.math.numeric-tower)
(use 'euler.primes)

(defn- triangle-number [i]
  {:doc "return i-th triangle number"}

  {:number (/ (* i (inc i)) 2)
   :index i})

(defn- divide-times [number divisor]
  {:doc "determine how many times number can be divided using given divisor"}

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
          (let [divide-times (divide-times number current-prime)]
            (recur
             (/ number (expt current-prime divide-times))
             (inc prime-index)
             (conj! factors { :prime current-prime :times divide-times })))
          (recur
           number
           (inc prime-index)
           factors))))))

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
   (let [factors (factorize number primes)]
         (reduce * (map #(inc (:times %)) factors)))))

(defn- triangle-number-divisors-count [index primes divisors-cache]
  (let [a (if (even? index) (/ index 2) index)
        b (if (even? index) (inc index) (/ (inc index) 2))]
    (* (divisors-count a primes divisors-cache)
       (divisors-count b primes divisors-cache))))

(defn- optimal-primes-count [divisors-count]
  {:doc "determines optimal primes count for finding triangle number with specified divisors count"}

  (-> divisors-count
      (exact-integer-sqrt)
      (first)
      (inc)))

(defn task [divisors-count]
  (let [divisors-cache (java.util.HashMap.)
        primes (primes (optimal-primes-count divisors-count))]
    (loop [i 1]
      (let [number (triangle-number i)
            number (merge number {:count
                                  (triangle-number-divisors-count (:index number) primes divisors-cache)})]
        (if (> (:count number) divisors-count)
          number
          (recur (inc i)))
        ))))

;(time (task 500))
