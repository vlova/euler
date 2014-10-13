(ns euler.task49)

(use 'clojure.math.numeric-tower)
(use 'euler.common)

(def collatz-cache (java.util.HashMap.))

(defn- collatz [n]
  (with-cache collatz-cache n
    (fn []
      (if (= n 1)
        [{:number 1
          :count 1}]

        (let [rest (if (even? n)
                     (collatz (/ n 2))
                     (collatz (inc (* 3 n))))
              rest-count (:count (first rest))]
          (cons {:number n
                 :count (inc rest-count)}
                rest))))))

(defn find-max [f coll]
  (reduce (fn [a b] (if (>= (f a) (f b)) a b))
          {:count 1 :number 1}
          coll))

(defn task []
  (find-max :count
           (map (comp first collatz)
                (range 1 1000001))))

(time (task))
