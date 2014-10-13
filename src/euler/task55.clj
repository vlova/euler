(ns euler.task55)

(use 'euler.common)

(defn reversedNumber [number]
  (bigint (apply str (reverse (str number)))))

(defn canProducePalindrome? [number]
  (let [number (bigint number)]
    (loop [number (+ number (reversedNumber number))
         iteration 1]
    (if (> iteration 60)
      false
      (if (isPalindrome? number)
        true
        (recur (+ number (reversedNumber number))
               (inc iteration)))))))

(defn isLychrel? [number]
  (not (canProducePalindrome? number)))

(defn task []
  (as->
   (range 10001) $
   (filter isLychrel? $)
   (count $)))

(future (println "answer:" (task)))
