(ns euler.task4)

(use 'clojure.math.numeric-tower)

(defn- isPalindrome? [number]
  {:doc "checks whether the number is palindrome" }

  (let [digits (seq (.toString number))]
    (= digits (reverse digits))))

(defn- divides? [number div]
  (= (mod number div)))

(defn- find-largest-near-divider [number div]
  (- number (mod number div)))

(defn find-largest-multiply-palindrom
  {:doc "finds the largest palindrome made from the product of two numbers"}

  ([digits]
   {:pre [(> digits 1)]}

   (let [startLimit (expt 10 (dec digits))
         endLimit (dec (expt 10 digits))]
     (find-largest-multiply-palindrom startLimit endLimit)))

  ([startLimit endLimit]
   {:pre [(number? startLimit)
          (number? endLimit)
          (>= startLimit 10)
          (pos? endLimit)
          (<= startLimit endLimit)]}

   ; It is easy to prove that if startLimit > 10 then palindrome must be divisible by 11.
   ; Cuz 11 is prime number, then one of factors must be divisible by 11.
   ; All stuff around i-mod-11 is written because `mod` works slow.
   (loop [i endLimit
          j (if (divides? endLimit 11)
              endLimit
              (find-largest-near-divider endLimit 11))
          maxPalindrome startLimit
          i-mod-11 (mod i 11)]
     (let [next-i (dec i)
           next-i-mod-11 (if (= i-mod-11 0) 10 (dec i-mod-11))
           next-start-j (if (= next-i-mod-11 0) next-i (- next-i next-i-mod-11))
           next-j (- j (if (= i-mod-11 0) 1 11))]
       (cond (< i startLimit) maxPalindrome
             (< j startLimit) (recur next-i next-start-j maxPalindrome next-i-mod-11)
             (< (* i i) maxPalindrome) maxPalindrome
             (< (* i j) maxPalindrome) (recur next-i next-start-j maxPalindrome next-i-mod-11)
             :else (let [multed (* i j)]
                     (if (isPalindrome? multed)
                       (recur i next-j (max multed maxPalindrome) i-mod-11)
                       (recur i next-j maxPalindrome i-mod-11))))))))

(time (find-largest-multiply-palindrom 2))
