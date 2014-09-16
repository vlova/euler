(ns euler.task4)

(use 'clojure.math.numeric-tower)

(defn- isPalindrome? [number]
  {:doc "checks whether the number is palindrome" }

  (let [digits (seq (.toString number))]
    (= digits (reverse digits))))

(defn find-largest-multiply-palindrom
  {:doc "finds the largest palindrome made from the product of two numbers"}

  ([digits]
   {:pre [(pos? digits)]}

   (let [startLimit (bigint (expt 10 (dec digits)))
         endLimit (bigint (dec (expt 10 digits)))]
     (find-largest-multiply-palindrom startLimit endLimit)))

  ([startLimit endLimit]
   {:pre [(number? startLimit)
          (number? endLimit)
          (pos? startLimit)
          (pos? endLimit)
          (<= startLimit endLimit)]}

   (loop [i endLimit
          j endLimit
          maxPalindrome (bigint startLimit)]
     (let [next-i (dec i)
           next-j next-i]
       (cond (< i startLimit) maxPalindrome
             (< j startLimit) (recur next-i next-j maxPalindrome)
             (< (* i i) maxPalindrome) maxPalindrome
             (< (* i j) maxPalindrome) (recur next-i next-j maxPalindrome)
             :else (let [multed (* i j)]
                     (if (isPalindrome? multed)
                       (recur i (dec j) (max multed maxPalindrome))
                       (recur i (dec j) maxPalindrome))))))))

(time (find-largest-multiply-palindrom 3))
