(ns euler.task31)

(defn permutable? [number]
  (as->
   number $
   (map (partial * $) [2 3 4 5 6])
   (map str $)
   (map sort $)
   (map (partial = (sort (str number))) $)
   (filter false? $)
   (zero? (count $))))

(future
  (as->
   (range) $
   (drop 1 $)
   (filter permutable? $)
   (first $)
   (println "answer:" $)))
