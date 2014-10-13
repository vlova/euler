(ns euler.task18)

(defn to-triangle [vector]
  (loop [count 1
         to-process vector
         result (transient [])]
    (if (empty? to-process)
      (persistent! result)
      (recur (inc count)
             (drop count to-process)
             (conj! result (take count to-process))))))

(defn transform-row [prev-row next-row]
  (map max
       (map + (concat prev-row [0]) next-row)
       (map + (concat [0] prev-row) next-row)))

(defn sum-array [array]
  (loop [row 1
         transformed [(first array)]]
    (if (> (inc row) (count array))
      transformed
      (recur (inc row)
             (conj transformed
                   (transform-row (last transformed)
                                  (nth array row)))))))

(defn find-max [array]
  (apply max (map #(apply max %) array)))

(defn find-max-total [array]
  (find-max (sum-array array)))

(find-max-total
 (to-triangle
  [75
   95 64
   17 47 82
   18 35 87 10
   20  4 82 47 65
   19  1 23 75  3 34
   88  2 77 73  7 63 67
   99 65  4 28  6 16 70 92
   41 41 26 56 83 40 80 70 33
   41 48 72 33 47 32 37 16 94 29
   53 71 44 65 25 43 91 52 97 51 14
   70 11 33 28 77 73 17 78 39 68 17 57
   91 71 52 38 17 14 91 43 58 50 27 29 48
   63 66  4 68 89 53 67 30 73 16 69 87 40 31
   4 62 98 27 23  9 70 98 73 93 38 53 60  4 23])
 )
