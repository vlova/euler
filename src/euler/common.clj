(ns euler.common)

(use 'clojure.math.numeric-tower)

(defn divides? [number div]
  (= (mod number div) 0))

(defn third [coll]
  (nth coll 2))

(defn with-cache [cache key fn]
  (let [cached-result (.get cache key)]
    (if (nil? cached-result)
      (let [computed-result (fn)]
        (do
          (.put cache key computed-result)
          computed-result))
      cached-result)))


(def factorial-cache (java.util.HashMap.))

(defn factorial [n]
  (with-cache factorial-cache n
    (fn [] (reduce * (bigint 1) (take n (iterate inc (bigint 1)))))))

(defn sum [coll]
  (reduce + 0 coll))
