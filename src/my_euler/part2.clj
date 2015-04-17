(ns my-euler.part2)
;;problem 11
#_(str/split-lines (slurp "resources/p11.txt" ))
#_(map #(str/split % #" ") a)
#_(mapv (fn [b] (mapv #(Integer/valueOf %) b)) a)

;;problem 12
(defn fac
  [num]
  (count (filter #(zero? (rem num %)) (range 1 (inc (Math/sqrt num ))))))

(defn fac
  [num]
  (filter #(zero? (rem num %)) (range 1 (inc (quot num 2)))))

(defn triangle-num
  [n]
  (reduce + (range 1 (inc n))))

(defn p12
  [num]
  (loop [n num]
    (if (>= (fac (triangle-num n)) 250)
      (triangle-num n)
      (recur (inc n)))))

;;problem 13
(defn large-sum [] (reduce + (map read-string (str/split-lines (slurp "resources/p13.txt") ))))

;;problem 14
(defn p14 [n x]
  (if (= n 1)
    (+ 1 x)
    (if (even? n)
      (p14 (/ n 2) (+ 1 x))
      (p14 (+ (* 3 n) 1) (+ 1 x)))))
#_(apply max (mapv #(p14 % 0) (range 1 1000000) ))
#_(.indexOf a 525)

;;problem 15
;;problem 16
(defn num-adder [num]
  (reduce + (map #(- (int %) 48) num)))

(defn p16 [] (reduce + (map #(- (int %) 48) (seq (str (apply *' (repeat 1000 2)))))))

;;problem 17
;;problem 18
;;problem 19
;;problem 20
;; use num-adder
(def refs (atom []))
(defn p20 []
  (reduce * (range 1 101)))
