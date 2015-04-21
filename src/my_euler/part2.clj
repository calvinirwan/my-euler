(ns my-euler.part2)
;;problem 11
;;seems like i'm doing this stupidly

(def take-p11 (str/split-lines (slurp "resources/p11.txt" )))
(def split-p11 (map #(str/split % #" ") take-p11))
(def finale-p11 (mapv (fn [b] (mapv #(Integer/valueOf %) b)) split-p11))

(defn horizontal
  [coll]
  (if (<= 4 (count coll))
    (reduce * (take 4 coll))
    (max (reduce * (take 4 coll)) (horizontal (rest coll)))))

(defn finale1 [] (apply max (map horizontal finale-p11)))

(defn take-vertical
  [x y coll]
  (reduce * (map #(get-in coll [(+ x %) y]) (range 4))))

(defn vertical
  [x y coll]
  (if (= 16 x)
    (if (= 19 y)
      (take-vertical x y coll)
      (max (take-vertical x y coll)
           (vertical 0 (inc y) coll)))
    (max (take-vertical x y coll)
         (vertical (inc x) y coll))))

(defn finale2 [] (vertical 0 0 finale-p11))

(defn take-diagonal1
  [x y coll]
  (reduce * (map #(get-in coll [(+ x %) (+ y %)]) (range 4))))

(defn diagonal1
  [x y coll]
  (if (= y 16)
    (if (zero? x)
      (take-diagonal1 x y coll)
      (max (take-diagonal1 x y coll)
           (diagonal1 0 (- y (dec x)) coll)))
    (if (= x 16)
      (max (take-diagonal1 x y coll)
           (diagonal1 (- x (inc y)) 0 coll))
      (max (take-diagonal1 x y coll)
           (diagonal1 (inc x) (inc y) coll)))))

(defn finale3 [] (diagonal1 0 0 finale-p11))

(defn take-diagonal2
  [x y coll]
  (reduce * (map #(get-in coll [(+ x %) (- y %)]) (range 4))))

(defn diagonal2
  [x y coll]
  (if (= y 3)
    (if (zero? x)
      (take-diagonal2 x y coll)
      (max (take-diagonal2 x y coll)
           (diagonal2 0 (+ y (dec x)) coll)))
    (if (= x 16)
      (max (take-diagonal2 x y coll)
           (diagonal2 (- x (inc (- 19 y))) 19 coll))
      (max (take-diagonal2 x y coll)
           (diagonal2 (inc x) (dec y) coll)))))

(defn finale4 [] (diagonal2 0 0 finale-p11))

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
;;2 6 20 70 252 924 
(defn pascal-tri
  [n]
  (if (= n 1)
    [1]
    (let [x1 (next (pascal-tri (dec n)))
          x2 (butlast (pascal-tri (dec n)))
          pasqual (map + x1 x2)]
      (flatten [1 pasqual 1]))))

(defn lattice-path
  [left down]
  (if (and (zero? left) (zero? down))
    1
    (if (zero? down)
      (+ 0 (lattice-path (dec left) down))
      (if (zero? left)
        (+ 0 (lattice-path left (dec down)))
        (+ 0 (lattice-path (dec left) down) (lattice-path left (dec down)))))))

(defn lattice-path
  [left down sum]
  (loop [left left
         down down
         sum sum]
      (if (zero? left)
        (if (zero? down)
          (inc sum)
          (recur left (dec down)))
        (+ 0 (recur (dec left) down) (recur left (dec down))))))

;;problem 16
(defn num-adder [num]
  (reduce + (map #(- (int %) 48) num)))

(defn p16 [] (reduce + (map #(- (int %) 48) (seq (str (apply *' (repeat 1000 2)))))))

;;problem 17
(def satuan-dict
  {1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"
   })

(def puluhan-dict
  {2 "twenty"
   3 "thirty"
   4 "forty"
   5 "fifty"
   6 "sixty"
   7 "seventy"
   8 "eighty"
   9 "ninety"
   10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"})

(defn ratusan
  [n]
  (if (zero? (rem n 100))
    (str (get satuan-dict (quot n 100)) "hundred")
    (if (< 9 (rem n 100))
      (str (get satuan-dict (quot n 100)) "hundredand" (puluhan (rem n 100)))
      (str (get satuan-dict (quot n 100)) "hundredand" (get satuan-dict (rem n 100))))))

(defn puluhan
  [n]
  (if (< n 20)
    (get puluhan-dict n)
    (if (zero? (rem n 10))
      (get puluhan-dict (quot n 10))
      (str (get puluhan-dict (quot n 10)) (get satuan-dict (rem n 10))))))

(defn satuan
  [n]
  (get satuan-dict n))

(defn p17
  [n]
  (if (< n 10)
    (satuan n)
    (if (< n 100)
      (puluhan n)
      (if (< n 1000)
        (ratusan n)
        "onethousand"))))

;;problem 18
(def take-p18 (str/split-lines (slurp "resources/p18.txt" )))
(def split-p18 (map #(str/split % #" ") take-p18))
(def finale-p18 (mapv (fn [b] (mapv #(Integer/valueOf %) b)) split-p18))

(defn p18
  [x y coll]
  (if (get-in coll [(inc x) y])
    (+ (get-in coll [x y])
       (max (p18 (inc x) y coll)
            (p18 (inc x) (inc y) coll)))
    (get-in coll [x y])))

;;problem 19
(defn leap-year? [n]
  (if (zero? (rem n 100))
    (zero? (rem n 400))
    (zero? (rem n 4))))

(defn make-date
  [year]
  (if (leap-year? year)
    (concat (range 1 (inc 31))
            (range 1 (inc 29))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31)))
    (concat (range 1 (inc 31))
            (range 1 (inc 28))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31))
            (range 1 (inc 30))
            (range 1 (inc 31)))))

(defn date1-sunday
  [ymin ymax]
  (let [date (mapcat make-date (range ymin (inc ymax)))
        days (cycle (range 1 (inc 7)))]
    (map #(if (and (= %1 1) (= %2 7))
            1
            0) date days)))

(defn p19
  [ymin1 ymax1 ymin2 ymax2]
  (- (reduce + (date1-sunday ymin1 ymax1))
     (reduce + (date1-sunday ymin2 ymax2))))

;;problem 20
;; use num-adder
(def refs (atom []))
(defn p20 []
  (reduce * (range 1 101)))
