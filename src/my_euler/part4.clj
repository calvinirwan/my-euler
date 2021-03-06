(ns my-euler.part4)
;;problem32
(defn apalah [x1 x2]
  (str x1 x2 (* x1 x2)))

;;problem33
(defn twin-num?
  [num]
  (= (quot num 10) (rem num 10)))

(defn proper-num
  [num]
  (remove #(or (zero? (rem % 10)) (twin-num? %)) (range 11 num)))

(defn cancelling-fractions?
  [num1 num2]
  (let [div1 (/ num1 num2)
        x1 (quot num1 10)
        x2 (rem num1 10)
        y1 (quot num2 10)
        y2 (rem num2 10)]
    (cond
     (= x1 y1) (= div1 (/ x2 y2))
     (= x1 y2) (= div1 (/ x2 y1))
     (= x2 y1) (= div1 (/ x1 y2))
     (= x2 y2) (= div1 (/ x1 y1)))))

(defn p33
  [n]
  (for [x (proper-num n)
        y (proper-num n) :while (< y x)]
    (if (cancelling-fractions? y x)
      (/ y x)
      0)))

#_(reduce * (filter #(not (zero? %)) (p33 100)))

;;problem34
(defn factorial [n]
  (if (zero? n)
    1
    (if (= n 1)
      1
      (*' n (factorial (- n 1))))))

(defn digit-factorial [n]
  (if (zero? (quot n 10))
    (factorial (rem n 10))
    (+ (factorial (rem n 10)) (digit-factorial (quot n 10)))))

(defn p34 [n]
  (filter #(= % (digit-factorial %)) (range 10 (inc n))))

;;problem36
(defn palin?
  [snum]
  (let [seq-snum (seq snum)]
    (if (= seq-snum (reverse seq-snum))
      snum)))

(defn dec->bin
  [num]
  (if (zero? (quot num 2))
    (conj [] (rem num 2))
    (conj (dec->bin (quot num 2)) (rem num 2))))

(defn double-palin?
  [num]
  (let [snum (str num)
        sbin (apply str (dec->bin num))]
    (if (and (palin? snum) (palin? sbin))
      num)))

80 / 2 40 rem 0
40 / 2 20 rem 0
20 / 2 10 rem 0
10 / 2 5 rem 0
5 / 2 2 rem 1
2 / 2 1 rem 0
1 / 2 0 rem 1
