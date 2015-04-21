(ns my-euler.part3)
;;problem21
(defn fac
  [num]
  (let [a (filter #(zero? (rem num %)) (range 1 (Math/sqrt num )))
        b (map #(quot num %) (reverse a))]
    (butlast (concat a b))))

(defn amicable?
  [num]
  (let [num2 (reduce + (fac num))
        num3 (reduce + (fac num2))]
    (if (not= num num2)
      (= num num3))))

;;problem22
#_(def a (sort (map #(str/replace % "\"" "")(str/split (slurp "resources/p22.txt" ) #","))))
#_(fn [name] (map #(- (int %) 64) name))
#_(def b (map (fn [name] (reduce + (map #(- (int %) 64) name))) a))
#_(defn p22 [] (reduce + (map #(* %1 %2) b (range 1 (inc (count b))))))

;;problem23

(defn fac
  [num]
  (if (= num 1)
    '(1)
    (if (= num 2)
      '(1)
      (filter #(zero? (rem num %)) (range 1 (inc (Math/sqrt num )))))))

(defn factoring
  [num]
  (let [factor (fac num)]
    (distinct (concat factor (keep #(if (not= (quot num %) %) (quot num %) ) (rest factor))))))

(defn abundant?
  [num]
  (> (reduce + (factoring num)) num))

(defn sum-abundant
  [coll]
  (for [x coll
        y coll :while (<= y x)]
    (if (<= (+ x y) 28123)
      (+ x y))))

(def a (filter abundant? (range 1 28124)))
(def b (distinct (keep identity (sum-abundant a))))
(def c (reduce + b))
(def d (reduce + (range 1 28124)))

;;problem26

(defn count-fractions
  [num]
  [num (count (loop [atas 10
                     bawah num
                     sisa []
                     digit []]
                (if ((set sisa) (rem atas bawah))
                  digit
                  (if (< atas bawah)
                    (recur (* atas 10) bawah sisa (conj digit 0))
                    (recur (* 10 (rem atas bawah)) bawah (conj sisa (rem atas bawah)) (conj digit (quot atas bawah)))))))])

(def unit-fractions (map count-fractions (range 1 1001)))
(def p26 (reduce (fn [a b] (if (> (second a) (second b)) a b)) unit-fractions) )

;;problem28
;;(range 2 1001 2)
(defn p28 [coll] (inc (reduce + (map #(+ 4
                                         (* % (dec %))
                                         (* % %)
                                         (* % (inc %))
                                         (* % (+ 2 %))) coll))))

;;problem29
;;(range 2 101)
(defn power [num pow] (apply *' (repeat pow num)))
(defn p29 [x1 x2]
  (count (set (for [x (range x1 x2)
        y (range x1 x2)]
    (power x y)))))

;;problem30
(defn num->digit
  [num]
  (if (zero? (quot num 10))
    (conj [] num)
    (conj (num->digit (quot num 10)) (mod num 10))))

(defn fifthpow?
  [num]
  (= num (reduce + (map #(power % 5) (num->digit num)))))
