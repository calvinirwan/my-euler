(ns my-euler.part1)
;;euler part 1, problem 1 - 50

;;problem 1
(defn prime? [num]
  (if (or (= num 2) (= num 3))
    true
    (not (some zero? (map #(mod num %) (range 3 num))))))

;;problem 2
;;problem 3
#_(filter prime? (filter #(zero? (rem 600851475143 %)) (range 3 775146 2)))

;;problem 4
(defn palin? [x]
  (let [num (str x)]
    (= (reverse num) (seq num))))

(defn multipal [x1 x2]
  (apply max
         (filter palin?
                 (for [x (range x1 x2)
                       y (range x1 x2) :while (<= y x)]
                   (* x y)))))

;;problem 5
(defn clm? [num] (zero? (reduce + (map #(mod num %) (range 11 21)))))
(defn clm? [num & a] (every? identity (map #(zero? (mod num %)) a)))
(defn clm? [num] (and (zero? (mod num 11))
                      (zero? (mod num 12))
                      (zero? (mod num 13))
                      (zero? (mod num 14))
                      (zero? (mod num 15))
                      (zero? (mod num 16))
                      (zero? (mod num 17))
                      (zero? (mod num 18))
                      (zero? (mod num 19))
                      (zero? (mod num 20))))

(defn clm? [num] (if (zero? (mod num 20))
                   (if (zero? (mod num 19))
                     (if (zero? (mod num 18))
                       (if (zero? (mod num 17))
                         (if (zero? (mod num 16))
                           (if (zero? (mod num 15))
                             (if (zero? (mod num 14))
                               (if (zero? (mod num 13))
                                 (if (zero? (mod num 12))
                                   (zero? (mod num 11))
                                   ))))))))))


#_(take 1 (filter #(clm? %) (iterate #(+ 20 %) 20)))
#_(fn [& fac](take 1 (filter (fn [num] (every? identity (map #(zero? (mod num %)) fac))) (iterate inc 1))))

;;problem 6
(defn square [x] (* x x))
#_(square (reduce + (range 1 101)))
#_(reduce + (map #(square %) (range 1 101)))

;;problem 7
;;problem 8
;;problem 9
;;problem 10
;;problem 11
;;problem 12
(defn triangle-num [])

;;problem 13
;;problem 14
;;problem 15
;;problem 16
;;problem 17
;;problem 18
;;problem 19
;;problem 20
