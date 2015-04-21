(ns my-euler.part1)
;;euler part 1, problem 1 - 50

;;problem 1
(defn prime? [num]
  (if (or (= num 2) (= num 3))
    true
    (not (some zero? (map #(mod num %) (range 3 num))))))

(defn primes-to
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (let [refs (boolean-array (+ lim 1) true)
        root (int (Math/sqrt lim))]
    (do (doseq [i (range 2 lim)
                :while (<= i root)
                :when (aget refs i)]
          (doseq [j (range (* i i) lim i)]
            (aset refs j false)))
        (filter #(aget refs %) (range 2 lim)))))

(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (letfn [(nxtprm [cs] ; current candidates
            (let [p (first cs)]
              (if (> p (Math/sqrt n)) cs
                (cons p (lazy-seq (nxtprm (-> (range (* p p) (inc n) p)
                                              set (remove cs) rest)))))))]
    (nxtprm (range 2 (inc n)))))

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
#_(str/replace (slurp "resources/p8.txt" ) #"\r\n" "")
(defn recurtake [coll] (if (= (count coll) 13)
                         (conj [] (map #(- (int %) 48) (take 13 coll)))
                         (conj (recurtake (rest coll)) (map #(- (int %) 48) (take 13 coll)) )))
(defn filterpos [coll] (filter #(every? pos? %) coll))
(defn p8 [coll] (apply max (map #(apply * %) (filterpos (recurtake coll)))))

;;problem 9
(defn sptriplet? [a b]
  (== 1000 (+ a b (Math/sqrt (+ (* a a) (* b b))))))

(defn multipal [x1 x2]
  (keep #(if (not (nil? %)) %)
        (for [x (range x1 x2)
              y (range x1 x2) :while (<= y x)]
          (if (sptriplet? x y) (* x y (Math/sqrt (+ (* x x) (* y y))))))))

;;problem 10
(defn p10 [n] (apply + (primes-to 1000000)))

(defn greverse
  [[x & xs]]
  (if xs
    (conj (greverse xs) x)
    (conj [] x)))

(defn greverse
  [coll]
  (if (empty? (butlast coll))
    (cons (last coll) [])
    (cons (last coll) (greverse (butlast coll)))))
