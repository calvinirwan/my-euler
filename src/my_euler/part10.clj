(ns my-euler.part10)

;;problem92
(defn next-chain
  [num]
  (let [sisa (quot num 10)
        nomor (rem num 10)]
      (if (zero? sisa)
    (* nomor nomor)
    (+ (* nomor nomor) (next-chain sisa)))))

(defn chain89?
  [num]
  (if (= 89 num)
    1
    (if (= 1 num)
      0
      (chain89? (next-chain num)))))

(def p92 (reduce #(+ %1 (chain89? %2)) 0 (range 1 10000000)))
