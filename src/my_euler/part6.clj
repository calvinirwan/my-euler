(ns my-euler.part6)
;;problem 55
(defn num->coll
  [num]
  (if (zero? (quot num 10))
    (conj [] (rem num 10))
    (conj (num->coll (quot num 10)) (rem num 10))))

(defn coll->num
  [coll]
  (reduce #(+' %1 (*' (power 10 (first %2)) (second %2))) 0 (map-indexed vector coll)))

(defn palin-num?
  [num]
  (let [str-num (str num)
        seq-num (seq str-num)
        seq-rev-num (reverse str-num)]
    (= seq-num seq-rev-num)))

(defn sum-reverse
  [num]
  (let [rev-num (coll->num (num->coll num))]
    (+' num rev-num)))

(def lychrel
  (memoize
   (fn [num cnt]
     (let [sum-rev (sum-reverse num)]
       (if (= cnt 48)
         (if (palin-num? sum-rev)
           0
           1)
         (if (palin-num? sum-rev)
           0
           (lychrel sum-rev (inc cnt))))))))
