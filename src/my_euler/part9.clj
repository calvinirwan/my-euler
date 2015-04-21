(ns my-euler.part9)
;;problem 81
(def take-p81 (str/split-lines (slurp "resources/p81.txt" )))
(def split-p81 (map #(str/split % #",") take-p81))
(def finale-p81 (mapv (fn [b] (mapv #(Integer/valueOf %) b)) split-p81))



(defn p81
  [x y coll]
  (if (= (inc x) 80)
    (if (= (inc y) 80)
      (get-in coll [x y])
      (+ (get-in coll [x y])
         (p81 x (inc y) coll)))
    (if (= (inc y) 80)
      (+ (get-in coll [x y])
         (p81 (inc x) y coll))
      (+ (get-in coll [x y])
         (min (p81 (inc x) y coll) (p81 x (inc y) coll))))))

(def p81
  (memoize
   (fn [x y coll]
     (if (= (inc x) 80)
       (if (= (inc y) 80)
         (get-in coll [x y])
         (+ (get-in coll [x y])
            (p81 x (inc y) coll)))
       (if (= (inc y) 80)
         (+ (get-in coll [x y])
            (p81 (inc x) y coll))
         (+ (get-in coll [x y])
            (min (p81 (inc x) y coll) (p81 x (inc y) coll))))))))
