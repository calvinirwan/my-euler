(ns my-euler.part7)

;;problem63
(defn powerful-digit
  [x y]
  (if (= (count (str (power x y))) y)
    1
    0))

(defn p63
  [x y]
  (if (= x 9)
    (if (= y 21)
      (powerful-digit x y)
      (+ (powerful-digit x y) (p63 1 (inc y))))
    (+ (powerful-digit x y) (p63 (inc x) y))))

;;problem67
(def take-p67 (str/split-lines (slurp "resources/p67.txt" )))
(def split-p67 (map #(str/split % #" ") take-p67))
(def finale-p67 (mapv (fn [b] (mapv #(Integer/valueOf %) b)) split-p67))

(defn p67
  [x y coll]
  (if (get-in coll [(inc x) y])
    (+ (get-in coll [x y])
       (max (p67 (inc x) y coll)
            (p67 (inc x) (inc y) coll)))
    (get-in coll [x y])))

(def p67
  (memoize
   (fn [x y coll]
     (if (get-in coll [(inc x) y])
       (+ (get-in coll [x y])
          (max (tiger (inc x) y coll)
               (tiger (inc x) (inc y) coll)))
       (get-in coll [x y])))))
