(ns my-euler.part3)

;;problem22
#_(def a (sort (map #(str/replace % "\"" "")(str/split (slurp "resources/p22.txt" ) #","))))
#_(fn [name] (map #(- (int %) 64) name))
#_(def b (map (fn [name] (reduce + (map #(- (int %) 64) name))) a))
#_(defn p22 [] (reduce + (map #(* %1 %2) b (range 1 (inc (count b))))))

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
