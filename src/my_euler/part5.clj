(ns my-euler.part5)
;;problem42
(def take-p42 (str/split (slurp "resources/p42.txt") #","))
(def finale-p42 (map #(str/replace % #"\"" "") take-p42))

(defn triangle-num
  [n]
  (reduce + (range 1 (inc n))))

(defn word-value
  [word]
  (reduce + (map #(- (int %) 64) (seq word))))

(def word-needed (map word-value p42))
(def triangle-needed (map triangle-num (range 1 21)))
(defn p42 [] (filter (set triangle-needed) word-needed))

;;problem47
;;problem 48
;;masukkin num 1001
(defn p48 [num]
  (reduce + (map #(apply *' (repeat % %)) (range 1 num))))
