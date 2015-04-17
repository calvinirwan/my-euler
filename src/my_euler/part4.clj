(ns my-euler.part4)
;;problem 48
;;masukkin num 1001
(defn p48 [num]
  (reduce + (map #(apply *' (repeat % %)) (range 1 num))))

