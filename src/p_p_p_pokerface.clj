(ns p-p-p-pokerface)

(defn rank [card]
  (let [[first _] card]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (let [h {\T 10, \J 11, \Q 12, \K 13, \A 14}]
        (Integer/valueOf(str (get h first)))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2]))
    (= (sort (vals (frequencies (map rank hand)))) (seq [1 4]))))

(defn straight? [hand]
  (let [min (apply min (map rank hand))
        max (apply max (map rank hand))]
        (if (> min 9)
          (= (sort (map rank hand)) (range min (+ 1 max)))
          (if (and (= min 2) (= max 14))
            (= (sort (replace {14 1} (map rank hand))) (range 1 (+ 1 5)))
            (= (sort (map rank hand)) (range min (+ 1 max)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [checker (fn [x] (if ((first x) hand) (second x) 0))]
      (apply max (map checker checkers)))))
