(ns wonderland-number.finder)

(defn sameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn wonderland-number []
  (let [wondernum? #(and (sameDigits? (* 2 %) %)
                         (sameDigits? (* 3 %) %)
                         (sameDigits? (* 4 %) %)
                         (sameDigits? (* 5 %) %)
                         (sameDigits? (* 6 %) %))]
    (->> (range 100000 1000000)
         (filter wondernum?)
         (first))))
