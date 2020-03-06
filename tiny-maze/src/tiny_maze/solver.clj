(ns tiny-maze.solver)

(defn next-moves [maze [x y]]
  (let [neighbours [[(dec x) y]
                    [x (dec y)]
                    [(inc x) y]
                    [x (inc y)]]]
    (->> neighbours
         (filter #(or (= (get-in maze %) 0) (= (get-in maze %) :E))))))

(defn gen-path [maze path]
  (let [[x y] (last path)]
    (cond
      (= :E (get-in maze [x y])) (conj path [x y])
      :else (->> (next-moves maze [x y])
                 (filter (fn [move] (not-any? #(= move %) path)))
                 (map (fn [move] (gen-path maze (conj path move))))
                 (filter some?)
                 (first)))))

(defn solve-maze [maze]
  (let [path (gen-path maze [[0 0]])]
    (reduce #(assoc-in %1 %2 :x) maze path)))
