(ns tiny-maze.solver)

(defn next-moves [m [x y]]
  (let [left-pos  [(dec x) y]
        upper-pos [x (dec y)]
        right-pos [(inc x) y]
        lower-pos [x (inc y)]
        neighbours [[left-pos (get-in m left-pos)]
                    [upper-pos (get-in m upper-pos)]
                    [right-pos (get-in m right-pos)]
                    [lower-pos (get-in m lower-pos)]]]
    (->> neighbours
         (filter (fn [[_ val]] (or (= val 0) (= val :E))))
         (map first))))

(defn gen-path [m path [x y]]
  (cond
    (= :E (get-in m [x y])) (conj path [x y])
    :else (->> (next-moves m [x y])
               (filter (fn [next] (not-any? #(= next %) path)))
               (map (fn [next] (gen-path m (conj path [x y]) next)))
               (filter some?)
               (first))))

(defn solve-maze [maze]
  (let [path (gen-path maze [] [0 0])]
    (reduce #(assoc-in %1 %2 :x) maze path)))
