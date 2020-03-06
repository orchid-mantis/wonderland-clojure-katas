(ns tiny-maze.solver)

(defn next-moves [m [x y]]
  (->> [[(dec x) y]
        [x (dec y)]
        [(inc x) y]
        [x (inc y)]]
       (filter #(or (= (get-in m %) 0) (= (get-in m %) :E)))))

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
