(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as s]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def invalid-states [#{:fox :goose}
                     #{:fox :goose nil}
                     #{:goose :corn}
                     #{:goose :corn nil}])

(defn possible-picks [items last-picked]
  (let [set-items (s/difference (s/union (set items) #{nil}) #{:you})]
    (->> (vec set-items)
         (map (fn [item] (s/difference set-items #{item})))
         (filter (fn [set] (not-any? #(= set %) invalid-states)))
         (map #(first (s/difference set-items %)))
         (remove #(= last-picked %)))))

(defn update-from [from item]
  (let [change (if (nil? item) #{:you} #{item :you})]
    (vec (s/difference (set from) change))))

(defn update-dest [dest item]
  (let [change (if (nil? item) [:you] [:you item])]
    (vec (concat dest change))))

(defn update-boat [item]
  (if (nil? item) [:boat :you] [:boat :you item]))

(defn move-right [left right item]
  (let [left (update-from left item)]
    [[left (update-boat item) right]
     [left [:boat] (update-dest right item)]]))

(defn move-left [left right item]
  (let [right (update-from right item)]
    [[left (update-boat item) right]
     [(update-dest left item) [:boat] right]]))

(defn gen-moves [moves n target item]
  (let [[left _ right] (last moves)]
    (cond
      (and (empty? left) (= target :left)) moves

      (= n 0) false

      (= target :right)
      (let [item (first (remove nil? (possible-picks left item)))] ; always pick item from left
        (concat moves
                (gen-moves (move-right left right item) (dec n) :left item)))

      (= target :left)
      (let [possible-picks (possible-picks right item)
            possible-moves (fn [item] (gen-moves
                                       (move-left left right item)
                                       (dec n)
                                       :right
                                       item))]
        (concat moves
                (first (filter coll? (map possible-moves possible-picks))))))))

(def static-solution [[[:fox :goose :corn :you] [:boat] []]
                      [[:fox :corn] [:boat :you :goose] []]
                      [[:fox :corn] [:boat] [:you :goose]]
                      [[:fox :corn] [:boat :you] [:goose]]
                      [[:fox :corn :you] [:boat] [:goose]]
                      [[:corn] [:boat :you :fox] [:goose]]
                      [[:corn] [:boat] [:you :fox :goose]]
                      [[:corn] [:boat :you :goose] [:fox]]
                      [[:you :goose :corn] [:boat] [:fox]]
                      [[:goose] [:boat :you :corn] [:fox]]
                      [[:goose] [:boat] [:you :corn :fox]]
                      [[:goose] [:boat :you] [:corn :fox]]
                      [[:goose :you] [:boat] [:corn :fox]]
                      [[] [:boat :goose :you] [:corn :fox]]
                      [[] [:boat] [:corn :fox :goose :you]]])

(defn river-crossing-plan []
  (-> (gen-moves start-pos 10 :right nil)
      (vec)))
