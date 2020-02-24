(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as s]))

(def start-pos [[#{:fox :goose :corn :you} #{:boat} #{}]])
(def end-pos [#{} #{:boat} #{:fox :goose :corn :you}])

(def invalid-states [#{:fox :goose} #{:goose :corn}])

(def possible-picks [:fox :goose :corn nil])

(defn next-move [last-move item]
  (let [[left boat right] last-move
        to-move (if (nil? item) #{:you} #{:you item})]
    (cond
      (s/subset? to-move left) [[(s/difference left to-move) (s/union boat to-move) right]
                                [(s/difference left to-move) #{:boat} (s/union right to-move)]]

      (s/subset? to-move right) [[left (s/union boat to-move) (s/difference right to-move)]
                                 [(s/union left to-move) #{:boat} (s/difference right to-move)]]
      :else nil)))

(defn valid? [[_ [rl _ rr]]]
  (let [valid? (fn [set] (not-any? #(= set %) invalid-states))]
    (and (valid? rl) (valid? rr))))

(defn useful? [[[_ _ lr] [_ _ rr]]]
  (cond
    (some? (:you rr)) (not= (count lr) (count (s/difference rr #{:you})))
    :else true))

(defn gen-moves [moves]
  (let [last-move (last moves)]
    (cond
      (= end-pos last-move) moves
      :else (->> possible-picks
                 (map #(next-move last-move %))
                 (filter (fn [next] (and (some? next) (valid? next) (useful? next)
                                         (not-any? #(= (last next) %) moves)))) ; dont expand duplicit moves
                 (map #(gen-moves (vec (concat moves %))))
                 (filter some?)
                 (first)))))

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
  (gen-moves start-pos))
