(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(def shuffled-decks
  (partition 26 (shuffle cards)))

(def player1-cards (first shuffled-decks))
(def player2-cards (second shuffled-decks))

(def ranks-int (->> (map-indexed (fn [i r] {r i}) ranks)
                    (reduce #(merge %1 %2))))

(defn rank-val [[_ r]]
  (get ranks-int r))

(defn play-round [[p1-first & p1-rest] [p2-first & p2-rest] prize]
  (let [p1-rank (rank-val p1-first)
        p2-rank (rank-val p2-first)
        final-prize (shuffle (concat prize [p1-first p2-first]))]
    (cond
      (or (nil? p1-first) (nil? p2-first)) []
      (> p1-rank p2-rank) [[[:p1-won p1-first p2-first]
                            (concat p1-rest final-prize)
                            p2-rest]]

      (< p1-rank p2-rank) [[[:p2-won p1-first p2-first]
                            p1-rest
                            (concat p2-rest final-prize)]]

      :else (let [p1-deck (drop 3 p1-rest)
                  p2-deck (drop 3 p2-rest)
                  prize (concat prize
                                [p1-first p2-first]
                                (take 3 p1-rest)
                                (take 3 p2-rest))]
              (concat [[[:tie p1-first p2-first] p1-deck p2-deck]]
                      (play-round p1-deck p2-deck prize))))))

(defn game-loop [steps]
  (let [step (last steps)
        [_ p1-deck p2-deck] step]
    (cond
      ;(= (count steps) 10) steps
      (and (empty? p1-deck) (empty? p2-deck)) {:game-result :tie :steps steps}
      (empty? p1-deck) {:game-result :player2-won :steps steps}
      (empty? p2-deck) {:game-result :player1-won :steps steps}
      :else (recur (concat steps (play-round p1-deck p2-deck []))))
    ))

(defn play-game [p1-deck p2-deck]
  (game-loop [[[] p1-deck p2-deck]]))
