(ns card-game-war.game-test
  (:require [clojure.test :refer [deftest testing is]]
            [card-game-war.game :as g]))

(deftest test-rank-val
  (testing "10s are higher rank than 2s"
    (is (> (g/rank-val [:diamond 10])
           (g/rank-val [:heart 2]))))
  (testing "jacks are higher rank than 10s"
    (is (> (g/rank-val [:diamond :jack])
           (g/rank-val [:heart 10]))))
  (testing "queens are higher rank than jacks"
    (is (> (g/rank-val [:diamond :queen])
           (g/rank-val [:heart :jack]))))
  (testing "kings are higher rank than queens"
    (is (> (g/rank-val [:diamond :king])
           (g/rank-val [:heart :queen]))))
  (testing "aces are higher rank than kings"
    (is (> (g/rank-val [:diamond :ace])
           (g/rank-val [:heart :king])))))

(deftest test-play-round
  (testing "returns [] when one or both players have empty deck"
    (is (= []
           (g/play-round nil nil [])))
    (is (= []
           (g/play-round [[:diamond :ace]] nil []))))

  (testing "the highest rank wins the cards in the round"
    (let [[[result p1-deck p2-deck]] (g/play-round [[:diamond :ace]] [[:heart 10]] [])]
      (is (= [:p1-won [:diamond :ace] [:heart 10]]
             result))
      (is (= #{[:diamond :ace] [:heart 10]}
             (set p1-deck)))
      (is (nil? p2-deck)))

    (let [[[result p1-deck p2-deck]] (g/play-round [[:diamond 5]] [[:heart 10]] [])]
      (is (= [:p2-won [:diamond 5] [:heart 10]]
             result))
      (is (= #{[:diamond 5] [:heart 10]}
             (set p2-deck)))
      (is (nil? p1-deck))))

  (testing "tie scenario where both players run out of cards"
    (let [round (g/play-round [[:diamond :ace]] [[:heart :ace]] [])
          [result p1-deck p2-deck] (first round)]
      (is (= result
             [:tie [:diamond :ace] [:heart :ace]]))
      (is (empty? p1-deck))
      (is (empty? p2-deck)))))

(deftest test-play-game
  (testing "first step should contain initial decks"
    (let [p1-deck '([:c :jack] [:c 2] [:c 10])
          p2-deck '([:c 5] [:c 6] [:c 6])
          {:keys [steps]} (g/play-game p1-deck p2-deck)
          [_ deck1 deck2] (first steps)]
      (is (> (count steps)
             1))
      (is (= deck1
             p1-deck))
      (is (= deck2
             p2-deck))))

  (testing "the player loses when they run out of cards"
    (let [p1-deck '([:c :jack] [:c :ace] [:c 10])
          p2-deck '([:c 10] [:c :queen] [:c 6])
          {:keys [game-result steps]} (g/play-game p1-deck p2-deck)
          [_ _ p2-deck] (last steps)]
      (is (some? steps))
      (is (= :player1-won game-result))
      (is (empty? p2-deck))))

  (testing "can generate correct game steps"
    (let [p1-deck '([:c :jack] [:c 10] [:c 9] [:c 6] [:c :ace])
          p2-deck '([:c 10] [:c :ace] [:c 6] [:c 2] [:c 9])
          {:keys [steps]} (g/play-game p1-deck p2-deck)
          steps (map first steps)]
      (is (= '([]
               [:p1-won [:c :jack] [:c 10]]
               [:p2-won [:c 10] [:c :ace]]
               [:p1-won [:c 9] [:c 6]])
             (take 4 steps)))))

  (testing "tie in first round"
    (let [three-cards (repeat 3 [:c 2])
          p1-deck (concat '([:c :jack]) three-cards '([:c :ace]))
          p2-deck (concat '([:c :jack]) three-cards '([:c 9]))
          {:keys [game-result steps]} (g/play-game p1-deck p2-deck)
          step-result (map first steps)]
      (is (= :player1-won
             game-result))
      (is (= '([]
               [:tie [:c :jack] [:c :jack]]
               [:p1-won [:c :ace] [:c 9]])
             step-result))))

  (testing "double tie in first round"
    (let [three-cards (repeat 3 [:c 2])
          p1-deck (concat '([:c :jack]) three-cards '([:c :ace]) three-cards '([:c 10]))
          p2-deck (concat '([:c :jack]) three-cards '([:c :ace]) three-cards '([:c 2]))
          {:keys [game-result steps]} (g/play-game p1-deck p2-deck)
          step-result (map first steps)
          [_ deck1 _] (last steps)]
      (is (= :player1-won
             game-result))
      (is (= '([]
               [:tie [:c :jack] [:c :jack]]
               [:tie [:c :ace] [:c :ace]]
               [:p1-won [:c 10] [:c 2]])
             step-result))
      (is (= (+ (count p1-deck) (count p2-deck))
             (count deck1)))))

  (testing "can play with all cards"
    (let [{:keys [game-result]} (g/play-game g/player1-cards g/player2-cards)]
      (is (or (= :player1-won
                 game-result)
              (= :player2-won
                 game-result)
              (= :tie
                 game-result)))))

  (testing "should be tie when players have same decks"
    (let [deck g/player1-cards
          {:keys [game-result]} (g/play-game deck deck)]
      (is (= :tie
             game-result)))))
