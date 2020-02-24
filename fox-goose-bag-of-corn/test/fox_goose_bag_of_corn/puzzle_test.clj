(ns fox-goose-bag-of-corn.puzzle-test
  (:require [clojure.test :refer [deftest testing is]]
            [fox-goose-bag-of-corn.puzzle :as puzzle]
            [clojure.set]))

(deftest test-next-move
  (testing "can move with one item to other side"
    (is (= [[#{:goose} #{:boat :you :fox} #{}]
            [#{:goose} #{:boat} #{:you :fox}]]
           (puzzle/next-move [#{:you :goose :fox} #{:boat} #{}] :fox)))
    (is (= [[#{:goose :fox} #{:boat :you :corn} #{}]
            [#{:goose :fox :you :corn} #{:boat} #{}]]
           (puzzle/next-move [#{:goose :fox} #{:boat} #{:you :corn}] :corn))))
  (testing "can move without any item"
    (is (= [[#{:goose} #{:boat :you} #{:corn}]
            [#{:goose} #{:boat} #{:you :corn}]]
           (puzzle/next-move [#{:you :goose} #{:boat} #{:corn}] nil))))
  (testing "cannot move item which is not present with you"
    (is (= nil
           (puzzle/next-move [#{:you :goose} #{:boat} #{:fox}] :fox)))))

(deftest test-valid?
  (testing "can validate move"
    (is (= true
           (puzzle/valid? [[#{:fox :corn} #{:you :boat :goose} #{}]
                           [#{:fox :corn} #{:boat} #{:you :goose}]])))
    (is (= true
           (puzzle/valid? [[#{:fox} #{:you :boat :goose} #{:corn}]
                           [#{:fox} #{:boat} #{:you :goose :corn}]])))
    (is (= false
           (puzzle/valid? [[#{:goose :corn} #{:you :fox :boat} #{}]
                           [#{:goose :corn} #{:boat} #{:you :fox}]])))
    (is (= false
           (puzzle/valid? [[#{:fox :goose} #{:you :corn} #{}]
                           [#{:fox :goose} #{:boat} #{:you :corn}]])))))

(deftest test-useful?
  (testing "can check whether item is always moved from left bank"
    (is (= true
           (puzzle/useful? [[#{:fox :corn} #{:you :boat :goose} #{}]
                            [#{:fox :corn} #{:boat} #{:you :goose}]])))
    (is (= false
           (puzzle/useful? [[#{:fox :goose :corn} #{:you :boat} #{}]
                            [#{:fox :goose :corn} #{:boat} #{:you}]])))
    (testing "return true when move from right to left bank"
      (is (= true
             (puzzle/useful? [[#{:fox :corn} #{:you :boat} #{:goose}]
                              [#{:you :fox :corn} #{:boat} #{:goose}]]))))))

(defn validate-move [step1 step2]
  (testing "only you and another thing can move"
    (let [diff1 (clojure.set/difference step1 step2)
          diff2 (clojure.set/difference step2 step1)
          diffs (concat diff1 diff2)
          diff-num (count diffs)]
      (is (> 3 diff-num))
      (when (pos? diff-num)
        (is (contains? (set diffs) :you)))
      step2)))

(deftest test-river-crossing-plan
  (let [crossing-plan (map (partial map set) (puzzle/river-crossing-plan))]
    (testing "you begin with the fox, goose and corn on one side of the river"
      (is (= [#{:you :fox :goose :corn} #{:boat} #{}]
             (first crossing-plan))))
    (testing "you end with the fox, goose and corn on one side of the river"
      (is (= [#{} #{:boat} #{:you :fox :goose :corn}]
             (last crossing-plan))))
    (testing "things are safe"
      (let [left-bank (map first crossing-plan)
            right-bank (map last crossing-plan)]
        (testing "the fox and the goose should never be left alone together"
          (is (empty?
               (filter #(= % #{:fox :goose}) (concat left-bank right-bank)))))
        (testing "the goose and the corn should never be left alone together"
          (is (empty?
               (filter #(= % #{:goose :corn}) (concat left-bank right-bank)))))))
    (testing "The boat can carry only you plus one other"
      (let [boat-positions (map second crossing-plan)]
        (is (empty?
             (filter #(> (count %) 3) boat-positions)))))
    (testing "moves are valid"
      (let [left-moves (map first crossing-plan)
            middle-moves (map second crossing-plan)
            right-moves (map last crossing-plan)]
        (reduce validate-move left-moves)
        (reduce validate-move middle-moves)
        (reduce validate-move right-moves)))))

