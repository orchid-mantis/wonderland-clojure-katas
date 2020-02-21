(ns fox-goose-bag-of-corn.puzzle-test
  (:require [clojure.test :refer [deftest testing is]]
            [fox-goose-bag-of-corn.puzzle :as puzzle]
            [clojure.set]))

(deftest test-possible-picks
  (testing "cannot pick item which was picked last time"
    (is (= #{nil}
           (set (puzzle/possible-picks [:fox] :fox))))
    (is (= #{:fox}
           (set (puzzle/possible-picks [:fox] nil)))))
  
  (testing "can pick only goose - other picks leads to invalid states"
    (is (= #{:goose}
           (set (puzzle/possible-picks [:fox :goose :corn] nil)))))
  (testing "can pick fox or return with nothing (nil)"
    (is (= #{:fox nil}
           (set (puzzle/possible-picks [:fox :corn] :corn)))))
  (testing "can pick only goose - fox was picked last time and nothing leads to invalid state"
    (is (= #{:goose}
           (set (puzzle/possible-picks [:fox :goose] :fox))))))

(deftest test-move-right
  (testing "can move one item from left to right"
    (let [moves (map (partial map set) (puzzle/move-right [:fox :goose :corn] [] :corn))]
      (is (= [[#{:fox :goose} #{:boat :you :corn} #{}]
              [#{:fox :goose} #{:boat} #{:you :corn}]]
             moves)))
    (let [moves (map (partial map set) (puzzle/move-right [:fox] [:apple] :fox))]
      (is (= [[#{} #{:boat :you :fox} #{:apple}]
              [#{} #{:boat} #{:you :apple :fox}]]
             moves)))))

(deftest test-move-left
  (testing "can move one item from right to left"
    (let [moves (map (partial map set) (puzzle/move-left [] [:fox :goose :corn] :corn))]
      (is (= [[#{} #{:boat :you :corn} #{:fox :goose}]
              [#{:you :corn} #{:boat} #{:fox :goose}]]
             moves)))
    (let [moves (map (partial map set) (puzzle/move-left [:fox] [:apple] :apple))]
      (is (= [[#{:fox} #{:boat :you :apple} #{}]
              [#{:you :apple :fox} #{:boat} #{}]]
             moves)))))

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

