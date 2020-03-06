(ns tiny-maze.solver-test
  (:require [clojure.test :refer [deftest testing is]]
            [tiny-maze.solver :as solver]))

(def maze [[:S 0 1]
           [1  0 1]
           [1  0 :E]])

(deftest test-position
  (testing "can find position of start location (marked with :S)"
    (is (= [0 0]
           (solver/position maze :S))))
  (testing "can find position of end location (marked with :E)"
    (is (= [2 2]
           (solver/position maze :E)))))

(deftest test-next-moves
  (testing "can generate all possible moves from current position in maze"
    (is (= [[0 1]]
           (solver/next-moves maze [0 0])))
    (is (= [[0 1] [2 1]]
           (solver/next-moves maze [1 1]))))
  (testing "end position (marked with :E) is accessible from position [2 1]"
    (is (= [[1 1] [2 2]]
           (solver/next-moves maze [2 1])))))

(deftest test-solve-maze
  (testing "can find way to exit with 3x3 maze"
    (let [maze [[:S 0 1]
                [1  0 1]
                [1  0 :E]]
          sol [[:x :x 1]
               [1  :x 1]
               [1  :x :x]]]
      (is (= sol (solver/solve-maze maze)))))

  (testing "can find way to exit with 4x4 maze"
    (let [maze [[:S 0 0 1]
                [1  1 0 0]
                [1  0  0 1]
                [1  1  0 :E]]
          sol [[:x :x :x 1]
               [1  1 :x 0]
               [1  0 :x 1]
               [1  1  :x :x]]]
      (is (= sol (solver/solve-maze maze))))

    (let [maze [[0  0  0  :S]
                [0  1  1  1]
                [0  0  0  1]
                [0  1  0  :E]]
          sol [[:x :x :x :x]
               [:x 1  1  1]
               [:x :x :x 1]
               [0  1  :x :x]]]
      (is (= sol (solver/solve-maze maze)))))
  
  (testing "can find way to exit with 10x10 maze"
    (let [maze [[0  0 :S  0  0  0  0  0  0  0]
                [0  1  1  1  1  1  1  1  0  1]
                [0  0  0  0  0  0  0  0  0  1]
                [0  1  1  1  1  1  1  1  0  1]
                [0  0  0  1 :E  1  0  0  0  0]
                [0  1  1  1  0  1  1  1  0  1]
                [0  1  0  0  0  1  0  1  0  1]
                [0  1  0  1  0  1  0  1  0  1]
                [0  1  0  1  1  1  0  1  0  1]
                [0  0  0  1  1  1  0  0  0  1]]
          sol [[:x :x :x  0  0  0  0  0  0  0]
               [:x  1  1  1  1  1  1  1  0  1]
               [:x  0  0  0  0  0  0  0  0  1]
               [:x  1  1  1  1  1  1  1  0  1]
               [:x  0  0  1 :x  1  0  0  0  0]
               [:x  1  1  1 :x  1  1  1  0  1]
               [:x  1 :x :x :x  1  0  1  0  1]
               [:x  1 :x  1  0  1  0  1  0  1]
               [:x  1 :x  1  1  1  0  1  0  1]
               [:x :x :x  1  1  1  0  0  0  1]]]
      (is (= sol (solver/solve-maze maze))))))
