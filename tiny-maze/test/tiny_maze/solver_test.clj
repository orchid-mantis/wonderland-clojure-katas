(ns tiny-maze.solver-test
  (:require [clojure.test :refer [deftest testing is]]
            [tiny-maze.solver :as solver]))

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
     (is (= sol (solver/solve-maze maze))))))
