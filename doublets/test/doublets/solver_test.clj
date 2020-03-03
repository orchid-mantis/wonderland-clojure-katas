(ns doublets.solver-test
  (:require [clojure.test :refer [deftest testing is]]
            [doublets.solver :refer [one-char-diff? paths-by-word doublets]]))

(deftest test-one-char-diff?
  (testing "no char difference"
    (is (= false
           (one-char-diff? "frog" "frog"))))
  (testing "single char difference"
    (is (= true
           (one-char-diff? "look" "cook"))))
  (testing "multiple char difference"
    (is (= false
           (one-char-diff? "frog" "lock")))))

(def small-dict ["door"
                 "boor"
                 "book"
                 "look"
                 "frog"])

(deftest test-paths-by-word
  (testing "can generate correct paths for words of same size"
    (is (= {"door" #{"boor"}
            "boor" #{"book" "door"}
            "book" #{"look" "boor"}
            "look" #{"book"}
            "frog" #{}}
           (paths-by-word small-dict)))))

(deftest solver-test
  (testing "with word links found"
    (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
           (doublets "head" "tail")))

    (is (= ["door" "boor" "book" "look" "lock"]
           (doublets "door" "lock")))

    (is (= ["bank" "bonk" "book" "look" "loon" "loan"]
           (doublets "bank" "loan")))

    (is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
           (doublets "wheat" "bread"))))

  (testing "with no word links found"
    (is (= []
           (doublets "ye" "freezer")))))
