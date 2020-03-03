(ns doublets.solver
  (:require [clojure.java.io :as io]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn all-pairs [dict]
  (for [w1 dict
        w2 dict]
    [w1 w2]))

(defn one-char-diff? [w1 w2]
  (->> (map (fn [ch1 ch2] (= ch1 ch2)) w1 w2)
       (filter false?)
       (count)
       (= 1)))

(defn paths-by-word [dict]
  (let [pairs (->> (all-pairs dict)
                   (filter (fn [[w1 w2]] (one-char-diff? w1 w2))))
        maps (for [w dict]
               (assoc {} w (->> (filter (fn [[first _]] (= first w)) pairs)
                                (map second)
                                (set))))]
    (apply merge maps)))

(defn gen-paths [start-word end-word paths dict]
  (let [next-words (get dict start-word)]
    (cond
      (contains? next-words end-word) (conj (conj paths start-word) end-word)
      :else (->> (vec next-words)
                 (filter (fn [w] (not-any? #(= w %) paths)))
                 (map #(gen-paths % end-word (conj paths start-word) dict))
                 (first)
                 (vec)))))

(defn doublets [word1 word2]
  (let [w1-size (count word1)]
    (cond
      (not= w1-size (count word2)) []
      :else (->> words
                 (filter #(= w1-size (count %)))
                 (paths-by-word)
                 (gen-paths word1 word2 [])))))
