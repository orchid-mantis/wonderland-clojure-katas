(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn rotate [n coll]
  (let [size (count coll)
        n (if (neg? n)
            (* (- n) (- size 1))
            n)
        mod-n (mod n size)]
    (drop mod-n (take (+ mod-n size) (cycle coll)))))

(defn rotate-m [m sign]
  (let [rotated (for [indexed-row (zipmap m (range))
                      :let [row (first indexed-row)
                            n (sign (second indexed-row))]]
                  (rotate n row))]
    (apply map vector rotated)))

(defn sum-each-row [m]
  (map (partial apply +) m))

(defn sum-each-column [m]
  (->> (apply map vector m)
       (map (partial apply +))))

(defn sum-each-diagonal [m]
  (let [first-diag  (first (rotate-m m +))
        second-diag (last  (rotate-m m -))]
    (list (reduce + first-diag) (reduce + second-diag))))

(defn build-matrix [values]
  (vec (map vec (partition 3 values))))

(defn magic-square? [m]
  (let [sum-rows (set (sum-each-row m))
        sum-cols (set (sum-each-column m))
        sum-diag (set (sum-each-diagonal m))]
    (and (= sum-rows
            sum-cols
            sum-diag)
         (= 1
            (count sum-rows)
            (count sum-cols)
            (count sum-diag)))))

(defn magic-square [values]
  (->> (combo/permutations values)
       (map (partial build-matrix))
       (filter magic-square?)
       (first)))
