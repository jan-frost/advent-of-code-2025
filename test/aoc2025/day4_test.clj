(ns aoc2025.day4-test
  (:require [clojure.test :refer [deftest testing are is]]
            [clojure.string :as str]
            [clojure.set :as set]))
  
(defn split-lines [s]
  (map str/trim (str/split-lines s)))

(deftest day4
  
  (defn lines-with-index [lines]
    (map-indexed vector lines))
  
  (testing "map index"
    (is (= [[0 \a] [1 \b]] (map-indexed vector [\a \b]))))
  
  (testing "map @'s"
    (is (= [1 3] (keep-indexed #(when (= %2 \@) %1) ".@.@"))))
  
  (defn map-to-indices [s]
    (->> s
         split-lines
         (map-indexed vector)
         (map (fn [[y line]]
                (map #(vector % y) (keep-indexed #(when (= %2 \@) %1) line))))
         (into [])
         (apply concat)))

  (testing "map to indices"
    (are
     [input output]
     (= output (map-to-indices input))
      "@" [[0 0]]
      ".
       @" [[0 1]]
      ".@" [[1 0]]))
  
  (defn neighbors [x y]
    (apply hash-map [[(- x 1) (- y 1)] 1
                     [(- x 1) (- y 0)] 1
                     [(- x 1) (+ y 1)] 1
                     [(- x 0) (- y 1)] 1
                     [(- x 0) (+ y 1)] 1
                     [(+ x 1) (- y 1)] 1
                     [(+ x 1) (- y 0)] 1
                     [(+ x 1) (+ y 1)] 1]))
  
  (testing "index to neighbors"
    (are
     [input output]
     (= output (apply neighbors input))
      [0 0] {[-1 -1] 1 [-1 0] 1 [-1 1] 1 [0 -1] 1 [0 1] 1 [1 -1] 1 [1 0] 1 [1 1] 1}))

  (testing "count neighbors"
    (are
     [input output]
     (= output (apply merge-with + input))
      [{[-1 -1] 1} {[-1 -1] 1}] {[-1 -1] 2}))

  (testing "at least 4 neighbors"
    (are
     [input output]
     (= output (filter #(>= (second %) 4) input))
      {[-1 -1] 3 [-1 1] 4} [[[-1 1] 4]]))

  (testing "more than 4 neighbors"
    (are
     [input output]
     (= output (->> input
                    (filter #(> (second %) 4))
                    (map first)
                    set))
      {[-1 -1] 4 [-1 1] 5} #{[-1 1]}))

  (testing "remove greater 4"
    (are
     [coll1 coll2 output]
     (= output (set/difference (set coll1) (set coll2)))
      [[-1 1] [0 0]] [[1 1] [-1 1]] #{[0 0]}))
  
  (defn accessible-parperrolls [s]
    (let [paperrolls (map-to-indices s)]
      (->> paperrolls
           (map #(apply neighbors %))
           (apply merge-with +)
           (filter #(>= (second %) 4))
           (map first)
           set
           (set/difference (set paperrolls))
           count
           )))

  (testing "day4"
    (are
     [input output]
     (= output (accessible-parperrolls input))
      "@@@
       @@@
       @@@" 4
      "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." 13
    (slurp "test/aoc2025/day4_input.txt") 1346))
  
  )