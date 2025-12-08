(ns aoc2025.day4-test
  (:require [clojure.test :refer [deftest testing are is]]
            [clojure.string :as str]
            [clojure.set :as set]))
  
(defn split-lines [s]
  (map str/trim (str/split-lines s)))

(deftest day4
  
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
         (apply set/union)
         (set)))

  (testing "map to indices"
    (are
     [input output]
     (= output (map-to-indices input))
      "@" #{[0 0]}
      ".
       @" #{[0 1]}
      ".@" #{[1 0]}))
  
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
  
  (defn accessible-parperrolls [paperrolls]
    (let [accessible_rolls (->> paperrolls
                                (map #(apply neighbors %))
                                (apply merge-with +)
                                (filter #(>= (second %) 4))
                                (map first)
                                set)
          rolls_to_remove (set/difference paperrolls accessible_rolls)
          number_of_rolls_removed (count rolls_to_remove)]
      (case number_of_rolls_removed
        0 0
        (+ number_of_rolls_removed (accessible-parperrolls (set/difference paperrolls rolls_to_remove))))))

  (testing "day4"
    (are
     [input output]
     (= output (accessible-parperrolls (map-to-indices input)))
      "@@@
       @@@
       @@@" 9
      "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." 43
      (slurp "test/aoc2025/day4_input.txt") 8493))
  
  )