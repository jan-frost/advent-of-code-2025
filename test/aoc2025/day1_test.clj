(ns aoc2025.day1-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]))

(defn get-direction [s]
  {:direction (case (first s) \L - \R +)
   :distance (parse-long (subs s 1))})

(defn turn [position op amount]
  (let [position-range (map #(mod (op % 1) 100) (range position (op position amount) (op 0 1)))]
    {:position (last position-range)
     :zeros (count (filter #(= 0 %) position-range))}))

(defn positions [moves]
  (reductions (fn [acc move] (apply turn (acc :position) (vals move))) {:position 50 :zeros 0} moves))

(defn count-zeros [coll]
  (reduce + (map :zeros coll)))

(deftest test-main

  (testing "get direction"
    (are
     [input direction distance]
     (= {:direction direction :distance distance} (get-direction input))
      "L2" - 2
      "R55" + 55))

  (testing "turn without overshoot"
    (are
     [position direction amount result]
     (= result (turn position direction amount))
      50 - 10 {:position 40 :zeros 0}
      30 + 5 {:position 35 :zeros 0}))

  (testing "turn with overshoot"
    (are
     [position direction amount result]
     (= result (turn position direction amount))
      0 - 1 {:position 99 :zeros 0}
      99 + 1 {:position 0 :zeros 1}
      0 + 200 {:position 0 :zeros 2}))

  (testing "accumulate position"
    (are
     [moves result]
     (= result (positions moves))
      [{:direction - :distance 10}] [{:position 50 :zeros 0} {:position 40 :zeros 0}]
      [{:direction + :distance 50} {:direction - :distance 10}] [{:position 50 :zeros 0} {:position 0 :zeros 1} {:position 90 :zeros 0}]))

  (testing "count zeros"
    (are
     [input result]
     (= result (count-zeros input))
      [{:position 50 :zeros 0}] 0
      [{:position 50 :zeros 1}] 1
      [{:zeros 1} {:zeros 1}] 2))

  (testing "integration test"
    (are
     [input result]
     (= result (->> input
                    (str/split-lines)
                    (map get-direction)
                    (positions)
                    (count-zeros)))
      (slurp "test/aoc2025/day1_input.txt") 5961)))
