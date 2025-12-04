(ns aoc2025.day3-test
  (:require [clojure.test :refer [deftest testing are]]
            [clojure.string :as str]))

(deftest day3

  (testing "number from char"
    (are
     [input output]
     (= output (- (int input) (int \0)))
      \0 0
      \9 9))
  
  (defn split-lines [s]
    (map str/trim (str/split-lines s)))

  (testing "split lines"
    (are
     [input output]
     (= output (split-lines input))
      "87
       78" ["87" "78"]
      ))
  
  (testing "comparison"
    (are
     [input output]
     (= output (reduce #(if (>= (first %1) (first %2)) %1 %2) '(-1 -1) input))
      ['(9 0) '(8 1) '(7 2)] '(9 0) 
      ['(9 0) '(9 1) '(9 2)] '(9 0) 
      ))
  
  (defn string-to-digits [s] (map #(- (int %) (int \0)) s))
  
  (testing "string to number list"
    (are
     [input output]
     (= output (string-to-digits input))
      "999" [9 9 9]))
  
  (defn digits-with-index [s] (map list (range (- (count s) 1)) (string-to-digits s)))
  
  (testing "range index"
    (are
     [input output]
     (= output (digits-with-index input))
      "999" ('(0 9) '(1 9))))
  
  (defn first-index-with-digit [s]
    (->> s
         (digits-with-index)
         (reduce #(if (>= (second %1) (second %2)) %1 %2) '(-1 -1))))
  
  (testing "largest number index"
    (are
     [input output]
     (= output (first-index-with-digit input))
      "987" [0 9]
      "897" [1 9]
      "789" [1 8]
      "999" [0 9]
      "4644333464433434855275434364342435544972433324444444432243354936362863356444434344443333433425344432" [37 9]))
  
  (defn next-largest-number [index s]
    (reduce max -1 (string-to-digits (subs s (inc index)))))
  
  (testing "second largest number index"
    (are
     [input output]
     (= output (let [largest (first-index-with-digit input)]
                 (next-largest-number (first largest) input)))
      "987" 8
      "897" 7
      "789" 9
      "4644333464433434855275434364342435544972433324444444432243354936362863356444434344443333433425344432" 9))
  
  (defn largest-number [s]
    (let [largest-with-index (first-index-with-digit s)
          first-digit (second largest-with-index)
          second-digit (next-largest-number (first largest-with-index) s)]
      (+ (* first-digit 10) second-digit)))
  
  (testing "largest number below 100"
    (are
     [input output]
     (= output (largest-number input))
      "873256218561" 88
      "4644333464433434855275434364342435544972433324444444432243354936362863356444434344443333433425344432" 99)
    )
  
  (testing "day3 star 1"
    (are
     [input output]
     (= output (->> input
                    (split-lines)
                    (map largest-number)
                    (reduce +)))
      "987654321111111
  811111111111119
  234234234234278
  818181911112111" 357
      (slurp "test/aoc2025/day3_input.txt") 17107))
  
  
  
  )
