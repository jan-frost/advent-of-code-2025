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
  
  (defn digits-with-index [n s] (map list (range (- (count s) n)) (string-to-digits s)))
  
  (testing "range index"
    (are
     [input n output]
     (= output (digits-with-index n input))
      "999" 1 '((0 9) (1 9))
      "999" 2 '((0 9))))
  
  (defn largest-digit-with-index [n s]
    (->> s
         (digits-with-index n)
         (reduce #(if (>= (second %1) (second %2)) %1 %2) '(-1 -1))))
  
  (testing "largest number index"
    (are
     [input output]
     (= output (largest-digit-with-index 1 input))
      "987" [0 9]
      "897" [1 9]
      "789" [1 8]
      "999" [0 9]
      "4644333464433434855275434364342435544972433324444444432243354936362863356444434344443333433425344432" [37 9]))
  
  (defn largest-number [s n]
    (let [next-digit-with-index (largest-digit-with-index n s)
          next-substring (subs s (inc (first next-digit-with-index)))]
      (case n
        0 (second next-digit-with-index)
        (+ (* (second next-digit-with-index) (long (Math/pow 10 n)))
           (largest-number next-substring (dec n))))))
    
  
  (testing "find largest number in string"
         (are
          [input output]
          (= output (largest-number input 11))
           "987654321111111" 987654321111))
  
  (testing "day3 star 2"
    (are
     [input output]
     (= output (->> input
                    (split-lines)
                    (map #(largest-number % 11))
                    (reduce +)))
      "987654321111111
  811111111111119
  234234234234278
  818181911112111" 3121910778619
      (slurp "test/aoc2025/day3_input.txt") 169349762274117)) 
  
  )
