(ns wonderland-number.finder-test
  (:require [clojure.test :refer :all]
            [wonderland-number.finder :refer :all]))

(defn hasAllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn sum-of-cubed-digits [n]
  (let [digits (map (comp - (partial - (int \0)) int) (seq (str n)))
        cubes (map #(* % % %) digits)]
    (apply + cubes)))

(deftest test-wonderland-number
  (testing "A wonderland number must have the following things true about it"
    (let [wondernum (wonderland-number)]
      (is (= 6 (count (str wondernum))))
      (is (hasAllTheSameDigits? wondernum (* 2 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 3 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 4 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 5 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 6 wondernum))))))

(deftest sum-of-cubes
  (testing "A certain number must be equal to sum of its cubed digits"
    (let [num-under-test (self-cube-number)]
      (is (= num-under-test (sum-of-cubed-digits num-under-test))))))
