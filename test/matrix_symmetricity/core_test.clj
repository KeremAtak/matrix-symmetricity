(ns matrix-symmetricity.core-test
  (:require [clojure.test :refer :all]
            [matrix-symmetricity.core :refer :all]))

(deftest a-test
  (testing "matrix-symmetricity"
    (let [str-arr1 ["1" "7" "3" "<>" "7" "4" "-5" "<>" "3" "-5" "6"]
          str-arr2 ["1" "2" "<>" "3" "4"]
          str-arr3 ["1" "7" "3" "<>" "7" "4" "-5"]
          str-arr4 ["0" "6" "<>" "6" "0"]
          str-arr5 ["1" "9" "3" "<>" "7" "4" "-5" "<>" "3" "-5" "6"]]
      (is (= (matrix-symmetricity str-arr1) "symmetric"))
      (is (= (matrix-symmetricity str-arr2) "not symmetric"))
      (is (= (matrix-symmetricity str-arr3) "not possible"))
      (is (= (matrix-symmetricity str-arr4) "symmetric"))
      (is (= (matrix-symmetricity str-arr5) "not symmetric")))))
