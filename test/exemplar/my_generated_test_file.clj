(ns exemplar.my-generated-test-file (:require [clojure.test :refer [deftest is testing]]))
 
 
(deftest exemplar-core-my-func-test
  (is (= (apply exemplar.core/my-func [[1 2 3]])
         '(1))))

