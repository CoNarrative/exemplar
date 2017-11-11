(ns exemplar.util-test
  (:require [clojure.test :refer :all]
            [exemplar.util :as util]))


(deftest ensure-require-test
  (testing "Requires ns if doesn't exist"
    (let [form '(ns exemplar.util-test (:require [clojure.test :refer :all]))]
      (is (= (util/ensure-require form 'exemplar.util)
             '(ns exemplar.util-test (:require [clojure.test :refer :all]
                                               [exemplar.util])))))
    (testing "Returns unmodified if already exists"
      (let [form '(ns exemplar.util-test (:require [clojure.test :refer :all]
                                                   [exemplar.util]))]
        (is (= (util/ensure-require form 'exemplar.util)
               form))))))
