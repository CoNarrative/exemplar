(ns exemplar.core-test
  (:require [clojure.test :refer :all]
            [exemplar.core :as exemplar]))

(defn main-fixture [f]
  (exemplar/register-path "test.edn")
  (f)
  (spit "test.edn" "{}"))

(use-fixtures :each main-fixture)

(defn my-func [args]
  (map inc args))

(deftest basic
  (testing "Show before save"
    (is (= (exemplar/show my-func)
           {:name "exemplar.core-test/my-func"})))
  (testing "Show after save"
    (exemplar/save (my-func [1 2 3]))
    (is (= (exemplar/show my-func)
           {:name "exemplar.core-test/my-func"
            :out '(2 3 4)
            :in [[1 2 3]]})))
  (testing "Run after save"
    (is (= (exemplar/run my-func)
           '(2 3 4)))))

(def users
  [{:id 1 :first-name "Bob" :last-name "Smith"}
   {:id 2 :first-name "Mary" :last-name "Brown"}])
(defn user-ids [users]
  (mapv :id users))

(deftest variables
  (exemplar/save (user-ids users))
  (is (= (exemplar/show user-ids)
         {:name "exemplar.core-test/user-ids"
          :out [1 2]
          :in [[{:id 1 :first-name "Bob" :last-name "Smith"}
                {:id 2 :first-name "Mary" :last-name "Brown"}]]})))

(def my-list [1 2 3])
(def my-other-list [2 2 2])

(deftest operations-with-variables
  (exemplar/save (my-func (concat my-list my-other-list)))
  (is (= (exemplar/show my-func)
         {:name "exemplar.core-test/my-func",
          :out '(2 3 4 3 3 3),
          :in ['(1 2 3 2 2 2)]})))
