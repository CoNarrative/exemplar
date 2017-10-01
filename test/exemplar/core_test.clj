(ns exemplar.core-test
  (:require [clojure.test :refer :all]
            [exemplar.core :as exemplar]))

(defn main-fixture [f]
  (exemplar/register-path "test.edn")
  (f)
  (exemplar/delete-all (:path @exemplar/state)))

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
           {:name 'my-func
            :ns 'exemplar.core-test
            :source "(defn my-func [args] (map inc args))"
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
         {:name 'user-ids
          :ns 'exemplar.core-test
          :source "(defn user-ids [users] (mapv :id users))"
          :out [1 2]
          :in [[{:id 1 :first-name "Bob" :last-name "Smith"}
                {:id 2 :first-name "Mary" :last-name "Brown"}]]})))

(def my-list [1 2 3])
(def my-other-list [2 2 2])

(deftest operations-with-variables
  (exemplar/save (my-func (concat my-list my-other-list)))
  (is (= (exemplar/show my-func)
         {:name 'my-func
          :ns 'exemplar.core-test
          :source "(defn my-func [args] (map inc args))"
          :out '(2 3 4 3 3 3)
          :in ['(1 2 3 2 2 2)]})))

(deftest record-once
  (testing "Records first call"
    (exemplar/record-once my-func)
    (my-func [1 2 3])
    (let [saved (exemplar/show my-func)]
      (is (= saved
             {:name 'my-func
              :ns 'exemplar.core-test
              :source "(defn my-func [args] (map inc args))"
              :out '(2 3 4)
              :in [[1 2 3]]}))
      (my-func [0])
      (is (= (exemplar/show my-func) saved)))))
