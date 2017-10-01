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
    (is (= (my-func [1 2 3]) [2 3 4])
      "Return value of first call to fn should not be altered")
    (let [saved (exemplar/show my-func)]
      (is (= saved
             {:name 'my-func
              :ns 'exemplar.core-test
              :source "(defn my-func [args] (map inc args))"
              :out '(2 3 4)
              :in [[1 2 3]]}))
      (is (= (my-func [5 4 3]) [6 5 4])
          "Return value of subsequent calls to fn should not be altered")
      (is (= (exemplar/show my-func) saved)
          "record-once should only save the first call")))

  (testing "Saved in memory"
    (let [saved-mem (get-in @exemplar.core/state [:entries "exemplar.core-test/my-func"])]
      (is (= (select-keys saved-mem [:ns :name])
             {:ns 'exemplar.core-test,
              :name 'my-func}))
      (is (= true (clojure.string/includes?
                    (str (:var-val saved-mem))
                    "exemplar.core_test$my_func"))
          ":var-val should correspond to the original var"))))

(deftest record
  (testing "Records first call"
    (exemplar/record my-func)
    (is (= (my-func [1 2 3]) [2 3 4])
        "Return value of first call to fn should not be altered")
    (let [saved (exemplar/show my-func)]
      (is (= saved
            {:name 'my-func
             :ns 'exemplar.core-test
             :source "(defn my-func [args] (map inc args))"
             :out '(2 3 4)
             :in [[1 2 3]]})
         "First call should have been persisted")))
  (testing "Records second call"
    (is (= (my-func [1 2 5]) [2 3 6])
      "Return value of second call to fn should not be altered")
    (let [saved (exemplar/show my-func)]
      (is (= saved
            {:name 'my-func
             :ns 'exemplar.core-test
             :source "(defn my-func [args] (map inc args))"
             :out '(2 3 6)
             :in [[1 2 5]]})
          "Second call should have been persisted")))
  (testing "Stop recording"
    (exemplar/stop-recording my-func)
    (is (= true (clojure.string/includes?
                  (str @#'my-func)
                  "exemplar.core_test$my_func"))
     "Value of var should have been reset to what it was before recording started")
    (is (= (my-func [1 2 3]) [2 3 4])
        "Fn should behave normally after recording has stopped")
    (let [saved (exemplar/show my-func)]
      (is (and (= (:in saved) [[1 2 5]])
               (= (:out saved) [2 3 6]))
          "Recording should have stopped but new values were written"))))
