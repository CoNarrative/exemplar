(ns exemplar.core-test
    (:require [clojure.test :refer :all]
              [exemplar.core :as exemplar]
              [exemplar.ns-a :as ns-a])
    (:import [java.io File]
             [exemplar.core UnreadableTag]))

(defn main-fixture [f]
  (exemplar/register-path "test.edn")
  (f)
  (exemplar/delete-all (:path @exemplar/state)))

(use-fixtures :each main-fixture)

(defn my-func [args]
  (map inc args))

(deftest basic
  (testing "Show before save"
    (is (= (exemplar/show ns-a/some-func)
           {:name "exemplar.ns-a/some-func"})))
  (testing "Show after save"
    (exemplar/save (ns-a/some-func [1 2 3]))
    (is (= (exemplar/show ns-a/some-func)
           {:name 'some-func
            :ns 'exemplar.ns-a
            :source "(defn some-func [xs] (map inc xs))"
            :out '(2 3 4)
            :in [[1 2 3]]})))
  (testing "Run after save"
    (is (= (exemplar/run ns-a/some-func)
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

(defn fn-with-clojure-core-fn-as-argument [f xs] (mapv f xs))
(deftest save-show-clojure-core-fn-as-argument
  (exemplar/save (fn-with-clojure-core-fn-as-argument inc [1 2 3]))
  (is (= (exemplar/show fn-with-clojure-core-fn-as-argument)
         '{:name fn-with-clojure-core-fn-as-argument
           :ns exemplar.core-test
           :source "(defn fn-with-clojure-core-fn-as-argument [f xs] (mapv f xs))"
           :out [2 3 4]
           :in [clojure.core/inc [1 2 3]]})))

(deftest save-show-clojure-core-fn
  (exemplar/save (mapv inc [1 2 3]))
  (is (= (exemplar/show mapv)
        '{:name mapv
          :ns clojure.core
          :source "(defn mapv\n  \"Returns a vector consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments.\"\n  {:added \"1.4\"\n   :static true}\n  ([f coll]\n     (-> (reduce (fn [v o] (conj! v (f o))) (transient []) coll)\n         persistent!))\n  ([f c1 c2]\n     (into [] (map f c1 c2)))\n  ([f c1 c2 c3]\n     (into [] (map f c1 c2 c3)))\n  ([f c1 c2 c3 & colls]\n     (into [] (apply map f c1 c2 c3 colls))))"
          :out [2 3 4]
          :in [clojure.core/inc [1 2 3]]})))

(deftest record-once
  (testing "Records first call"
    (exemplar/record-once ns-a/some-func)
    (is (= (ns-a/some-func [1 2 3]) [2 3 4])
      "Return value of first call to fn should not be altered")
    (let [saved (exemplar/show ns-a/some-func)]
      (is (= saved
             {:name 'some-func
              :ns 'exemplar.ns-a
              :source "(defn some-func [xs] (map inc xs))"
              :out '(2 3 4)
              :in [[1 2 3]]}))
      (is (= (ns-a/some-func [5 4 3]) [6 5 4])
          "Return value of subsequent calls to fn should not be altered")
      (is (= (exemplar/show ns-a/some-func) saved)
          "record-once should only save the first call")))

  (testing "Saved in memory"
    (let [saved-mem (get-in @exemplar.core/state [:entries "exemplar.ns-a/some-func"])]
      (is (= (select-keys saved-mem [:ns :name])
             {:ns 'exemplar.ns-a,
              :name 'some-func}))
      (is (= true (clojure.string/includes?
                    (str (:var-val saved-mem))
                    "exemplar.ns_a$some_func"))
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

(deftest get-ns-meta
  (testing "Identify def vs. defn vs. defmacro"
    (is (= (map #(dissoc % :var) (exemplar.core/get-decl-types exemplar.ns-a))
          `[{:symbol exemplar.ns-a/some-func
             :fn? true}
            {:symbol exemplar.ns-a/some-macro :macro? true}
            {:symbol exemplar.ns-a/some-atom
             :def? true}
            {:symbol exemplar.ns-a/some-impure-func
             :fn? true}]))))

(deftest record-namespace-once
  (testing "Records first call"
    (exemplar.core/record-namespace-once exemplar.ns-a)
    (is (= (ns-a/some-func [1 2 3]) [2 3 4])
      "Return value of first call to fn should not be altered")
    (let [saved (exemplar/show ns-a/some-func)]
      (is (= saved
            {:name 'some-func
             :ns 'exemplar.ns-a
             :source "(defn some-func [xs] (map inc xs))"
             :out '(2 3 4)
             :in [[1 2 3]]})
        "Incorrect saved values")
      (is (= (ns-a/some-func [5 4 3]) [6 5 4])
        "Return value of subsequent calls to fn should not be altered")
      (is (= (exemplar/show ns-a/some-func) saved)
        "record-once should only save the first call")))

  (testing "Saved in memory"
    (let [saved-mem (get-in @exemplar.core/state [:entries "exemplar.ns-a/some-func"])]
      (is (= (select-keys saved-mem [:ns :name])
            {:ns 'exemplar.ns-a,
             :name 'some-func}))
      (is (= true (clojure.string/includes?
                    (str (:var-val saved-mem))
                    "exemplar.ns_a$some_func"))
        ":var-val should correspond to the original var"))))

(deftest record-namespace
  (testing "Records first call"
    (exemplar/record-namespace exemplar.ns-a)
    (is (= (ns-a/some-func [1 2 3]) [2 3 4])
      "Return value of first call to fn should not be altered")
    (let [saved (exemplar/show ns-a/some-func)]
      (is (= saved
            {:name 'some-func
             :ns 'exemplar.ns-a
             :source "(defn some-func [xs] (map inc xs))"
             :out '(2 3 4)
             :in [[1 2 3]]})
        "First call should have been persisted")))
  (testing "Records second call"
    (is (= (ns-a/some-func [1 2 5]) [2 3 6])
      "Return value of second call to fn should not be altered")
    (let [saved (exemplar/show exemplar.ns-a/some-func)]
      (is (= saved
            {:name 'some-func
             :ns 'exemplar.ns-a
             :source "(defn some-func [xs] (map inc xs))"
             :out '(2 3 6)
             :in [[1 2 5]]})
        "Second call should have been persisted")))
  (testing "Stop recording"
    (exemplar/stop-recording-namespace exemplar.ns-a)
    (is (= true (clojure.string/includes?
                  (str @#'ns-a/some-func)
                  "exemplar.ns_a$some_func"))
      "Value of var should have been reset to what it was before recording started")
    (is (= (ns-a/some-func [1 2 3]) [2 3 4])
      "Fn should behave normally after recording has stopped")
    (let [saved (exemplar/show ns-a/some-func)]
      (is (and (= (:in saved) [[1 2 5]])
            (= (:out saved) [2 3 6]))
        "Recording should have stopped but new values were written"))))

(defn echo [x] x)

(deftest unreadable-tags
  (testing "#object[java.io.File] nested"
    (let [file (File. "my-file.edn")]
      (exemplar/save (echo {:some-keyword file}))
      (let [saved (exemplar/show echo)
            in-arg (some-> saved :in (first) :some-keyword)
            out (get-in saved [:out :some-keyword])]
        (is (= (type out) UnreadableTag))
        (is (= (type in-arg) UnreadableTag))
        (is (= (:tag out) 'object))
        (is (vector? (:value out)))
        (is (= 'java.io.File (first (:value out)))))))
  (testing "#object[java.io.File] non-nested"
    (let [file (File. "my-file.edn")]
      (exemplar/save (echo file))
      (let [saved (exemplar/show echo)
            in-arg (some-> saved :in (first))
            out (get-in saved [:out])]
        (is (= (type out) UnreadableTag))
        (is (= (type in-arg) UnreadableTag))
        (is (= (:tag out) 'object))
        (is (vector? (:value out)))
        (is (= 'java.io.File (first (:value out)))))))
  (testing "IDeref"
    (let [a (atom {:hello "world"})]
      (exemplar/save (echo a))
      (let [saved (exemplar/show echo)
            in-arg (some-> saved :in (first))
            out (get-in saved [:out])]
        (is (= (type out) UnreadableTag))
        (is (= (type in-arg) UnreadableTag))
        (is (= (:tag out) 'object))
        (is (vector? (:value out)))
        (is (= 'clojure.lang.Atom (first (:value out))))))))
