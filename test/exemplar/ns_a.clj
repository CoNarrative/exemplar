(ns exemplar.ns-a)

(defn some-func [xs]
  (map inc xs))

(defmacro some-macro [xs]
  `(map inc ~xs))

(def some-atom (atom nil))

(defn some-impure-func [x]
  (reset! some-atom x))
