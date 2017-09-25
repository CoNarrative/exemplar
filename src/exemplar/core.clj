(ns exemplar.core)


(def state (atom {:path nil}))

(defn register-path [path]
  (swap! exemplar.core/state assoc :path path))

(defn write-out [path example]
  (let [in (slurp path)
        examples (read-string (if (= in "") "{}" in))]
    (spit path (with-out-str (clojure.pprint/pprint (merge examples example))))))

(defmacro save [sexpr]
  (let [met `(meta (var ~(first sexpr)))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        args (vec (rest sexpr))
        entry `{~key {:in ~args :out ~sexpr}}]
    `(write-out (:path (deref exemplar.core/state)) ~entry)))

(defmacro run [sym]
  (let [met `(meta (var ~sym))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        examples '(read-string (slurp (:path @exemplar.core/state)))
        data `(get ~examples ~key)
        ex `(apply ~sym (:in ~data))]
    ex))

(defmacro show [sym]
  (let [met `(meta (var ~sym))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        examples '(read-string (slurp (:path @exemplar.core/state)))
        data `(get ~examples ~key)]
    `(merge {:name ~key} ~data)))
