(ns exemplar.core
  (:require [local-file]))


(def state (atom {:path nil}))

(defn ns->abs-path [ns]
  (-> (the-ns ns)
    (local-file/namespace-to-source)
    (local-file/find-resource)
    (.getPath)))

(defn register-path [path]
  (swap! exemplar.core/state assoc :path path))

(defn write-out [path example]
  (let [in (slurp path)
        examples (read-string (if (= in "") "{}" in))]
    (spit path (with-out-str (clojure.pprint/pprint (merge examples example))))))

(defn rec [lines line-number form-str]
  (try
    (read-string (str form-str (nth lines line-number)))
    (catch Exception e
      (rec lines (inc line-number) (str form-str (nth lines line-number))))))

(defmacro get-source [ns name]
  (let [met (meta (ns-resolve ns name))
        abs-path (ns->abs-path ns)
        line (:line met)]
    (with-open [rdr (clojure.java.io/reader abs-path)]
      (let [lines (line-seq rdr)]
        `'~(rec lines (dec line) "")))))

(defmacro save [sexpr]
  (let [met `(meta (var ~(first sexpr)))
        fn-ns `(ns-name (:ns ~met))
        fn-name `(:name ~met)
        realized-ns (eval fn-ns)
        realized-name (eval fn-name)
        key `(clojure.string/join "/" [~fn-ns ~fn-name])
        args (vec (rest sexpr))
        source (eval `(get-source ~realized-ns ~realized-name))
        entry `{~key {:in ~args :out ~sexpr :source (str '~source) :ns ~fn-ns :name ~fn-name}}]
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

(defn save* [ns name args ^String source out]
  (let [key (clojure.string/join "/" [ns name])
        entry {key {:in (vec args) :out out :source source :ns ns :name name}}]
    (write-out (:path (deref exemplar.core/state)) entry)))

(defmacro record [func]
  `(alter-var-root (var ~func)
    (fn [~'f]
      (fn [& ~'args]
        (let [met# (meta (var ~func))
              ns# (ns-name (:ns met#))
              name# (:name met#)]
              ;called-form# (cons name# ~'args)]
            (if-not (:exemplar/recording? met#)
              (let [out# (apply ~'f ~'args)]
                (do
                  (alter-meta! (var ~func) assoc :exemplar/recording? true)
                  (save* ns# name# ~'args (str (eval `(get-source ~ns# ~name#))) out#)
                  out#))
              (apply ~'f ~'args)))))))

(defn delete-all [path]
  (spit path "{}"))
