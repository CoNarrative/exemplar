(ns exemplar.core
    (:require [local-file]
              [clojure.pprint]
              [clojure.edn :as edn]
              [clojure.repl :as repl]))


(def state (atom {:path nil
                  :debug? false
                  :entries {}}))

(defn ns->abs-path [ns]
  (-> (the-ns ns)
    (local-file/namespace-to-source)
    (local-file/find-resource)
    (.getPath)))

(defn register-path
  "Register the file to which data will be persisted"
  [path]
  (swap! exemplar.core/state assoc :path path))

(defrecord UnreadableTag [tag value])

(defn string-reader [x]
  (edn/read-string {:default ->UnreadableTag}
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contributed by Dominic Monroe
(def ^:dynamic *original-dispatch* nil)

(defmulti my-dispatch class)

(defmethod my-dispatch clojure.lang.IDeref
  [obj]
  (print obj))

(defmethod my-dispatch :default
  [obj]
  (*original-dispatch* obj))

(defn my-pprint [x]
  (binding [*original-dispatch* clojure.pprint/*print-pprint-dispatch*
            clojure.pprint/*print-pprint-dispatch* my-dispatch]
    ;; ^^ turn the binding into a macro, see the source for clojure.pprint/with-pprint-dispatch or similar
    (clojure.pprint/pprint x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pretty-demunge
  [fn-object]
  (let [dem-fn (repl/demunge (str fn-object))
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty pretty dem-fn)))

(defn defunc [xs]
  (mapv (fn [x]
          (if (fn? x)
            (symbol (pretty-demunge (str x)))
            x))
        xs))

(defn write-out
  "Writes to persist path, merging into the existing persisted data"
  [path m]
  (let [in (slurp path)
        m (into {}
                (for [[k v] m]
                  [k (update v :in defunc)]))
        persisted (string-reader (if (= in "") "{}" in))]
    (spit path (with-out-str (my-pprint (merge persisted m))))))

(defn rec
  "Recur target for get-source"
  [lines line-number form-str]
  (try
    (read-string (str form-str (nth lines line-number)))
    (catch Exception e
      (try
        (rec lines (inc line-number) (str form-str (nth lines line-number)))
        (catch IndexOutOfBoundsException e2
          "<unknown>")))))

(defmacro get-source
  "Gets the source code for a function"
  [ns name]
  (let [met (meta (ns-resolve ns name))
        abs-path (ns->abs-path ns)
        line (:line met)]
    (with-open [rdr (clojure.java.io/reader abs-path)]
      (let [lines (line-seq rdr)]
        `'~(rec lines (dec line) "")))))

(defmacro save
  "Persists a function call"
  [sexpr]
  (let [met `(meta (var ~(first sexpr)))
        fn-ns `(ns-name (:ns ~met))
        fn-name `(:name ~met)
        realized-ns (eval fn-ns)
        realized-name (eval fn-name)
        key `(clojure.string/join "/" [~fn-ns ~fn-name])
        args (vec (rest sexpr))
        source (or (try (eval `(get-source ~realized-ns ~realized-name)) (catch Exception ex))
                   (repl/source-fn (first sexpr)))
        entry `{~key {:in ~args :out ~sexpr :source (str '~source) :ns ~fn-ns :name ~fn-name}}]
    `(write-out (:path (deref exemplar.core/state)) ~entry)))

(defn save*
  "Same as `save` but used internally in different compilation layer"
  [ns name args ^String source out]
  (let [key (clojure.string/join "/" [ns name])
        entry {key {:in (vec args) :out out :source source :ns ns :name name}}]
    (write-out (:path @exemplar.core/state) entry)))

(defmacro run
  "Calls the provided function with the persisted input"
  [sym]
  (let [met `(meta (var ~sym))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        examples '(exemplar.core/string-reader (slurp (:path @exemplar.core/state)))
        data `(get ~examples ~key)
        ex `(apply ~sym (:in ~data))]
    ex))

(defmacro show
  "Returns the persisted data for a function"
  [sym]
  (let [met `(meta (var ~sym))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        examples '(exemplar.core/string-reader (slurp (:path @exemplar.core/state)))
        data `(get ~examples ~key)]
    `(merge {:name ~key} ~data)))

(defn write-mem
  [entry]
  (let [key (ffirst entry)
        value (second (first entry))]
    (swap! exemplar.core/state assoc-in [:entries key] value)))

(defn save-mem [ns name var-val]
  (let [key (clojure.string/join "/" [ns name])
        entry {key {:ns ns :name name :var-val var-val}}]
    (write-mem entry)))

(defmacro stop-recording
  "Stops persisting data for a function. Accepts a symbol."
  [sym]
  `(let [cur-var# (var ~sym)
         met# (meta cur-var#)
         ns# (ns-name (:ns met#))
         name# (:name met#)
         key# (clojure.string/join "/" [ns# name#])
         old-var-val# (get-in @exemplar.core/state [:entries key# :var-val])]
     (do
       (alter-meta! cur-var# dissoc :exemplar/recording?)
       (alter-var-root cur-var# (fn [~'f] old-var-val#)))))

(defn stop-recording*
  "Stops persisting data for a function. Accepts a var."
  [avar]
  (let [met (meta avar)
        ns (ns-name (:ns met))
        name (:name met)
        key (clojure.string/join "/" [ns name])
        old-var-val (get-in @exemplar.core/state [:entries key :var-val])]
    (do
      (alter-meta! avar dissoc :exemplar/recording?)
      (alter-var-root avar (fn [f] old-var-val)))))

(defmacro record-once
  "Persists first input and output of the provided function while allowing it to work normally.
   Restores the initial var's value on the second call to the fn"
  [sym]
  `(alter-var-root (var ~sym)
     (fn [~'f]
       (let [the-var# (var ~sym)
             var-val# @the-var#
             met# (meta the-var#)
             ns# (ns-name (:ns met#))
             name# (:name met#)]
         (save-mem ns# name# var-val#))
       (fn [& ~'args]
         (let [met# (meta (var ~sym))
               ns# (ns-name (:ns met#))
               name# (:name met#)]
           (cond
             ;; unset - set recording to true, persist data, write var to mem, rt out
             (= nil (:exemplar/recording? met#))
             (let [out# (apply ~'f ~'args)]
               (do
                 (alter-meta! (var ~sym) assoc :exemplar/recording? true)
                 (save* ns# name# ~'args (str (eval `(get-source ~ns# ~name#))) out#)
                 out#))
             ;; set true - unset recording and return orig var
             (= true (:exemplar/recording? met#))
             (apply (stop-recording ~sym) ~'args)))))))

(defn record-once*
  "Persists first input and output of the provided function while allowing it to work normally.
   Restores the initial var's value on the second call to the fn"
  [avar]
  (alter-var-root avar
    (fn [f]
      (let [the-var avar
            var-val @the-var
            met (meta the-var)
            ns (ns-name (:ns met))
            name (:name met)]
        (save-mem ns name var-val))
      (fn [& args]
        (let [met (meta avar)
              ns (ns-name (:ns met))
              name (:name met)]
          (cond
            ;; unset - set recording to true, persist data, write var to mem, rt out
            (= nil (:exemplar/recording? met))
            (let [out (apply f args)]
              (do
                (alter-meta! avar assoc :exemplar/recording? true)
                (save* ns name args (str (eval `(get-source ~ns ~name))) out)
                out))
            ;; set true - unset recording and return orig var
            (= true (:exemplar/recording? met))
            (apply (stop-recording* avar) args)))))))

(defmacro record
  "Repeatedly persists input and output of the provided function while allowing it to work normally.
   Restores the initial var's value on explicit call to stop-recording"
  [sym]
  `(alter-var-root (var ~sym)
     (fn [~'f]
       (let [the-var# (var ~sym)
             var-val# @the-var#
             met# (meta the-var#)
             ns# (ns-name (:ns met#))
             name# (:name met#)]
         (save-mem ns# name# var-val#))
       (fn [& ~'args]
         (let [met# (meta (var ~sym))
               ns# (ns-name (:ns met#))
               name# (:name met#)]
           (when (nil? (:exemplar/recording? met#))
             (alter-meta! (var ~sym) assoc :exemplar/recording? true))
           (let [out# (apply ~'f ~'args)]
             (do
               ;; TODO. Only need to get the source once. Could store in meta.
               (save* ns# name# ~'args (str (eval `(get-source ~ns# ~name#))) out#)
               out#)))))))

(defn record*
  "Repeatedly persists input and output of the provided function while allowing it to work normally.
   Restores the initial var's value on explicit call to stop-recording"
  [avar]
  (alter-var-root avar
     (fn [f]
       (let [var-val @avar
             met (meta avar)
             ns (ns-name (:ns met))
             name (:name met)]
         (save-mem ns name var-val))
       (fn [& args]
         (let [met (meta avar)
               ns (ns-name (:ns met))
               name (:name met)]
           (when (nil? (:exemplar/recording? met))
             (alter-meta! avar assoc :exemplar/recording? true))
           (let [out (apply f args)]
             (do
               ;; TODO. Only need to get the source once. Could store in meta.
               (save* ns name args (str (eval `(get-source ~ns ~name))) out)
               out)))))))


(defn disambiguate-ns-decl
  ([sym]
   (try
     (eval `(if (fn? ~sym) {:symbol '~sym :fn? true} {:symbol '~sym :def? true}))
     (catch Exception e
       (if (clojure.string/includes? (.getMessage e) "Can't take value of a macro")
         {:symbol sym :macro? true}
         (do (println "Unhandled exception" e)
             nil)))))
  ([sym avar]
   (try
     (eval `(if (fn? ~sym)
              {:symbol '~sym :var '~avar :fn? true}
              {:symbol '~sym :var '~avar :def? true}))
     (catch Exception e
       (if (clojure.string/includes? (.getMessage e) "Can't take value of a macro")
         {:symbol sym :macro? true}
         (do (println "Unhandled exception" e)
             nil))))))

(defmacro get-decl-types [ns-sym]
  (let [x (the-ns ns-sym)
        namespace-name (.getName x)
        interns (ns-interns x)
        m (reduce (fn [acc [sym var]]
                    (let [fqsym (symbol (clojure.string/join "/" [namespace-name sym]))]
                      (conj acc [fqsym var])))
            []
            interns)]
    `(->> '~m
       (mapv ~'(fn [[k v]] (exemplar.core/disambiguate-ns-decl k v))))))

(defmacro record-namespace-once [sym]
  (let [types-of-decls `(exemplar.core/get-decl-types ~sym)]
    `(->> ~types-of-decls
      (filter :fn?)
      (map :var)
      (run! exemplar.core/record-once*))))

(defmacro record-namespace [sym]
  (let [types-of-decls `(exemplar.core/get-decl-types ~sym)]
    `(->> ~types-of-decls
       (filter :fn?)
       (map :var)
       (run! exemplar.core/record*))))

(defmacro stop-recording-namespace [sym]
  (let [types-of-decls `(exemplar.core/get-decl-types ~sym)]
    `(->> ~types-of-decls
       (filter :fn?)
       (map :var)
       (run! exemplar.core/stop-recording*))))

(defn delete-all [path]
  (spit path "{}"))
