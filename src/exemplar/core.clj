(ns exemplar.core
  (:require [local-file]))

(defn my-func [xs] (map inc xs))

(def state (atom {:path nil
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

(defn write-out
  "Writes to persist path, merging into the existing persisted data"
  [path m]
  (let [in (slurp path)
        persisted (read-string (if (= in "") "{}" in))]
    (spit path (with-out-str (clojure.pprint/pprint (merge persisted m))))))

(defn rec
  "Recur target for get-source"
  [lines line-number form-str]
  (try
    (read-string (str form-str (nth lines line-number)))
    (catch Exception e
      (rec lines (inc line-number) (str form-str (nth lines line-number))))))

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
        source (eval `(get-source ~realized-ns ~realized-name))
        entry `{~key {:in ~args :out ~sexpr :source (str '~source) :ns ~fn-ns :name ~fn-name}}]
    `(write-out (:path (deref exemplar.core/state)) ~entry)))

(defmacro run
  "Calls the provided function with the persisted input"
  [sym]
  (let [met `(meta (var ~sym))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        examples '(read-string (slurp (:path @exemplar.core/state)))
        data `(get ~examples ~key)
        ex `(apply ~sym (:in ~data))]
    ex))

(defmacro show
  "Returns the persisted data for a function"
  [sym]
  (let [met `(meta (var ~sym))
        key `(clojure.string/join "/" [(ns-name (:ns ~met)) (:name ~met)])
        examples '(read-string (slurp (:path @exemplar.core/state)))
        data `(get ~examples ~key)]
    `(merge {:name ~key} ~data)))

(defn write-mem
  [entry]
  (let [key (ffirst entry)
        value (second (first entry))]
    (swap! exemplar.core/state assoc-in [:entries key] value)))

(defn save*
  "Same as `save` but used internally in different compilation layer"
  [ns name args ^String source out]
  (let [key (clojure.string/join "/" [ns name])
        entry {key {:in (vec args) :out out :source source :ns ns :name name}}]
    (write-out (:path @exemplar.core/state) entry)))

(defn save-mem [ns name var-val]
  (let [key (clojure.string/join "/" [ns name])
        entry {key {:ns ns :name name :var-val var-val}}]
    (write-mem entry)))

(defmacro stop-recording
  "Stops persisting inputs to the provided function"
  [func]
  `(let [cur-var# (var ~func)
         met# (meta cur-var#)
         ns# (ns-name (:ns met#))
         name# (:name met#)
         key# (clojure.string/join "/" [ns# name#])
         old-var-val# (get-in @exemplar.core/state [:entries key# :var-val])]
     (do
       (alter-meta! cur-var# dissoc :exemplar/recording?)
       (alter-var-root cur-var# (fn [~'f] old-var-val#)))))

(defmacro record-once
  "Persists first input and output of the provided function while allowing it to work normally.
   Restores the initial var's value on the second call to the fn"
  [func]
  `(alter-var-root (var ~func)
     (fn [~'f]
       (let [the-var# (var ~func)
             var-val# @the-var#
             met# (meta the-var#)
             ns# (ns-name (:ns met#))
             name# (:name met#)]
         (save-mem ns# name# var-val#))
       (fn [& ~'args]
         (let [met# (meta (var ~func))
               ns# (ns-name (:ns met#))
               name# (:name met#)]
           (cond
             ;; unset - set recording to true, persist data, write var to mem, rt out
             (= nil (:exemplar/recording? met#))
             (let [out# (apply ~'f ~'args)]
               (do
                 (alter-meta! (var ~func) assoc :exemplar/recording? true)
                 (save* ns# name# ~'args (str (eval `(get-source ~ns# ~name#))) out#)
                 out#))
             ;; set true - unset recording and return orig var
             (= true (:exemplar/recording? met#))
             (apply (stop-recording ~func) ~'args)))))))

(defmacro record
  "Repeatedly persists input and output of the provided function while allowing it to work normally.
   Restores the initial var's value on explicit call to stop-recording"
  [func]
  `(alter-var-root (var ~func)
     (fn [~'f]
       (let [the-var# (var ~func)
             var-val# @the-var#
             met# (meta the-var#)
             ns# (ns-name (:ns met#))
             name# (:name met#)]
         (save-mem ns# name# var-val#))
       (fn [& ~'args]
         (let [met# (meta (var ~func))
               ns# (ns-name (:ns met#))
               name# (:name met#)]
           (when (nil? (:exemplar/recording? met#))
             (alter-meta! (var ~func) assoc :exemplar/recording? true))
           (let [out# (apply ~'f ~'args)]
             (do
               ;; TODO. Only need to get the source once. Could store in meta.
               (save* ns# name# ~'args (str (eval `(get-source ~ns# ~name#))) out#)
               out#)))))))


(defn delete-all [path]
  (spit path "{}"))

;(register-path "test.edn")
;(record my-func)
;(my-func [1 2 7])
;(stop-recording my-func)
;(record-once my-func)
;;@state
;(meta #'my-func)
;@#'my-func
;(defn my-func-orig [xs] (map dec xs))
;(alter-var-root #'my-func (fn [_] @#'my-func-orig))
;;(apply @#'my-func [[1 2 3]])


;; TODO.
;; 1. Record with atom/impure functions
;; 2. Record a namespace
;; 3. --Record until explicit stop--
