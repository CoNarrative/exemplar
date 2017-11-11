(ns exemplar.util)


;; From clojure.contrib
(defmacro while-let
  "Makes it easy to continue processing an expression as long as it is true"
  [binding & forms]
  `(loop []
     (when-let ~binding
       ~@forms
       (recur))))

(defmacro when-let*
  "Multiple binding version of when-let"
  [bindings & body]
  (if (seq bindings)
    `(when-let [~(first bindings) ~(second bindings)]
       (when-let* ~(vec (drop 2 bindings)) ~@body))
    `(do ~@body)))

(defmacro while-let*
  "Runs body in a loop while all bindings are true"
  [bindings & forms]
  `(loop []
     (when-let* ~bindings
       ~@forms
       (recur))))

(defn read-chars [file-path]
  (let [stream (java.io.PushbackReader. (clojure.java.io/reader file-path))
        reading? (atom true)
        string-builder (StringBuilder.)]
    (while @reading?
      (let [n (.read stream)]
        (if (= n -1)
          (do (reset! reading? false)
              (.close stream))
          (.append string-builder (char n)))))
    (.toString string-builder)))

(defn apply-to-form-at-line [filepath start f]
  (with-open [rdr (clojure.java.io/reader filepath)]
    (with-open [wrt (clojure.java.io/writer (str filepath ".tmp"))]
      (loop [line-number 1
             acc-form-str ""
             form-read? false]
        (if-let [line (.readLine rdr)]
          (cond
            ;; try to read form when line we're on is at or above where we want to start and haven't read the form
            (and (>= line-number start)
              (not form-read?))
            (let [lines (str acc-form-str line)
                  form (try (clojure.edn/read-string lines) (catch Exception e nil))]
              (if form
                ;; Parsed target form. Write result of calling f and recur with form-read? true
                (do
                  (println "Got form " form line-number)
                  (doto wrt (.write (str (f form))) (.newLine))
                  (recur -1 nil true))
                ;; Form must be on multiple lines, so recur with lines we've read so far
                (recur
                  (inc line-number)
                  lines
                  false)))

            form-read?
            ;; pass everything else through
            (do
              (doto wrt (.write line) (.newLine))
              (recur -1 nil true))

            :else   (println "unhandled"))))))
  (.renameTo
    (clojure.java.io/file (str filepath ".tmp"))
    (clojure.java.io/file filepath)))

(defn ensure-require
  "Adds namespace to top-level ns/require form if not present."
  [form ns]
  (let [[ns-decl require-statement] (partition-by #(not (seq? %)) form)]
    (if-let [exists? (boolean (some #{[ns]} (first require-statement)))]
      form
      `(~@ns-decl ~(concat (first require-statement) [[ns]])))))
