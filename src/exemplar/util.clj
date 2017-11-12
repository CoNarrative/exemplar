(ns exemplar.util)


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

(defn apply-to-forms-in-file [filepath f]
  (with-open [rdr (clojure.java.io/reader filepath)]
    (with-open [wrt (clojure.java.io/writer (str filepath ".tmp"))]
      (loop [acc-form-str ""]
        (if-let [line (.readLine rdr)]
          ;; try to read form when line we're on is at or above where we want to start and haven't read the form
          (let [lines (str acc-form-str line)
                form (try (clojure.edn/read-string lines) (catch Exception e nil))]
            (if-not form
              ;; Form must be on multiple lines, so recur with lines we've read so far
              (recur lines)
              ;; Parsed a form. Write application of f if not nil. Recur with empty form buffer
              (let [new-form (str (f form))]
                (when new-form
                  (doto wrt (.write new-form) (.newLine)))
                (recur ""))))))))
  (.renameTo
    (clojure.java.io/file (str filepath ".tmp"))
    (clojure.java.io/file filepath)))
