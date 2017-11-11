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
