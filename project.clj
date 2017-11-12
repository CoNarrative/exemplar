(defproject com.conarrative/exemplar "0.0.2-alpha"
  :description "Record inputs and outputs."
  :url "https://github.com/CoNarrative/exemplar.git"
  :license {:name "Apache 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [local-file "0.1.0"]]
  :plugins [[lein-cloverage "1.0.10"]]
  :test-paths ["test" "generated/test"]
  :deploy-repositories [["releases"  {:sign-releases false
                                      :url "https://clojars.org/repo"}]
                        ["snapshots" {:sign-releases false
                                      :url "https://clojars.org/repo"}]])