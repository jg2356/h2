(defproject h2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.twitter/hpack "v1.0.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [bytebuffer "0.2.0"]
                 [clojurewerkz/buffy "1.0.2"]
                 [slingshot "0.12.2"]]
  :repositories [["libs-release" "http://artifactory.rd.bms.com/artifactory/libs-release/"]
                 ["libs-snapshot" "http://artifactory.rd.bms.com/artifactory/libs-snapshot/"]]
  :main ^:skip-aot h2.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
