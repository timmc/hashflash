(defproject org.timmc/hashflash "1.0.0-SNAPSHOT"
  :description "An email autoresponder for geohashing, written in Clojure."
  :url "https://github.com/timmc/hashflash"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.timmc/geohash "1.0.0"]
                 [com.sun.mail/javax.mail "1.4.5"]
                 [com.taoensso/timbre "1.2.0"]])
