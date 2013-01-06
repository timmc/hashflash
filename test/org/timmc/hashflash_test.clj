(ns org.timmc.hashflash-test
  (:use clojure.test
        org.timmc.hashflash)
  (:import org.joda.time.LocalDate))

(deftest math
  (is (= (double-negative? 0.0) false))
  (is (= (double-negative? -0.0) true))
  (is (= (double-negative? 1.0) false))
  (is (= (double-negative? -1.0) true)))

(deftest djia-sourcing
  (testing "url-building"
    (is (= (get-djia-url (LocalDate. 2008 3 25))
           "http://geo.crox.net/djia/2008/03/25")))
  (testing "validation"
    (with-redefs
      [slurp (fn [url]
               (when-not (= (str url) "http://geo.crox.net/djia/2008/03/25")
                 (throw (RuntimeException. (str "Bad URL: " url))))
               "123.45")]
      (is (= (get-djia (LocalDate. 2008 3 25)) "123.45")))
    (with-redefs [slurp (constantly "")]
      (is (= (get-djia (LocalDate. 2008 3 25)) nil)))
    (with-redefs [slurp (fn [& _]
                          (throw (java.io.FileNotFoundException. "whee")))]
      (is (= (get-djia (LocalDate. 2008 3 25)) nil)))))

(deftest formatting
  (is (= (dms 42.3851 :lat) "N42°23'6.360\""))
  (is (= (dms -0.0 :lon) "W0°0'0.000\""))
  (with-redefs [get-djia (constantly "12948.96")])
  (is (= (geohash-response! {:lat -43.0 :lon 146.0
                             :date (LocalDate. 2012 12 6)})
         "Geohash for 2012-12-06
-43.416328, 146.995790
or
S43°24'58.783\", E146°59'44.843\"")))

(deftest parsing
  (is (= (parse-body " \n 2008-03-2 42, -71.5 stuff")
         {:date (LocalDate. 2008 3 2) :lat 42.0 :lon -71.0}))
  (is (= (parse-body "hello") nil)))
