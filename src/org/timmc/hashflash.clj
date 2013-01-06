(ns org.timmc.hashflash
  (:require [org.timmc.geohash :as gh]
            [clojure.string :as str])
  (:use [taoensso.timbre :as timbre :only (trace debug info warn error fatal)])
  (:import (javax.mail Session Store Folder Message Flags$Flag)
           (javax.mail.internet MimeMessage)
           (java.util.concurrent Executors TimeUnit)
           (java.util.regex Pattern)
           (org.joda.time LocalDate)
           (org.joda.time.format DateTimeFormat)))
(.importClass *ns* 'FLAG Flags$Flag)

(timbre/set-level! :debug)

;; TODO: Reply to original message (for threading)

;;;; Mail

(def mail-session-properties
  (doto (java.util.Properties.)
    (.putAll {"mail.mime.address.strict" "true"})))

(defn get-store
  [imap-domain username password]
  (doto (-> (Session/getInstance mail-session-properties)
            (.getStore "imaps"))
    (.connect imap-domain username password)))

(defn close-store
  [store]
  (.close store))

(defn send-msg
  [m]
  )

(defn move-to
  [msg folder-name]
  (debug "Moving message to folder:" folder-name)
  (.setFlag msg FLAG/SEEN true)
  (.saveChanges msg)
  (let [dest (-> msg (.getFolder) (.getStore) (.getFolder folder-name))]
    (.appendMessages dest (into-array msg))
    (.setFlag msg FLAG/DELETED true)
    (.saveChanges msg)))

;;;; Worker thread

(def ^:private match-date #"([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})")
(def ^:private match-coord #"(-?)([0-9]{1,3})(?:\.[0-9]*)?")
(def match-inquiry
  (Pattern/compile (format "^(?s)\\s+%s %s,? %s"
                           match-date match-coord match-coord)))

(defn parse-body
  "Return {:date LocalDate, :lat double, :lon double} or nil"
  [s]
  (when-let [[_ y m d lats latm lons lonm] (re-find match-inquiry s)]
    (try
      {:date (LocalDate. (Long/parseLong y)
                         (Long/parseLong m)
                         (Long/parseLong d))
       :lat (* (if (= lats "-") -1 1)
               (Double/parseDouble latm))
       :lon (* (if (= lons "-") -1 1)
               (Double/parseDouble lonm))}
      (catch NumberFormatException nfe
        nil))))

(def djia-source-url-format
  "http://geo.crox.net/djia/%s")
(def djia-source-date-format
  (DateTimeFormat/forPattern "yyyy/MM/dd"))

(defn get-djia-url
  [date]
  (format djia-source-url-format (.print djia-source-date-format date)))

(def match-djia #"[0-9]+.[0-9]{2}")

(defn get-djia
  [date]
  (let [url (get-djia-url date)]
    (try
      (let [answer (slurp (java.net.URI. url))]
        (when (re-matches match-djia answer)
          answer))
      (catch java.io.FileNotFoundException fnfe nil))))

(defn extract-query
  "Extract a query spec, or return nil if invalid."
  [m]
  (let [ctype (.getContentType m)
        body (.getContent m)]
    (if-not (re-find #"(?i)^text/plain(;|$)" ctype)
      (info "Not plain-text.")
      (if-let [parsed (parse-body body)]
        parsed
        (info "Could not parse body.")))))

(def dms-dirsyms
  {:lat [\N \S] :lon [\E \W]})

(defn double-negative?
  "Check if a double is negative. This is harder than you might expect."
  [d]
  (neg? (Math/copySign 4.0 (double d))))

(defn dms
  "Degree-minute-second format of coordinate. Pass :lat or :lon for `orient`."
  [val orient]
  {:pre [(dms-dirsyms orient)]}
  (let [dirsym (get (dms-dirsyms orient) (if (double-negative? val) 1 0))
        val (Math/abs val)
        d (int (quot val 1))
        val (* 60 (rem val 1))
        m (int (quot val 1))
        s (* 60 (rem val 1))]
    (format "%s%d\u00b0%d'%.3f\"" dirsym d m s)))

(defn geohash-response!
  "Produce a string suitable for use as an SMS response to a geohash query.
This function will make network calls."
  [{:keys [lat lon date]}]
  (if-let [djia (get-djia (gh/dow-date lat lon date))]
    (let [[olat olon] (gh/geohash lat lon date djia)]
      (str/join "\n"
                (map (partial apply format)
                     [["Geohash for %s" (.toString date "yyyy-MM-dd")]
                      ["%.6f, %.6f" olat olon]
                      ["or"]
                      ["%s, %s" (dms olat :lat) (dms olon :lon)]])))))

(defn build-reply
  [query oldm]
  (let [session (Session/getInstance mail-session-properties)]
    (doto (MimeMessage. session)
      (.setSubject (str "Re: " (.getSubject oldm)))
      (.setContent (geohash-response! query))
      )))

(defn do-message
  "Handle message and return destination folder name."
  [m config]
  (if-let [query (extract-query m)]
    (let [response (build-reply query m)]
      ;; Mark as sent before sending -- if sending fails, we'll notice and
      ;; flag it on the next iteration.
      (.setFlag m FLAG/ANSWERED true)
      (.saveChanges m)
      (send-msg response)
      (move-to m (:done-folder config)))
    (move-to m (:reject-folder config))))

(defn check-messages
  [fldr config]
  (debug "Checking for new messages...")
  (let [msgs (.getMessages fldr)]
    (info "New messages found:" (count msgs))
    (doseq [m msgs
            ;; Ignore flagged emssages -- these need dev attention.
            :when (not (.isSet m FLAG/FLAGGED))]
      (debug "Message ID:" (seq (.getHeader m "Message-ID")))
      ;; TODO: rate-limiting
      (if (.isSet m FLAG/ANSWERED)
        (do (warn "Flagging lingering message.")
            (.setFlag m FLAG/FLAGGED true)
            (.saveChanges m))
        (do-message m config)))))

;;;; Coordination

(defn -main
  [& args]
  (let [config (binding [*read-eval* false]
                 (read-string (slurp (first args))))
        store (apply get-store ((juxt :imap-domain :username :password)
                                config))
        runtime (Runtime/getRuntime)]
    (.addShutdownHook runtime #(close-store store))
    (let [inbox (.getFolder store (:source-folder config))]
      (.addShutdownHook runtime #(when (.isOpen inbox) (.close inbox)))
      ;; create destinations if necessary
      (doseq [fname ((juxt :reject-folder :done-folder :sent-folder) config)]
        (.create (.getFolder store fname) javax.mail.Folder/HOLDS_MESSAGES))
      (.open inbox Folder/READ_WRITE)
      ;; start main loop
      (let [executor (Executors/newScheduledThreadPool 1)]
        (.addShutdownHook runtime #(.shutdown executor))
        (-> executor (.scheduleWithFixedDelay #(check-messages inbox config)
                                              0 1 TimeUnit/MINUTES))))))
