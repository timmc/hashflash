(ns org.timmc.hashflash
  (:require [org.timmc.geohash :as gh]
            [clojure.string :as str])
  (:use [taoensso.timbre :as timbre :only (trace debug info warn error fatal)])
  (:import (javax.mail Session Store Folder Message SendFailedException)
           (javax.mail.internet MimeMessage)
           (java.util.concurrent Executors TimeUnit)
           (java.util.regex Pattern)
           (org.joda.time LocalDate)
           (org.joda.time.format DateTimeFormat)))
(.importClass *ns* 'FLAG javax.mail.Flags$Flag)
(.importClass *ns* 'AddrType javax.mail.Message$RecipientType)

(timbre/set-level! :info)

;;;; Mail

(def mail-session-properties
  (doto (java.util.Properties.)
    (.putAll {"mail.mime.address.strict" "true"})))

(defn get-inbox
  "Set up and return an open inbox."
  [config]
  (trace "Getting new inbox instance...")
  (let [store (doto (-> (Session/getInstance mail-session-properties)
                        (.getStore "imaps"))
                (.connect (:imap-domain config)
                          (:username config)
                          (:password config)))]
    (trace "Connected to store...")
    (let [inbox (.getFolder store (:source-folder config))]
      (trace "Got folder...")
      ;; create destinations if necessary
      (doseq [fname ((juxt :reject-folder :done-folder) config)]
        (.create (.getFolder store fname) javax.mail.Folder/HOLDS_MESSAGES))
      (.open inbox Folder/READ_WRITE)
      (trace "And it's open!")
      inbox)))

(defn close-inbox
  [inbox]
  (when (.isOpen inbox)
    (.close inbox false))
  (.close (.getStore inbox)))

(defn send-msg
  [m config]
  (let [session (Session/getInstance mail-session-properties)
        transport (doto (.getTransport session "smtps")
                    (.connect (:smtp-domain config)
                              (:smtp-port config)
                              (:username config)
                              (:password config)))]
    (try
      (.sendMessage transport m (.getRecipients m AddrType/TO))
      true
      (catch SendFailedException sfe
        false)
      (finally (.close transport)))))

(defn move-to
  [msg folder-name]
  (debug "Moving message to folder:" folder-name)
  (.setFlag msg FLAG/SEEN true)
  (let [dest (-> msg (.getFolder) (.getStore) (.getFolder folder-name))]
    (.appendMessages dest (into-array [msg]))
    (.setFlag msg FLAG/DELETED true)))

;;;; Worker thread

(def ^:private match-date #"([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})")
(def ^:private match-coord #"(-?)([0-9]{1,3})(?:\.[0-9]*)?")
(def match-inquiry
  (Pattern/compile (format "^(?s)\\s*%s %s[, ]{1,2}%s"
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
        (info "Failed to parse a number."))
      (catch org.joda.time.IllegalFieldValueException ifve
        (info "Invalid date.")))))

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
  (let [ddate (gh/dow-date lat lon date)
        df (DateTimeFormat/forPattern "yyyy-MM-dd")]
    (if-let [djia (get-djia ddate)]
      (let [[olat olon] (gh/geohash lat lon date djia)]
        (str/join "\n"
                  (map (partial apply format)
                       [["Geohash for %s" (.print df date)]
                        ["%.6f, %.6f" olat olon]
                        ["or"]
                        ["%s, %s" (dms olat :lat) (dms olon :lon)]
                        ["DJIA = %s (from %s)" djia (.print df ddate)]]))))))

(defn build-reply
  [query oldm]
  (let [session (Session/getInstance mail-session-properties)
        from (first (.getFrom oldm))
        old-msgid (.getMessageID oldm)]
    (doto (MimeMessage. session)
      (.addRecipients AddrType/TO (into-array [from]))
      (.setSubject (str "Re: " (.getSubject oldm)))
      (.setText (geohash-response! query))
      (.setHeader "References" old-msgid)
      (.setHeader "In-Reply-To" old-msgid)
      (.setHeader "X-Mailer", "org.timmc/hashflash")
      (.setSentDate (java.util.Date.))
      (.saveChanges))))

(defn do-message
  "Handle message and return destination folder name."
  [m config]
  (if-let [query (extract-query m)]
    (let [response (build-reply query m)]
      (debug "Received query:" query)
      ;; Mark as sent before sending -- if sending fails, we'll notice and
      ;; flag it on the next iteration.
      (.setFlag m FLAG/ANSWERED true)
      (info "Sending response.")
      (when-not (send-msg response config)
        (warn "Failed to send response. Flagged.")
        (.setFlag m FLAG/FLAGGED true))
      (move-to m (:done-folder config)))
    (move-to m (:reject-folder config))))

(defn check-messages
  [config]
  (let [inbox (get-inbox config)]
    (try
      (debug "Checking for new messages...")
      (let [msgs (.getMessages inbox)]
        (when-not (zero? (count msgs))
          (info "New messages found:" (count msgs)))
        (doseq [m msgs
                ;; Ignore flagged messages -- these need dev attention.
                :when (not (.isSet m FLAG/FLAGGED))]
          (debug "Message ID:" (seq (.getHeader m "Message-ID")))
          ;; TODO: rate-limiting
          (if (.isSet m FLAG/ANSWERED)
            (do (warn "Flagging lingering message.")
                (.setFlag m FLAG/FLAGGED true))
            (do-message m config))))
      (finally (close-inbox inbox)))))

;;;; Coordination

(defn -main
  [& args]
  (let [config (binding [*read-eval* false]
                 (read-string (slurp (first args))))]
    ;; kick off main loop
    (let [executor (Executors/newScheduledThreadPool 1)]
      (.addShutdownHook (Runtime/getRuntime) (Thread. #(.shutdown executor)))
      (-> executor (.scheduleWithFixedDelay
                    #(try (check-messages config)
                          (catch Exception e
                            (.printStackTrace e)
                            (throw (RuntimeException. e))))
                    0 1 TimeUnit/MINUTES))))
  ;; don't print a return value
  nil)
