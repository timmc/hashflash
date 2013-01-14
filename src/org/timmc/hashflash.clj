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
  "Return one of the following inquiry types, or nil:
* {:type :geohash, :date LocalDate, :lat double, :lon double}
* {:type :usage}"
  [s]
  (if-let [[_ y m d lats latm lons lonm] (re-find match-inquiry s)]
    (try
      {:type :geohash
       :date (LocalDate. (Long/parseLong y)
                         (Long/parseLong m)
                         (Long/parseLong d))
       :lat (* (if (= lats "-") -1 1)
               (Double/parseDouble latm))
       :lon (* (if (= lons "-") -1 1)
               (Double/parseDouble lonm))}
      (catch NumberFormatException nfe
        (info "Failed to parse a number."))
      (catch org.joda.time.IllegalFieldValueException ifve
        (info "Invalid date.")))
    ;; OK, let's try some other things...
    (if (re-find #"(?i)\busage\b" s)
      {:type :usage}
      nil)))

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
    (format "%s %d %d' %.3f\"" dirsym d m s)))

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

(defn basic-response
  [oldm txt]
  (let [session (Session/getInstance mail-session-properties)
        from (first (.getFrom oldm))
        old-msgid (.getMessageID oldm)]
    (doto (MimeMessage. session)
      (.addRecipients AddrType/TO (into-array [from]))
      (.setSubject "Geohash inquiry" "US-ASCII")
      (.setText txt "US-ASCII")
      (.setHeader "References" old-msgid)
      (.setHeader "In-Reply-To" old-msgid)
      (.setHeader "X-Mailer", "org.timmc/hashflash")
      (.setSentDate (java.util.Date.))
      (.saveChanges))))

(defn geohash-reply
  [query oldm]
  (basic-response oldm (geohash-response! query)))

(defn usage-reply
  [query oldm]
  (basic-response oldm "Send a message like this:\n2012-3-15 42 -71"))

(defn build-reply
  [query oldm]
  ((-> query :type {:geohash geohash-reply, :usage usage-reply})
   query oldm))

(defn do-message
  "Handle message and return destination folder name."
  [m config]
  (if-let [query (extract-query m)]
    (let [response (build-reply query m)]
      (debug "Received query:" query)
      ;; Mark as sent before sending -- if sending fails horribly,
      ;; we'll notice and flag it on the next iteration.
      (.setFlag m FLAG/ANSWERED true)
      (info "Sending response.")
      (when-not (send-msg response config)
        (warn "Failed to send response. Flagged.")
        (.setFlag m FLAG/FLAGGED true))
      (move-to m (:done-folder config)))
    (move-to m (:reject-folder config))))

(defonce request-reprocess (atom false))

(defn check-messages
  [config]
  (let [inbox (get-inbox config)]
    (try
      (debug "Checking for new messages...")
      (let [msgs (try
                   (.getMessages inbox)
                   (catch javax.mail.MessagingException me
                     (warn "Could not fetch messages:" (str me))
                     []))]
        (when @request-reprocess
          (trace "Reprocessing.")
          (doseq [m msgs]
            (.setFlag m FLAG/FLAGGED false)
            (.setFlag m FLAG/ANSWERED false))
          (reset! request-reprocess false))
        (if (zero? (count msgs))
          (debug "No new messages.")
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

(defonce threadpool (atom nil))

(defn schedule-worker
  [config]
  (trace "Stopping existing worker")
  (when-let [tp @threadpool]
    (.shutdown tp))
  (trace "Starting worker")
  (let [executor (Executors/newScheduledThreadPool 1)]
    (reset! threadpool executor)
    (-> executor (.scheduleWithFixedDelay
                  #(try (check-messages config)
                        (catch Exception e
                          (error e "Worker threw exception, continuing.")))
                  0 1 TimeUnit/MINUTES))))

(defn -main
  [& args]
  (when (zero? (count args))
    (println "Please specify a config file as the first argument.")
    (System/exit 1))
  (let [config (binding [*read-eval* false]
                 (read-string (slurp (first args))))]
    (timbre/set-level! (get config :log-level :info))
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. #(when-let [tp @threadpool]
                                  (.shutdown tp))))
    ;; kick off main loop
    (schedule-worker config))
  ;; don't print a return value
  nil)
