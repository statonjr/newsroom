(ns newsroom.llm.sse
  "A server-sent event stream read off a socket, from samizdat.api.sse: the
  response head, the chunked body, and the event lines inside it.

  BYTES, NOT CHARACTERS, until a line is whole. A chunk's size is octets and a
  multibyte character can be cut across two chunks; a line ends at a newline
  byte, which never occurs inside a UTF-8 sequence, so a line is decoded once
  and whole."
  (:require [clojure.string :as str]))

;; --- the parser --------------------------------------------------------------

(defn reader
  "A fresh parser: feed it the bytes of one connection with `feed`.

  `:keep-body? true` keeps a response that is not an event stream — a status
  other than 200, or a 200 with some other content type — as its raw body
  (`:raw`, `body-text`) rather than giving it up: a provider's error, or a
  provider that answered a streamed request whole."
  ([] (reader nil))
  ([{:keys [keep-body?]}]
   {:phase :head :buf [] :status nil :headers {} :chunked? false
    :need nil :skip 0 :line [] :event {} :done? false
    :keep-body? (boolean keep-body?) :raw? false :raw []}))

(defn- utf8 [bs] (String. (byte-array bs) "UTF-8"))

(defn- head-end
  "Index just past the blank line ending the head in `buf`, or nil."
  [buf]
  (let [n (count buf)]
    (loop [i 3]
      (cond (>= i n) nil
            (and (= 13 (nth buf (- i 3))) (= 10 (nth buf (- i 2)))
                 (= 13 (nth buf (- i 1))) (= 10 (nth buf i))) (inc i)
            :else (recur (inc i))))))

(defn- dispatch
  "The event a blank line completes, or nil when it carried no data."
  [{:keys [data] :as ev}]
  (when (seq data)
    (cond-> {:data (str/join "\n" data)}
      (:id ev) (assoc :id (:id ev))
      (:event ev) (assoc :event (:event ev)))))

(defn- on-line
  "One whole event-stream line into state `st`, collecting into `out`."
  [st out line]
  (let [line (if (str/ends-with? line "\r") (subs line 0 (dec (count line))) line)]
    (cond
      (= "" line)
      [(assoc st :event {}) (if-let [e (dispatch (:event st))] (conj out e) out)]

      (str/starts-with? line ":") [st out]

      :else
      (let [i (str/index-of line ":")
            field (if i (subs line 0 i) line)
            v (if i (subs line (inc i)) "")
            v (if (str/starts-with? v " ") (subs v 1) v)]
        [(case field
           "data" (update-in st [:event :data] (fnil conj []) v)
           "id" (assoc-in st [:event :id] v)
           "event" (assoc-in st [:event :event] v)
           st)
         out]))))

(defn- payload
  "Body bytes (already de-chunked) into event lines."
  [st out bs]
  (reduce (fn [[st out] b]
            (if (= 10 b)
              (on-line (assoc st :line []) out (utf8 (:line st)))
              [(update st :line conj b) out]))
          [st out] bs))

(defn- sink
  "Body bytes to where they go: the raw body, or the event lines."
  [st out bs]
  (if (:raw? st) [(update st :raw into bs) out] (payload st out bs)))

(defn- chunked
  "De-chunk `bs` into the event parser."
  [st out bs]
  (loop [st st, out out, bs (seq bs)]
    (cond
      (or (nil? bs) (:done? st)) [st out]

      (pos? (:skip st)) (recur (update st :skip dec) out (next bs))

      (nil? (:need st))
      (let [b (first bs)]
        (if (= 10 b)
          (let [size-line (str/trim (first (str/split (utf8 (:buf st)) #";")))
                n (Long/parseLong (if (str/blank? size-line) "0" size-line) 16)]
            (if (zero? n)
              [(assoc st :done? true :buf []) out]
              (recur (assoc st :need n :buf []) out (next bs))))
          (recur (update st :buf conj b) out (next bs))))

      :else
      (let [k (min (:need st) (count bs))
            [st out] (sink st out (take k bs))
            left (- (:need st) k)]
        (recur (if (zero? left) (assoc st :need nil :skip 2) (assoc st :need left))
               out (seq (drop k bs)))))))

(defn- body [st out bs]
  (if (:chunked? st) (chunked st out bs) (sink st out bs)))

(defn- head-fields
  "The response head's header fields, names lower-cased."
  [head]
  (into {} (for [line (rest (str/split-lines head))
                 :let [i (str/index-of line ":")]
                 :when i]
             [(str/lower-case (str/trim (subs line 0 i))) (str/trim (subs line (inc i)))])))

(defn body-text
  "A kept body (`:keep-body?`) as text."
  [st]
  (utf8 (:raw st)))

(defn feed
  "Feed `bytes` to parser state `st`. Returns {:state :events}: the events
  those bytes completed, each {:data} with :id and :event when the stream
  named them. `:status` on the state once the head is in; `:done?` when the
  stream ended or was refused (a status other than 200)."
  [st bytes]
  (let [bs (vec bytes)]
    (if (= :head (:phase st))
      (let [buf (into (:buf st) bs)]
        (if-let [end (head-end buf)]
          (let [head (utf8 (subvec buf 0 end))
                status (some-> (re-find #"^HTTP/\d\.\d (\d{3})" head) second parse-long)
                fields (head-fields head)
                chunked? (boolean (some-> (get fields "transfer-encoding") str/lower-case
                                          (str/includes? "chunked")))
                events? (boolean (some-> (get fields "content-type") str/lower-case
                                         (str/includes? "text/event-stream")))
                raw? (and (:keep-body? st) (or (not= 200 status) (not events?)))
                st (assoc st :phase :body :buf [] :status status :headers fields
                          :chunked? chunked? :raw? raw?)]
            (if (or raw? (= 200 status))
              (let [[st out] (body st [] (subvec buf end))]
                {:state st :events out})
              {:state (assoc st :done? true) :events []}))
          {:state (assoc st :buf buf) :events []}))
      (let [[st out] (body st [] bs)]
        {:state st :events out}))))

(defn parse-url
  "`url` as {:tls? :host :port :path}, the port defaulting by scheme."
  [url]
  (let [[_ scheme host port path] (re-find #"^(https?)://([^:/]+)(?::(\d+))?(/.*)?$" (str url))
        tls? (= "https" scheme)]
    {:tls? tls? :host host :port (or (some-> port parse-long) (if tls? 443 80))
     :path (or path "/")}))
