(ns newsroom.store
  "The days in sqlite. A day is its briefing (the markdown, which model
  wrote it, when) and the sources gathered for it, numbered as the briefing
  cites them; `cited` marks the ones it cites.

  One connection, serialized by a lock: sqlite writes one at a time anyway,
  and the server's handlers and the pipeline share it."
  (:require [db.jdbc]
            [jdbc.core :as jdbc]
            [jolt.time]))

(def ^:private schema
  ["create table if not exists briefings (
      day text primary key,
      markdown text not null,
      model text,
      provider text,
      created_at text not null)"
   "create table if not exists sources (
      day text not null,
      n integer not null,
      title text not null,
      url text not null,
      source text,
      summary text,
      published text,
      cited integer not null default 0,
      primary key (day, n))"])

(defn open
  "A store on the sqlite database at `uri` (a path, or sqlite::memory:)."
  [uri]
  (let [conn (jdbc/connection (if (re-find #"^sqlite:" uri) uri (str "sqlite:" uri)))]
    (doseq [stmt schema] (jdbc/execute! conn stmt))
    {:conn conn :lock (Object.)}))

(defn close [{:keys [conn]}] (.close conn))

(defmacro ^:private with-db [[conn store] & body]
  `(locking (:lock ~store)
     (let [~conn (:conn ~store)] ~@body)))

(defn- now [] (str (java.time.Instant/now)))

(defn save-day!
  "Store a day's briefing and its sources, replacing whatever the day had."
  [store {:keys [day sources cited markdown model provider]}]
  (let [cited (set cited)]
    (with-db [conn store]
      (jdbc/atomic conn
        (jdbc/execute! conn ["delete from sources where day = ?" day])
        (jdbc/execute! conn ["delete from briefings where day = ?" day])
        (jdbc/execute! conn ["insert into briefings (day, markdown, model, provider, created_at)
                              values (?, ?, ?, ?, ?)"
                             day markdown model provider (now)])
        (doseq [{:keys [n title url source summary published]} sources]
          (jdbc/execute! conn ["insert into sources (day, n, title, url, source, summary, published, cited)
                                values (?, ?, ?, ?, ?, ?, ?, ?)"
                               day n title url source summary published
                               (if (contains? cited n) 1 0)]))))
    nil))

(defn- row->source [r]
  {:n (long (:n r)) :title (:title r) :url (:url r) :source (:source r)
   :summary (:summary r) :published (:published r)})

(defn day
  "The stored day, or nil: {:day :markdown :model :provider :created-at
  :sources :cited}."
  [store day]
  (with-db [conn store]
    (when-let [b (jdbc/fetch-one conn ["select * from briefings where day = ?" day])]
      (let [rows (jdbc/fetch conn ["select * from sources where day = ? order by n" day])]
        {:day day
         :markdown (:markdown b)
         :model (:model b)
         :provider (:provider b)
         :created-at (:created_at b)
         :sources (mapv row->source rows)
         :cited (mapv row->source (filter #(= 1 (:cited %)) rows))}))))

(defn days
  "Every day with a briefing, newest first."
  [store]
  (with-db [conn store]
    (mapv :day (jdbc/fetch conn "select day from briefings order by day desc"))))

(defn prune!
  "Delete every day but the newest `n`, with its sources, and return the
  days deleted, newest first. A negative `n` keeps everything. The file is
  vacuumed after a deletion so the space goes back to the disk."
  [store n]
  (if (neg? n)
    []
    (let [gone (with-db [conn store]
                 (let [gone (mapv :day (jdbc/fetch conn ["select day from briefings order by day desc
                                                          limit -1 offset ?" n]))]
                   (when (seq gone)
                     (jdbc/atomic conn
                       (doseq [d gone]
                         (jdbc/execute! conn ["delete from sources where day = ?" d])
                         (jdbc/execute! conn ["delete from briefings where day = ?" d]))))
                   gone))]
      (when (seq gone)
        (with-db [conn store] (jdbc/execute! conn "vacuum")))
      gone)))
