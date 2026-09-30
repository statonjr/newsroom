(ns newsroom.config
  "Where newsroom keeps its settings: ~/.config/newsroom, or $NEWSROOM_HOME.

    config.edn        sources, schedule, providers (see resources/defaults)
    prompt.md         the analysis prompt, {{date}} and {{sources}} filled in
    plugins/*.clj     loaded at startup; a plugin adds a source type
    newsroom.sqlite3  the gathered items and the briefings, by day

  The first run writes the defaults, so there is always a file to edit."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- env [k]
  (let [v (jolt.host/getenv k)]
    (when-not (str/blank? v) v)))

(defn home []
  (or (env "NEWSROOM_HOME")
      (str (env "HOME") "/.config/newsroom")))

(defn path [& parts] (str/join "/" (cons (home) parts)))

(defn- default-text [name]
  (slurp (io/resource (str "defaults/" name))))

(defn defaults []
  (edn/read-string (default-text "config.edn")))

(defn ensure-home!
  "Create the config directory with the default config and prompt, leaving
  any file that is already there alone."
  []
  (.mkdirs (io/file (path "plugins")))
  (doseq [name ["config.edn" "prompt.md"]
          :let [f (io/file (path name))]
          :when (not (.exists f))]
    (spit f (default-text name))))

(defn load-config
  "config.edn over the defaults, key by key."
  []
  (let [f (io/file (path "config.edn"))
        user (when (.exists f) (edn/read-string (slurp f)))]
    (merge (defaults) user)))

(defn prompt-template []
  (let [f (io/file (path "prompt.md"))]
    (if (.exists f) (slurp f) (default-text "prompt.md"))))

(defn db-file [config]
  (or (:db config) (path "newsroom.sqlite3")))

(defn load-plugins!
  "Load every plugins/*.clj, in name order. A plugin that fails to load is
  reported and skipped rather than keeping the server down."
  []
  (let [dir (io/file (path "plugins"))
        files (when (.exists dir)
                (sort-by #(.getName %)
                         (filter #(str/ends-with? (.getName %) ".clj") (.listFiles dir))))]
    (doall
     (for [f files]
       (try (load-file (.getPath f))
            {:plugin (.getName f) :ok true}
            (catch Throwable e
              (binding [*out* *err*]
                (println "plugin" (.getName f) "failed to load:" (ex-message e)))
              {:plugin (.getName f) :ok false :error (ex-message e)}))))))
