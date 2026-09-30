(ns newsroom.pipeline-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ebb.core :as m]
            [newsroom.pipeline :as pipeline]
            [newsroom.sources :as sources]
            [newsroom.store :as store]))

(defn- item [n day]
  {:title (str "Story " n) :url (str "https://e.com/" n) :source "Fixture"
   :summary (str "About " n) :published (str day "T08:00:00Z")})

(defmethod sources/fetch-items ::fixture [{:keys [ns name]} {:keys [day] :as ctx}]
  (sources/emit! ctx (str "Reading fixture " name) {:url (str "https://e.com/feed/" name)})
  (mapv #(item % day) ns))

(defmethod sources/fetch-items ::slow [_ _]
  (Thread/sleep 5000)
  [(item 99 "2026-09-30")])

(defmethod sources/fetch-items ::broken [_ _]
  (throw (ex-info "feed is down" {})))

(def dir (str (System/getProperty "java.io.tmpdir") "/newsroom-test-" (System/currentTimeMillis)))

(defn- ctx [sources chat]
  {:config {:sources sources :source-timeout-ms 500 :lookback-days 1
            :max-items-per-source 2 :max-items 10
            :providers {} :roles {:analyst :local}}
   :store (store/open "sqlite::memory:")
   :template "Brief {{date}}.\n\n{{sources}}"
   :markdown-dir dir
   :chat chat})

(deftest a-run-gathers-analyses-and-stores-the-day
  (let [prompts (atom [])
        c (ctx [{:type ::fixture :name "A" :ns [1 2 3]}
                {:type ::fixture :name "B" :ns [2 4]}
                {:type ::slow :name "Slow"}
                {:type ::broken :name "Broken"}]
               (fn [llm req]
                 (swap! prompts conj (-> req :messages first :content))
                 (doseq [d [{:reasoning "Let me "} {:reasoning "think."}
                            {:text "# Today\n\n"} {:text "## The mechanics\n\nOne [1], "}
                            {:text "two [2, 3], unknown [9]."}]]
                   ((:on-delta req) d))
                 {:content "# Today\n\n## The mechanics\n\nOne [1], two [2, 3], unknown [9]."
                  :model "fake"}))
        summary (m/? (pipeline/run-task c "2026-09-30"))
        day (store/day (:store c) "2026-09-30")]
    (testing "each source is capped, and a story two sources carry is kept once"
      (is (= ["https://e.com/1" "https://e.com/2" "https://e.com/4"] (map :url (:sources day)))))
    (testing "a slow and a broken source cost only their own items"
      (is (= :failed (get-in @pipeline/status [:sources "Slow" :state])))
      (is (str/includes? (get-in @pipeline/status [:sources "Slow" :error]) "timed out"))
      (is (= "feed is down" (get-in @pipeline/status [:sources "Broken" :error]))))
    (testing "the prompt numbers the sources"
      (is (str/starts-with? (first @prompts) "Brief 30 September 2026 (2026-09-30)."))
      (is (str/includes? (first @prompts) "[3] Story 4 (Fixture)")))
    (testing "the briefing links its citations and lists what it cited"
      (is (str/includes? (:markdown day) "One [[1]](https://e.com/1)"))
      (is (str/includes? (:markdown day) "unknown [9]"))
      (is (= [1 2 3] (map :n (:cited day))))
      (is (= "fake" (:model day)))
      (is (= "local" (:provider day))))
    (is (= (:markdown day) (slurp (io/file dir "2026-09-30.md"))))
    (testing "the run tells the page what it is doing"
      (let [events (:events @pipeline/status)
            texts (map :text events)
            has? (fn [s] (some #(str/includes? % s) texts))]
        (is (has? "Reading fixture A"))
        (is (some #(= "https://e.com/feed/A" (:url %)) events) "a fetch links to what it reads")
        (is (has? "A: 3 items"))
        (is (has? "Broken failed: feed is down"))
        (is (has? "Slow timed out"))
        (is (has? "3 stories to analyse"))
        (is (has? "Asking local (local-model) to write the briefing"))
        (is (has? "The model is thinking"))
        (is (has? "The model is writing"))
        (is (has? "Filed the briefing: 3 of 3 sources cited"))
        (is (every? :at events))
        (is (= 1 (count (filter #(str/includes? % "is writing") texts))) "said once, not per delta"))
      (let [w (:writing @pipeline/status)]
        (is (= 6 (:words w)) "words of prose, not markup or citations")
        (is (= "The mechanics" (:section w)))
        (is (str/ends-with? (:tail w) "unknown [9]."))))
    (is (= {:day "2026-09-30" :items 3 :cited 3} (select-keys summary [:day :items :cited])))))

(deftest a-run-with-nothing-gathered-fails-and-stores-nothing
  (let [c (ctx [{:type ::broken :name "Broken"}] (fn [_ _] (throw (ex-info "not called" {}))))]
    (is (thrown-with-msg? Exception #"no items were gathered"
                          (m/? (pipeline/run-task c "2026-09-30"))))
    (is (nil? (store/day (:store c) "2026-09-30")))))

(deftest a-failed-model-call-stores-nothing
  (let [c (ctx [{:type ::fixture :name "A" :ns [1]}]
               (fn [_ _] (throw (ex-info "DeepSeek 401: bad key" {}))))]
    (is (thrown-with-msg? Exception #"401" (m/? (pipeline/run-task c "2026-09-30"))))
    (is (nil? (store/day (:store c) "2026-09-30")))))

(deftest only-one-run-at-a-time-and-a-cancel-stops-it
  (let [c (ctx [{:type ::slow :name "Slow"}] (fn [_ _] {:content "x [1]"}))]
    (is (pipeline/start-run! c "2026-09-30"))
    (is (not (pipeline/start-run! c "2026-09-30")) "a second run is refused")
    (is (pipeline/cancel-run!))
    (loop [i 0] (when (and (pipeline/running?) (< i 50)) (Thread/sleep 20) (recur (inc i))))
    (is (not (pipeline/running?)))
    (is (= :cancelled (:state @pipeline/status)))
    (is (nil? (store/day (:store c) "2026-09-30")))))

(deftest the-item-cap-trims-every-source-evenly
  (let [results [{:items (mapv #(item % "2026-09-30") [1 2 3])}
                 {:items (mapv #(item % "2026-09-30") [10 11])}
                 {:items []}
                 {:items (mapv #(item % "2026-09-30") [20])}]]
    (is (= ["https://e.com/1" "https://e.com/10" "https://e.com/20"
            "https://e.com/2" "https://e.com/11" "https://e.com/3"]
           (map :url (pipeline/day-items results {:max-items-per-source 5} "2026-09-30"))))))

(deftest the-window-runs-from-the-lookback-to-the-next-day
  (let [dated (fn [n day] (assoc (item n day) :published (str day "T08:00:00Z")))
        results [{:items [(dated 1 "2026-09-28") (dated 2 "2026-09-29") (dated 3 "2026-09-30")
                          (dated 4 "2026-10-01") (dated 5 "2026-10-02")]}]]
    (is (= ["https://e.com/2" "https://e.com/3" "https://e.com/4"]
           (map :url (pipeline/day-items results {:lookback-days 1 :max-items-per-source 9}
                                         "2026-09-30"))))))

(deftest a-model-that-runs-out-of-tokens-says-so
  (let [c (ctx [{:type ::fixture :name "A" :ns [1]}]
               (fn [_ _] {:content "" :reasoning "lots" :finish-reason "length"}))]
    (is (thrown-with-msg? Exception #"whole token budget .* raise :max-tokens"
                          (m/? (pipeline/run-task c "2026-09-30"))))))

(deftest a-run-drops-the-days-past-the-limit
  (let [c (-> (ctx [{:type ::fixture :name "A" :ns [1]}] (fn [_ _] {:content "x [1]"}))
              (assoc-in [:config :keep-days] 2))
        old-file (io/file dir "2026-09-27.md")]
    (doseq [d ["2026-09-27" "2026-09-28" "2026-09-29"]]
      (store/save-day! (:store c) {:day d :sources [] :cited [] :markdown d :model "m" :provider "p"}))
    (.mkdirs (io/file dir))
    (spit old-file "old")
    (m/? (pipeline/run-task c "2026-09-30"))
    (is (= ["2026-09-30" "2026-09-29"] (store/days (:store c))))
    (is (not (.exists old-file)) "the markdown goes with the day")
    (is (some #(str/includes? (:text %) "Dropped 2 old days past the limit of 2")
              (:events @pipeline/status)))))

(deftest no-limit-keeps-every-day
  (let [c (-> (ctx [{:type ::fixture :name "A" :ns [1]}] (fn [_ _] {:content "x [1]"}))
              (assoc-in [:config :keep-days] -1))]
    (doseq [d ["2026-09-27" "2026-09-28" "2026-09-29"]]
      (store/save-day! (:store c) {:day d :sources [] :cited [] :markdown d :model "m" :provider "p"}))
    (m/? (pipeline/run-task c "2026-09-30"))
    (is (= 4 (count (store/days (:store c)))))))

(deftest a-cancel-during-the-model-call-reads-as-cancelled
  (let [c (ctx [{:type ::fixture :name "A" :ns [1]}]
               (fn [_ _] (Thread/sleep 5000) {:content "x [1]"}))]
    (is (pipeline/start-run! c "2026-09-30"))
    (loop [i 0] (when (and (not= :analysing (:state @pipeline/status)) (< i 100)) (Thread/sleep 20) (recur (inc i))))
    (is (pipeline/cancel-run!))
    (loop [i 0] (when (and (pipeline/running?) (< i 100)) (Thread/sleep 20) (recur (inc i))))
    (is (not (pipeline/running?)))
    (is (= :cancelled (:state @pipeline/status)))
    (is (nil? (store/day (:store c) "2026-09-30")))))
