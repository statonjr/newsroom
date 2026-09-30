(ns newsroom.sources-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [newsroom.sources :as sources]))

(defn- search-stub
  "exa-search that answers each query from `answers`: a seq of results, or
  an exception to throw, taken in turn."
  [answers calls]
  (fn [query _ {:keys [source]}]
    (swap! calls conj query)
    (let [a (get @answers query)]
      (swap! answers update query rest)
      (let [r (first a)]
        (if (instance? Throwable r)
          (throw r)
          (mapv #(hash-map :title % :url (str "https://e.com/" %) :source source) r))))))

(def ^:private rate-limited (ex-info "web search answered 429" {:status 429}))
(def ^:private down (ex-info "web search answered 500" {:status 500}))

(defn- run [source answers]
  (let [calls (atom []) events (atom [])]
    (with-redefs [sources/exa-search (search-stub (atom answers) calls)
                  sources/retry-wait-ms 1]
      (let [r (try (sources/fetch-items source {:day "2026-09-30" :config {}
                                                :emit #(swap! events conj (:text %))})
                   (catch Exception e e))]
        {:result r :calls @calls :events @events}))))

(deftest a-rate-limited-query-is-tried-again
  (let [{:keys [result calls]} (run {:type :web-search :queries ["a"]}
                                    {"a" [rate-limited ["x"]]})]
    (is (= ["x"] (map :title result)))
    (is (= ["a" "a"] calls))))

(deftest a-failed-query-costs-only-its-own-results
  (let [{:keys [result events]} (run {:type :web-search :name "Web" :queries ["a" "b" "c"]}
                                     {"a" [["x"]] "b" [down] "c" [["y"]]})]
    (is (= ["x" "y"] (map :title result)))
    (is (some #(str/includes? % "“b” failed: web search answered 500") events))))

(deftest a-source-whose-every-query-fails-fails
  (let [{:keys [result]} (run {:type :web-search :queries ["a" "b"]}
                              {"a" [down] "b" [rate-limited rate-limited]})]
    (is (instance? Exception result))
    (is (str/includes? (ex-message result) "every search failed"))))

(def ^:private front
  "<a href=\"/article/one-story-here\">The first story of the day</a>
   <a href=\"/article/two-story-here\">The second story of the day</a>
   <a href=\"/article/three-story-here\">The third story of the day</a>
   <a href=\"/about\">About this newspaper</a>")

(defn- article [desc day]
  (str "<meta property=\"og:description\" content=\"" desc "\">"
       "<meta property=\"article:published_time\" content=\"" day "T08:00:00Z\">"))

(deftest a-scraped-page-gives-its-story-links
  (let [fetched (atom [])]
    (with-redefs [sources/fetch-text (fn [url _] (swap! fetched conj url) front)]
      (let [items (sources/fetch-items {:type :scrape :name "Paper" :url "https://paper.test/world"
                                        :link-pattern "/article/" :limit 2}
                                       {:day "2026-09-30" :config {}})]
        (is (= ["https://paper.test/article/one-story-here" "https://paper.test/article/two-story-here"]
               (map :url items)))
        (is (every? #(= "Paper" (:source %)) items))
        (is (= ["https://paper.test/world"] @fetched) "only the page, without :summaries")))))

(defn- throw-later [] (fn [] (throw (ex-info "HTTP 403" {}))))

(deftest summaries-come-from-each-story-page
  (let [pages {"https://paper.test/world" front
               "https://paper.test/article/one-story-here" (article "One happened." "2026-09-30")
               "https://paper.test/article/two-story-here" (throw-later)}
        events (atom [])]
    (with-redefs [sources/fetch-text (fn [url _] (let [p (pages url)] (if (fn? p) (p) p)))]
      (let [items (sources/fetch-items {:type :scrape :name "Paper" :url "https://paper.test/world"
                                        :link-pattern "/article/" :limit 2 :summaries true}
                                       {:day "2026-09-30" :config {}
                                        :emit #(swap! events conj (:text %))})]
        (is (= ["One happened." ""] (map :summary items)))
        (is (= ["2026-09-30T08:00:00Z" nil] (map :published items)))
        (is (some #(str/includes? % "couldn't read") @events)
            "a story page that fails keeps its link and says so")))))
