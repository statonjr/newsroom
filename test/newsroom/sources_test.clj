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
