(ns newsroom.config-test
  "The shipped default config and the sample in examples/ stay usable: they
  read, every source has an adapter, and every provider resolves."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [newsroom.config :as config]
            [newsroom.llm.providers :as providers]
            [newsroom.sources :as sources]))

(defn- adapter? [type]
  (contains? (set (keys (methods sources/fetch-items))) type))

(defn- check [cfg]
  (testing "every source has an adapter and a name"
    (doseq [s (:sources cfg)]
      (is (adapter? (:type s)) (str "no adapter for " (:type s)))
      (is (string? (sources/source-name s)))
      (when (= :scrape (:type s))
        (is (re-pattern (:link-pattern s)) "the link pattern compiles"))))
  (testing "every declared provider, and the analyst, resolves"
    (doseq [alias (keys (:providers cfg))]
      (is (:base-url (providers/resolve-provider cfg alias))))
    (is (:model (providers/role-llm cfg :analyst)))))

(deftest the-defaults-are-usable
  (check (config/defaults)))

(deftest the-sample-config-is-usable
  (check (edn/read-string (slurp (io/file "examples/config.edn")))))
