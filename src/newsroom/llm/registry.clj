(ns newsroom.llm.registry
  "Provider type keyword to adapter. Adding a provider means writing an
  adapter and adding one line here."
  (:require [clojure.string :as str]
            [newsroom.llm.adapter.ollama :as ollama]
            [newsroom.llm.adapter.openai :as openai]))

(def adapters
  {:deepseek openai/deepseek
   :glm openai/glm
   :openai openai/openai
   :local openai/local
   :ollama ollama/ollama})

(defn adapter-for
  "The adapter for a provider type. Throws naming what is available rather
  than returning nil, since a nil adapter fails much later and less clearly."
  [provider]
  (or (get adapters provider)
      (throw (ex-info (str "No adapter for provider " provider
                           ". Known: " (str/join ", " (sort (map name (keys adapters)))))
                      {:provider provider :known (keys adapters)}))))
