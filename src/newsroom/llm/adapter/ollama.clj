(ns newsroom.llm.adapter.ollama
  "Ollama's native /api/chat, ported from samizdat.llm.adapter.ollama.

  The native API is where Ollama surfaces `think`, `num_predict` under
  `options`, and a `done_reason` that tells the token cap from a natural
  stop. `stream` has to be false or the body arrives as newline-delimited
  JSON."
  (:require [newsroom.llm.adapter :as adapter]))

(defrecord OllamaAdapter []
  adapter/Adapter
  (id [_] :ollama)
  (display-name [_] "Ollama")
  (chat-url [_ config] (str (:base-url config) "/api/chat"))
  (models-url [_ config] (str (:base-url config) "/api/tags"))

  (auth-headers [_ config]
    (if-let [k (:api-key config)] {"Authorization" (str "Bearer " k)} {}))

  (chat-body [_ config {:keys [messages max-tokens temperature]}]
    (cond-> {:model (:model config) :messages messages :stream false}
      (or max-tokens temperature)
      (assoc :options (cond-> {}
                        max-tokens (assoc :num_predict max-tokens)
                        temperature (assoc :temperature temperature)))))

  (parse-chat [_ body]
    (when-let [msg (:message body)]
      {:content (:content msg)
       :reasoning (:thinking msg)
       :finish-reason (or (:done_reason body) "stop")
       :model (some-> (:model body) str not-empty)
       :usage (when (:eval_count body)
                {:prompt-tokens (or (:prompt_eval_count body) 0)
                 :completion-tokens (or (:eval_count body) 0)
                 :total-tokens (+ (or (:prompt_eval_count body) 0)
                                  (or (:eval_count body) 0))})}))

  (parse-models [_ body] (mapv :name (:models body)))
  (error-message [_ body] (when-let [e (:error body)] (str e)))
  (usage-cap? [_ _ _] false))

(def ollama (->OllamaAdapter))
