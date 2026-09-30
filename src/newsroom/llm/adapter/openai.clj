(ns newsroom.llm.adapter.openai
  "The OpenAI chat-completions family, ported from samizdat.llm.adapter.openai.

  One adapter covers OpenAI, DeepSeek, Zhipu GLM, and any local llama-server
  or vLLM endpoint, because they all speak the same wire format. What differs
  is carried as fields on the record: where the reasoning stream lives, and
  what the max-tokens field is called. The tool-call, prefill and grammar
  machinery samizdat needs for an agent loop is left out; a briefing is one
  plain completion."
  (:require [clojure.string :as str]
            [newsroom.llm.adapter :as adapter]))

(def ^:private usage-cap-re
  #"(?i)insufficient|quota|balance|billing|credit|exceeded your current|usage limit|arrearage")

(defn- supports? [config feature]
  (contains? (or (:features config) #{}) feature))

(defn- local-wire
  "llama.cpp's knobs, each behind the feature that says the endpoint has it:
  Qwen-family thinking is turned off through the chat template unless the
  config opts in with :thinking? true."
  [config]
  (cond-> {}
    (and (supports? config :thinking-toggle) (not (:thinking? config)))
    (assoc :chat_template_kwargs {:enable_thinking false})))

(defn- reasoning-wire
  "The reasoning fields for `effort`. \"none\" is spelled per provider:
  DeepSeek disables thinking outright, GLM cannot be turned off so low is the
  least, everyone else takes reasoning_effort none."
  [provider-id effort]
  (if (= "none" effort)
    (case provider-id
      :deepseek {:thinking {:type "disabled"}}
      :glm      {:reasoning_effort "low"}
      {:reasoning_effort "none"})
    {:reasoning_effort effort}))

(defrecord OpenAIAdapter [provider-id label reasoning-key max-tokens-key]
  adapter/Adapter
  (id [_] provider-id)
  (display-name [_] label)

  (chat-url [_ config] (str (:base-url config) "/chat/completions"))

  (models-url [_ config]
    (str (str/replace (str (:base-url config)) #"/beta/?$" "/v1") "/models"))

  (auth-headers [_ config]
    (if-let [k (:api-key config)]
      {"Authorization" (str "Bearer " k)}
      {}))

  (chat-body [_ config {:keys [messages max-tokens temperature reasoning-effort]}]
    (let [effort (or reasoning-effort (:reasoning-effort config))]
      (cond-> {:model (:model config) :messages messages}
        max-tokens (assoc max-tokens-key max-tokens)
        temperature (assoc :temperature temperature)
        (some? effort) (merge (reasoning-wire provider-id effort))
        :always (merge (local-wire config)))))

  (parse-chat [_ body]
    (when-let [choice (first (:choices body))]
      (let [msg (:message choice)]
        {:content (:content msg)
         :reasoning (get msg reasoning-key)
         :finish-reason (or (:finish_reason choice) "stop")
         ;; the model that answered, which may not be the one requested
         :model (some-> (:model body) str not-empty)
         :usage (when-let [u (:usage body)]
                  {:prompt-tokens (or (:prompt_tokens u) 0)
                   :completion-tokens (or (:completion_tokens u) 0)
                   :total-tokens (or (:total_tokens u) 0)})})))

  (parse-models [_ body] (mapv :id (:data body)))

  (error-message [_ body]
    (when-let [e (:error body)]
      (if (map? e)
        (str (or (:message e) "unknown error")
             (when-let [c (:code e)] (str " (code " c ")")))
        (str e))))

  (usage-cap? [_ _status body]
    (let [msg (str (get-in body [:error :message])
                   " " (get-in body [:error :type])
                   " " (get-in body [:error :code]))]
      (boolean (re-find usage-cap-re msg)))))

(defn openai-family
  "An adapter for an OpenAI-compatible endpoint. `reasoning-key` names the
  field carrying a separate reasoning stream; `max-tokens-key` exists because
  newer OpenAI models renamed max_tokens to max_completion_tokens."
  [{:keys [id label reasoning-key max-tokens-key]
    :or {max-tokens-key :max_tokens}}]
  (->OpenAIAdapter id (or label (str/capitalize (name id)))
                   (or reasoning-key :__no_reasoning_field__)
                   max-tokens-key))

(def deepseek (openai-family {:id :deepseek :label "DeepSeek" :reasoning-key :reasoning_content}))
(def glm (openai-family {:id :glm :label "GLM" :reasoning-key :reasoning_content}))
(def openai (openai-family {:id :openai :label "OpenAI"}))
;; a local endpoint serves a model, and llama-server hands back
;; reasoning_content for a reasoning model exactly as DeepSeek does
(def local (openai-family {:id :local :label "local" :reasoning-key :reasoning_content}))
