(ns newsroom.llm.adapter
  "The provider adapter protocol, ported from samizdat.llm.adapter.

  Everything that is the same for every provider (retries, timeouts, JSON
  framing) lives in newsroom.llm.client. An adapter carries only the deltas:
  where the endpoint is, how it authenticates, what the request body looks
  like, and where the content and the reasoning live in the reply."
  (:refer-clojure :exclude [name]))

(defprotocol Adapter
  (id [this] "Keyword identifying the provider, e.g. :deepseek.")
  (display-name [this] "Human-readable name, used in error messages.")
  (chat-url [this config] "Full URL for a chat completion.")
  (models-url [this config] "Full URL for listing models, or nil.")
  (auth-headers [this config] "Provider-specific auth headers as a string->string map.")
  (chat-body [this config request]
    "The request body map for a normalized request:
     {:messages [{:role :content}] :max-tokens n :temperature d :reasoning-effort s}.")
  (parse-chat [this body]
    "{:content :reasoning :finish-reason :model :usage} out of a decoded
     successful reply, or nil for a reply that carries no completion.")
  (parse-models [this body] "Model ids from a decoded models reply.")
  (error-message [this body] "A provider-specific error string from a decoded error reply, or nil.")
  (usage-cap? [this status body]
    "Whether a 429 is a hard usage cap rather than a rate limit. A cap must
     not be retried."))
