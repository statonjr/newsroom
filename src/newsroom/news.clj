(ns newsroom.news
  "The pure core of a day's briefing: no IO, no clock, no storage.

  Items gathered from every source are deduplicated by their canonical URL
  and numbered as sources; the numbered sources are written into the
  analysis prompt; the model's markdown answer cites them as [n], and
  `briefing` links those citations and appends the sources the answer cited.
  The contract is test/newsroom/news_spec.clj."
  (:require [clojure.string :as str]))

;; --- urls ------------------------------------------------------------------------

(def tracking-params
  "Query parameters that say how a reader arrived, not what they read."
  #{"fbclid" "gclid" "dclid" "mc_cid" "mc_eid" "igshid" "cmpid"
    "at_medium" "at_campaign" "traffic_source" "maca" "ocid" "smid"})

(defn- tracking? [param]
  (let [k (first (str/split param #"=" 2))]
    (or (str/starts-with? k "utm_") (contains? tracking-params k))))

(defn canonical-url
  "The address a story is known by: trimmed, with no fragment, no tracking
  parameters and no trailing slash."
  [url]
  (let [u (first (str/split (str/trim url) #"#" 2))
        q (str/index-of u "?")
        base (str/replace (if q (subs u 0 q) u) #"/+$" "")
        params (remove tracking? (remove str/blank? (str/split (if q (subs u (inc q)) "") #"&")))]
    (if (seq params)
      (str base "?" (str/join "&" params))
      base)))

;; --- items -----------------------------------------------------------------------

(defn- usable-item? [item]
  (not (or (str/blank? (:title item)) (str/blank? (:url item)))))

(defn dedupe-items
  "The first copy of each story, in the order they came; an item with no
  title or no URL is dropped."
  [items]
  (first (reduce (fn [[kept seen] item]
                   (let [k (canonical-url (:url item))]
                     (if (or (not (usable-item? item)) (contains? seen k))
                       [kept seen]
                       [(conj kept item) (conj seen k)])))
                 [[] #{}]
                 items)))

(defn cite
  "The items as sources, numbered from 1 in order: the numbers the analysis
  cites them by."
  [items]
  (vec (map-indexed (fn [i item] (assoc item :n (inc i))) items)))

;; --- the prompt ------------------------------------------------------------------

(defn- source-line [s]
  (str "[" (:n s) "] " (:title s)
       (when-not (str/blank? (:source s)) (str " (" (:source s) ")"))
       (when-let [p (:published s)] (str ", " p))
       "\n" (:url s)
       (when-not (str/blank? (:summary s)) (str "\n" (:summary s)))))

(defn- source-block [sources]
  (str/join "\n\n" (map source-line sources)))

(defn render-prompt
  "The template with {{date}} and {{sources}} filled in. A template with no
  {{sources}} gets them after it, so the model always sees what it may cite."
  [template day sources]
  (let [block (source-block sources)
        filled (str/replace template "{{date}}" day)]
    (if (str/includes? filled "{{sources}}")
      (str/replace filled "{{sources}}" block)
      (str filled "\n\n" block))))

;; --- citations -------------------------------------------------------------------

(def ^:private citation-re
  "A fenced code block, which is left alone, or a citation: [n] or [n, m]."
  #"(?s)(```.*?(?:```|$))|\[(\d+(?:\s*,\s*\d+)*)\]")

(defn- numbers-in [group]
  (keep #(parse-long (str/trim %)) (str/split group #",")))

(defn citations
  "The source numbers the markdown cites, as [n] or [n, m], ascending.
  Fenced code blocks, a diagram's source among them, cite nothing."
  [markdown]
  (vec (sort (distinct (mapcat (fn [[_ fence group]] (when-not fence (numbers-in group)))
                               (re-seq citation-re markdown))))))

(defn link-citations
  "The markdown with every citation of a known source turned into a link to
  it: [2] becomes [[2]](url). A number no source has, and anything inside a
  fenced code block, is left as it was."
  [markdown sources]
  (let [urls (into {} (map (fn [s] [(:n s) (:url s)]) sources))]
    (str/replace markdown citation-re
                 (fn [[whole fence group]]
                   (let [ns (when-not fence (numbers-in group))]
                     (if (some #(contains? urls %) ns)
                       (str/join ", " (map (fn [n] (if (contains? urls n)
                                                     (str "[[" n "]](" (get urls n) ")")
                                                     (str "[" n "]")))
                                           ns))
                       whole))))))

(defn- source-entry [s]
  (str "- [" (:n s) "] [" (str/replace (:title s) #"[\[\]]" "") "](" (:url s) ")"
       (when-not (str/blank? (:source s)) (str " — " (:source s)))))

(defn briefing
  "The day's briefing: the answer with its citations linked, then a Sources
  section listing the sources it cited."
  [markdown sources]
  (let [cited (set (citations markdown))
        used (filter #(contains? cited (:n %)) sources)]
    (str (str/trimr (link-citations markdown sources))
         "\n\n## Sources\n\n"
         (if (seq used)
           (str/join "\n" (map source-entry used))
           "_No sources were cited._")
         "\n")))

;; --- days ------------------------------------------------------------------------

(defn- leap? [y]
  (and (zero? (mod y 4)) (or (pos? (mod y 100)) (zero? (mod y 400)))))

(defn- days-in-month [y m]
  (case m
    2 (if (leap? y) 29 28)
    (4 6 9 11) 30
    31))

(defn valid-day?
  "Whether s is a calendar day written YYYY-MM-DD."
  [s]
  (let [[_ y m d] (re-matches #"(\d{4})-(\d{2})-(\d{2})" s)]
    (boolean
     (and y
          (<= 1 (parse-long m) 12)
          (<= 1 (parse-long d) (days-in-month (parse-long y) (parse-long m)))))))

(defn adjacent-days
  "The nearest day before `day` and after it among `days`, or nil where
  there is none."
  [days day]
  [(last (sort (filter #(neg? (compare % day)) days)))
   (first (sort (filter #(pos? (compare % day)) days)))])
