(ns newsroom.ui
  "The pages. A day is a wiki-style article: its briefing, the sources it
  cites, and everything gathered for it. The sidebar walks the history and
  jumps to a date, and shows the run in progress.

  `fragment` is what the SSE stream re-renders: it reads the pipeline's
  ratoms, so a page updates as a run progresses and when a day is stored."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [jolt.datastar.core :as ds]
            [newsroom.markdown :as md]
            [newsroom.news :as news]
            [newsroom.pipeline :as pipeline]
            [newsroom.sources :as sources]
            [newsroom.store :as store]))

;; --- sidebar -----------------------------------------------------------------------

(defn- month-of [day] (subs day 0 7))

(defn- month-label [ym]
  (let [[_ date] (str/split (sources/long-date (str ym "-01")) #" " 2)]
    date))

(defn- history [days current]
  [:nav.history
   [:h2 "Archive"]
   (if (empty? days)
     [:p.muted "No briefings yet."]
     (for [ds (partition-by month-of days)
           :let [ym (month-of (first ds))]]
       [:section
        [:h3 (month-label ym)]
        [:ul
         (for [d ds]
           [:li [:a {:href (str "/day/" d) :class (when (= d current) "current")}
                 (sources/long-date d)]])]]))])

(defn- source-state [{:keys [state count error]}]
  (case state
    :ok [:span.ok (str count " items")]
    :failed [:span.bad {:title error} "failed"]
    [:span.muted "…"]))

(defn- clock [ms]
  (let [t (.toLocalTime (.atZone (java.time.Instant/ofEpochMilli ms) (java.time.ZoneId/systemDefault)))]
    (format "%02d:%02d:%02d" (.getHour t) (.getMinute t) (.getSecond t))))

(defn- wire
  "The run's events, newest first."
  [events]
  (when (seq events)
    [:ol.wire
     (for [{:keys [at text url level]} (take 14 (reverse events))]
       [:li {:class (some-> level name)}
        [:time (clock at)]
        (if url [:a {:href url :target "_blank" :rel "noopener"} text] [:span text])])]))

(defn- writing [{:keys [words reasoning-words section tail]}]
  [:div.writing
   (if (pos? (or words 0))
     [:p [:strong words " words"] " written"
      (when section [:span " · " [:em section]])]
     [:p [:strong "Thinking"] (when (pos? (or reasoning-words 0))
                                (str " · " reasoning-words " words of reasoning"))])
   (when-not (str/blank? tail) [:blockquote tail])])

(defn- run-panel [{:keys [state day error items cited provider model sources events] :as st} today]
  (let [busy? (contains? #{:starting :gathering :analysing} state)]
    [:section.run
     [:h2 "Desk"]
     (case state
       :starting [:p "Starting the run for " (sources/long-date day) "…"]
       :gathering [:p "Gathering the news for " (sources/long-date day) "…"]
       :analysing [:p "Analysing " items " items with " provider
                   (when model [:span.muted " (" model ")"]) "…"]
       :done [:p "Filed " [:a {:href (str "/day/" day)} (sources/long-date day)]
              ": " items " sources, " cited " cited."]
       :failed [:p.bad "The run for " (sources/long-date day) " failed: " error]
       :cancelled [:p.muted "The run for " (sources/long-date day) " was cancelled."]
       [:p.muted "Idle."])
     (when (and (= :analysing state) (:writing st))
       (writing (:writing st)))
     (when (and busy? (seq sources))
       [:ul.sources-progress
        (for [[name s] (sort-by key sources)]
          [:li [:span name] (source-state s)])])
     (wire events)
     (if busy?
       [:button {"data-on:click" "@post('/cancel')"} "Cancel run"]
       [:button {"data-on:click" (str "@post('/run?day=" today "')")}
        "Gather today’s news"])]))

(defn- sidebar
  "What goes inside the sidebar."
  [days current]
  (list
   [:header.masthead
    [:a {:href "/"} [:h1 "The Newsroom"]]
    [:p.tagline "Daily briefing & analysis"]]
   [:label.jump
    [:span "Go to date"]
    [:input {:type "date" :value current
             "data-on:change" "evt.target.value && (window.location = '/day/' + evt.target.value)"}]]
   (let [[prev next] (news/adjacent-days days current)]
     [:div.stepper
      (if prev [:a {:href (str "/day/" prev)} "← Earlier"] [:span.muted "← Earlier"])
      (if next [:a {:href (str "/day/" next)} "Later →"] [:span.muted "Later →"])])
   (run-panel @pipeline/status (pipeline/today))
   (history days current)))

;; --- the day -----------------------------------------------------------------------

(defn- gathered [sources]
  [:details.gathered
   [:summary "Everything gathered for the day (" (count sources) ")"]
   [:ol
    (for [{:keys [n title url source summary]} sources]
      [:li {:id (str "source-" n) :value n}
       [:a {:href url :rel "noopener" :target "_blank"} title]
       (when-not (str/blank? source) [:span.muted " — " source])
       (when-not (str/blank? summary) [:p.summary summary])])]])

(defn- article [st day]
  (if-let [{:keys [markdown sources model provider created-at]} (store/day st day)]
    [:article.briefing
     [:p.dateline (sources/long-date day)]
     [:div.prose (h/raw (md/html markdown))]
     [:footer.meta
      "Written by " provider (when model (str " / " model))
      (when created-at (str " at " (subs created-at 0 (min 16 (count created-at)))))
      " · " [:a {:href (str "/day/" day ".md")} "markdown"]]
     (gathered sources)]
    [:article.briefing.empty
     [:p.dateline (sources/long-date day)]
     [:h1 "No briefing for this day"]
     (if (= day (pipeline/today))
       [:p "Today’s news hasn’t been gathered yet. Use the button in the sidebar to gather and analyse it now."]
       [:p "Nothing was gathered on this day."])]))

(defn fragment
  "The content of one live part of a day's page, by the selector its stream
  patches: \"#sidebar\" or \"#article\".

  The two stream apart because each re-renders only when a ratom it read
  changes. The sidebar reads the run's status, which changes several times a
  second during a run; the article reads only `stored`, so it, and the
  diagram in it, stays still until a day is written."
  [st day selector]
  ;; stored changes when a day is written, so both re-read the store
  @pipeline/stored
  (if (= "#article" selector)
    (article st day)
    (sidebar (store/days st) day)))

(def styles "
:root {
  --paper: #f5f7f9; --ink: #121820; --muted: #58636f; --rule: #d6dce3;
  --accent: #0a6c74; --accent-ink: #ffffff; --panel: #e8ecf0; --ok: #2e7a4f; --bad: #b42318;
  --serif: Charter, 'Bitstream Charter', 'Iowan Old Style', Georgia, serif;
  --sans: 'Helvetica Neue', 'Inter', -apple-system, 'Segoe UI', system-ui, sans-serif;
  color-scheme: light;
}
@media (prefers-color-scheme: dark) {
  :root { --paper: #0d1117; --ink: #e4e8ee; --muted: #8b95a3; --rule: #252d38;
          --accent: #37c2c0; --accent-ink: #06201f; --panel: #141a22; --ok: #6fcf97; --bad: #ff7b72;
          color-scheme: dark; }
}
* { box-sizing: border-box; }
html, body { margin: 0; background: var(--paper); color: var(--ink); }
body { font: 17px/1.65 var(--serif); }
a { color: var(--accent); text-decoration-thickness: 1px; text-underline-offset: 2px; }
.muted { color: var(--muted); }
.ok { color: var(--ok); } .bad { color: var(--bad); }
.layout { display: grid; grid-template-columns: 290px minmax(0, 1fr); min-height: 100vh; }
.sidebar { border-right: 1px solid var(--rule); padding: 28px 22px; background: var(--panel);
           font: 14px/1.5 var(--sans);
           position: sticky; top: 0; height: 100vh; overflow-y: auto; }
.masthead a { color: inherit; text-decoration: none; }
.masthead { border-top: 5px solid var(--ink); padding-top: 10px; }
.masthead h1 { font: 800 25px/1 var(--sans); margin: 0; letter-spacing: -0.02em; text-transform: uppercase; }
.tagline { margin: 6px 0 22px; padding-bottom: 10px; border-bottom: 1px solid var(--ink); color: var(--accent);
           font: 600 11px var(--sans); text-transform: uppercase; letter-spacing: .16em; }
.jump { display: block; margin-bottom: 10px; }
.jump span { display: block; font-size: 12px; color: var(--muted); margin-bottom: 4px; }
.jump input { width: 100%; font: inherit; padding: 6px 8px; border: 1px solid var(--rule);
              background: var(--paper); color: var(--ink); border-radius: 4px; }
.stepper { display: flex; justify-content: space-between; margin-bottom: 22px; }
.run, .history { border-top: 1px solid var(--rule); padding-top: 14px; margin-top: 14px; }
.sidebar h2 { font-size: 11px; text-transform: uppercase; letter-spacing: .14em; color: var(--muted); margin: 0 0 8px; }
.sidebar h3 { font-size: 13px; margin: 12px 0 4px; }
.sidebar ul { list-style: none; margin: 0; padding: 0; }
.history li a { display: block; padding: 2px 8px; margin: 0 -8px; border-radius: 3px; text-decoration: none; }
.history li a:hover { background: var(--rule); }
.history li a.current { background: var(--accent); color: var(--accent-ink); }
.sources-progress li { display: flex; justify-content: space-between; gap: 8px; font-size: 13px; }
.sources-progress li span:first-child { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.writing { margin: 8px 0; padding: 8px 10px; background: var(--paper); border: 1px solid var(--rule); border-radius: 4px; }
.writing p { margin: 0; }
.writing blockquote { margin: 6px 0 0; font: italic 13px/1.45 var(--serif); color: var(--muted);
                      max-height: 7.5em; overflow: hidden; }
.wire { list-style: none; margin: 10px 0 0; padding: 0; font-size: 12px; max-height: 320px; overflow-y: auto; }
.wire li { display: grid; grid-template-columns: 58px 1fr; gap: 6px; padding: 3px 0; border-bottom: 1px dotted var(--rule); }
.wire li:first-child { font-weight: 600; }
.wire time { color: var(--muted); font-variant-numeric: tabular-nums; }
.wire a, .wire span { overflow-wrap: anywhere; }
.wire li.error span { color: var(--bad); }
.wire li.ok span { color: var(--ok); }
button { margin-top: 10px; width: 100%; font: 600 13px var(--sans); padding: 8px 10px;
         border: 1px solid var(--accent); background: var(--accent); color: var(--accent-ink);
         border-radius: 4px; cursor: pointer; }
button:hover { filter: brightness(1.1); }
main { padding: 48px clamp(20px, 6vw, 80px) 80px; }
.briefing { max-width: 44rem; margin: 0 auto; }
.dateline { font: 700 12px var(--sans); text-transform: uppercase; letter-spacing: .16em;
            color: var(--accent); margin: 0 0 8px; }
.prose h1 { font: 800 2.35rem/1.1 var(--sans); margin: 0 0 1.3rem; letter-spacing: -0.025em; }
.prose h2 { font: 700 .82rem/1.3 var(--sans); text-transform: uppercase; letter-spacing: .14em; color: var(--accent);
            margin: 2.6rem 0 .8rem; padding-top: .6rem; border-top: 2px solid var(--ink); }
.prose h3 { font: 700 1.2rem/1.3 var(--sans); margin: 1.6rem 0 .4rem; letter-spacing: -0.01em; }
.prose p, .prose li { hyphens: auto; }
.prose a { font-size: .8em; vertical-align: super; line-height: 0; text-decoration: none; }
.prose h2 + ul a, .prose ul li > a:only-child { font-size: 1em; vertical-align: baseline; }
.prose blockquote { margin: 1rem 0; padding: 0 1rem; border-left: 3px solid var(--rule); color: var(--muted); }
.prose code { font-size: .88em; background: var(--panel); padding: 1px 4px; border-radius: 3px; }
.prose pre { background: var(--panel); padding: 12px; overflow-x: auto; border-radius: 4px; }
.diagram { margin: 1.8rem 0; border: 1px solid var(--rule); border-radius: 6px; background: var(--panel); overflow: hidden; }
.diagram-tools { display: flex; gap: 6px; align-items: center; padding: 6px 8px; border-bottom: 1px solid var(--rule); }
.diagram-tools button { width: auto; min-width: 32px; margin: 0; padding: 3px 9px; background: var(--paper);
                        color: var(--ink); border: 1px solid var(--rule); font: 600 13px var(--sans); }
.diagram-tools button:hover { border-color: var(--accent); filter: none; }
.diagram-hint { margin-left: auto; font: 12px var(--sans); color: var(--muted); }
.diagram-view { overflow: hidden; max-height: 70vh; cursor: grab; touch-action: none; user-select: none; }
.diagram-view.dragging { cursor: grabbing; }
.prose .diagram pre.mermaid { background: transparent; margin: 0; padding: 16px; border-radius: 0; text-align: center;
                              overflow: visible;
                              font: 12px/1.4 ui-monospace, monospace; white-space: pre-wrap; color: var(--muted); }
.diagram-view svg { transform-origin: 0 0; height: auto; }
.diagram.expanded { position: fixed; inset: 16px; z-index: 50; margin: 0; display: flex; flex-direction: column;
                    background: var(--paper); box-shadow: 0 12px 48px rgba(0,0,0,.35); }
.diagram.expanded .diagram-view { flex: 1; max-height: none; min-height: 0; position: relative; }
.diagram.expanded .diagram-view pre.mermaid { position: absolute; inset: 0; padding: 0; text-align: left; }
@media (max-width: 800px) { .diagram-hint { display: none; } }
.prose hr { border: 0; border-top: 1px solid var(--rule); margin: 2rem 0; }
.meta { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid var(--rule); color: var(--muted);
        font: 13px var(--sans); }
.gathered { margin-top: 1.5rem; font: 14px/1.5 var(--sans); }
.gathered summary { cursor: pointer; color: var(--muted); }
.gathered li { margin: .6rem 0; }
.gathered .summary { margin: .2rem 0 0; color: var(--muted); font-size: 13px; }
.empty h1 { font: 800 2rem/1.15 var(--sans); letter-spacing: -0.02em; }
@media (max-width: 800px) {
  .layout { grid-template-columns: 1fr; }
  .sidebar { position: static; height: auto; border-right: 0; border-bottom: 1px solid var(--rule); }
  main { padding: 28px 16px 60px; }
}
")

(def mermaid-script
  "Draws every diagram in a briefing and drives its controls. A live patch
  morphs a drawn diagram back to its source text and drops the classes the
  script set, so the observer draws it again and reapplies the view, which
  is kept per figure: the zoom, the pan, and whether it is full screen."
  "import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@12.0.0/dist/mermaid.esm.min.mjs';
const dark = matchMedia('(prefers-color-scheme: dark)').matches;
// the page's palette, so a diagram reads as part of the article
const palette = dark
  ? {bg: '#0d1117', node: '#141a22', border: '#37c2c0', text: '#e4e8ee', line: '#8b95a3',
     cluster: '#10161d', clusterBorder: '#252d38', label: '#0d1117'}
  : {bg: '#f5f7f9', node: '#ffffff', border: '#0a6c74', text: '#121820', line: '#58636f',
     cluster: '#e8ecf0', clusterBorder: '#d6dce3', label: '#f5f7f9'};
mermaid.initialize({startOnLoad: false, securityLevel: 'strict', theme: 'base',
  fontFamily: 'Helvetica Neue, Inter, -apple-system, system-ui, sans-serif',
  themeVariables: {
    background: palette.bg, primaryColor: palette.node, primaryBorderColor: palette.border,
    primaryTextColor: palette.text, lineColor: palette.line, textColor: palette.text,
    clusterBkg: palette.cluster, clusterBorder: palette.clusterBorder,
    edgeLabelBackground: palette.label, titleColor: palette.text, fontSize: '14px'}});

const views = new WeakMap();
const view = f => {
  if (!views.has(f)) views.set(f, {s: 1, x: 0, y: 0, expanded: false, fitted: true});
  return views.get(f);
};
// write a style only when it differs: a write the observer sees would call
// draw again, and an unconditional one loops forever
const put = (el, k, val) => { if (el.style[k] !== val) el.style[k] = val; };
// A fitted diagram fills its view: inline at the page's width, as mermaid
// sizes it, and full screen scaled from its own dimensions to fill the whole
// view, centred. Zooming or panning leaves the fitted state.
const fit = (f, v, svg) => {
  if (!v.fitted) return;
  if (!v.expanded) { Object.assign(v, {s: 1, x: 0, y: 0}); return; }
  const vb = svg.viewBox && svg.viewBox.baseVal;
  const box = f.querySelector('.diagram-view').getBoundingClientRect();
  if (!vb || !vb.width || !box.width) return;
  const pad = 24;
  const s = Math.min((box.width - 2 * pad) / vb.width, (box.height - 2 * pad) / vb.height);
  Object.assign(v, {s, x: (box.width - vb.width * s) / 2, y: (box.height - vb.height * s) / 2});
};
const sync = f => {
  const v = view(f);
  if (f.classList.contains('expanded') !== v.expanded) f.classList.toggle('expanded', v.expanded);
  const b = f.querySelector('[data-diagram=expand]');
  const label = v.expanded ? '✕' : '⤢';
  if (b && b.textContent !== label) b.textContent = label;
  const svg = f.querySelector('.diagram-view svg');
  if (!svg) return;
  if (svg.dataset.maxWidth === undefined) svg.dataset.maxWidth = svg.style.maxWidth;
  const vb = svg.viewBox && svg.viewBox.baseVal;
  if (v.expanded && vb && vb.width) {
    // its own size, so the transform alone decides how big it is
    put(svg, 'maxWidth', 'none');
    put(svg, 'width', vb.width + 'px');
    put(svg, 'height', vb.height + 'px');
  } else {
    put(svg, 'maxWidth', svg.dataset.maxWidth);
    put(svg, 'width', '');
    put(svg, 'height', '');
  }
  fit(f, v, svg);
  put(svg, 'transform', `translate(${v.x}px, ${v.y}px) scale(${v.s})`);
};
const zoomAt = (f, factor, cx, cy) => {
  const v = view(f);
  const s = Math.min(10, Math.max(0.1, v.s * factor));
  const k = s / v.s;
  v.x = cx - k * (cx - v.x);
  v.y = cy - k * (cy - v.y);
  v.s = s;
  v.fitted = false;
  sync(f);
};
const middle = f => {
  const r = f.querySelector('.diagram-view').getBoundingClientRect();
  return [r.width / 2, r.height / 2];
};

// one draw at a time; a change that lands while one runs asks for another
let drawing = false, again = false;
const draw = async () => {
  if (drawing) { again = true; return; }
  drawing = true;
  try {
    do {
      again = false;
      const nodes = [...document.querySelectorAll('pre.mermaid:not([data-processed])')];
      if (nodes.length) await mermaid.run({nodes, suppressErrors: true});
      document.querySelectorAll('figure.diagram').forEach(sync);
    } while (again);
  } finally { drawing = false; }
};
// what mermaid and sync change themselves is not a reason to draw again
const ours = n => n.nodeType === 1 && n.closest && n.closest('svg, .diagram-tools');
new MutationObserver(ms => {
  if (ms.some(m => !ours(m.target.nodeType === 1 ? m.target : m.target.parentElement))) draw();
}).observe(document.getElementById('article'), {childList: true, subtree: true, characterData: true});
draw();

document.addEventListener('click', e => {
  const b = e.target.closest('[data-diagram]');
  if (!b) return;
  const f = b.closest('figure.diagram');
  const v = view(f);
  switch (b.dataset.diagram) {
    case 'in': zoomAt(f, 1.25, ...middle(f)); break;
    case 'out': zoomAt(f, 0.8, ...middle(f)); break;
    case 'reset': v.fitted = true; sync(f); break;
    case 'expand': v.expanded = !v.expanded; v.fitted = true; sync(f); break;
  }
});
document.addEventListener('keydown', e => {
  if (e.key !== 'Escape') return;
  document.querySelectorAll('figure.diagram.expanded').forEach(f => {
    Object.assign(view(f), {expanded: false, fitted: true});
    sync(f);
  });
});
window.addEventListener('resize', () => document.querySelectorAll('figure.diagram').forEach(sync));
document.addEventListener('dblclick', e => {
  const el = e.target.closest('.diagram-view');
  if (!el) return;
  const f = el.closest('figure.diagram');
  view(f).fitted = true;
  sync(f);
});
// zoom around the cursor: ctrl or cmd with the wheel (a trackpad pinch sends
// that too), or the plain wheel when full screen, where the page cannot scroll
document.addEventListener('wheel', e => {
  const el = e.target.closest && e.target.closest('.diagram-view');
  if (!el) return;
  const f = el.closest('figure.diagram');
  if (!(e.ctrlKey || e.metaKey || view(f).expanded)) return;
  e.preventDefault();
  const r = el.getBoundingClientRect();
  zoomAt(f, Math.exp(-e.deltaY * 0.002), e.clientX - r.left, e.clientY - r.top);
}, {passive: false});

let drag = null;
document.addEventListener('pointerdown', e => {
  const el = e.target.closest('.diagram-view');
  if (!el || e.button !== 0) return;
  const f = el.closest('figure.diagram');
  drag = {f, el, x: e.clientX, y: e.clientY};
  el.setPointerCapture(e.pointerId);
  el.classList.add('dragging');
});
document.addEventListener('pointermove', e => {
  if (!drag) return;
  const v = view(drag.f);
  v.x += e.clientX - drag.x;
  v.y += e.clientY - drag.y;
  v.fitted = false;
  drag.x = e.clientX;
  drag.y = e.clientY;
  sync(drag.f);
});
const stop = () => { if (drag) { drag.el.classList.remove('dragging'); drag = null; } };
document.addEventListener('pointerup', stop);
document.addEventListener('pointercancel', stop);")

(defn page
  "The whole document for `day`."
  [st day]
  (str "<!DOCTYPE html>"
       (h/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
          [:title (str (sources/long-date day) " · The Newsroom")]
          [:script {:type "module" :src "/js/datastar.js"}]
          [:style (h/raw styles)]]
         [:body
          [:div.layout
           [:aside#sidebar.sidebar (ds/init-opts {:selector "#sidebar"})
            (fragment st day "#sidebar")]
           [:main#article (dissoc (ds/init-opts {:selector "#article"}) :data-signals)
            (fragment st day "#article")]]
          [:script {:type "module"} (h/raw mermaid-script)]]])))
