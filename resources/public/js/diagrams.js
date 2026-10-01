// Draws every diagram in a briefing and drives its controls. A live patch
// morphs a drawn diagram back to its source text and drops the classes the
// script set, so the observer draws it again and reapplies the view, which
// is kept per figure: the zoom, the pan, and whether it is full screen.
import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@12.0.0/dist/mermaid.esm.min.mjs';
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
document.addEventListener('pointercancel', stop);
