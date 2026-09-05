/*
 * The matcher behind the search box in the bar — framework-free on purpose.
 *
 * Four documents carry that box: this site, the playground, the sample
 * catalogue and the per-sample pages. One of them is a Vue application and
 * three are static HTML with a module or two, so anything shared between them
 * has to be plain JavaScript with no imports. This file is that, and
 * `src/shell/search-engine.mjs` in abap2UI5/playground is its copy — kept in
 * step by hand, the same arrangement as the palette and site-memory. What is
 * NOT copied is the index it reads: that is one generated document at
 * /docs/search-index.json, fetched by whichever site the reader is on, because
 * two copies of the DATA would be two answers to the same query.
 *
 * The ranking, in one sentence: a word you typed is worth most in a title,
 * then in a class name, then in a heading, then in the summary or the
 * keywords — and every word has to appear somewhere, so a second word narrows
 * a search rather than widening it.
 *
 * There is no stemming and no fuzzy matching. The corpus is ~950 short
 * entries of technical vocabulary — control names, ABAP class names, chapter
 * titles — where a near-miss is usually a DIFFERENT control, and "close
 * enough" answers are worse than none. A prefix match is as far as it goes,
 * because that is what typing looks like before you have finished.
 */

/** Where the index lives — one document, on the origin all four sites share. */
export const INDEX_URL = 'https://abap2ui5.github.io/docs/search-index.json';

const normalise = (s) => (s || '').toLowerCase();
/* A query is split the way the index was built (scripts/lib/pages.mjs): the
 * dot and the underscore stay inside a word, everything else is a boundary. So
 * `client->view_display( )` pasted out of a page is two terms, both of which
 * are in the index, rather than one that is in nothing. */
const words = (s) => normalise(s).split(/[^\p{L}\p{N}._]+/u)
  .map((w) => w.replace(/^[._]+|[._]+$/g, ''))
  .filter(Boolean);

/* How much a word found in each field is worth. A title hit beats everything:
 * the entries are short and their titles are what a reader is trying to
 * remember. `code` is the ABAP class name, which is either exactly what
 * somebody pasted in or irrelevant - hence high, and only on a prefix. */
const FIELD = { title: 10, code: 8, heading: 5, text: 2, terms: 2, group: 1 };

function scoreField(value, term, weight) {
  if (!value) return 0;
  const hay = normalise(value);
  const at = hay.indexOf(term);
  if (at < 0) return 0;
  /* Where the word sits decides how much of the weight it earns: the whole
   * field, the start of it, the start of any word in it, or somewhere inside
   * a longer word - `list` in `ActionListItem` is a real hit and a weaker one
   * than `list` in `List Report`. */
  if (hay === term) return weight * 3;
  if (at === 0) return weight * 2;
  return /[^\p{L}\p{N}]/u.test(hay[at - 1] || '') ? weight : weight / 2;
}

/**
 * The entries that match `query`, best first.
 *
 * Every term must be found somewhere in an entry, or the entry is out: typing
 * a second word is how a reader narrows a result list, and a search that
 * treats the words as alternatives grows the list instead, which reads as the
 * box ignoring what you typed.
 */
export function search(entries, query, { limit = 30 } = {}) {
  const terms = words(query);
  if (!terms.length) return [];

  const hits = [];
  for (const e of entries) {
    let total = 0;
    let missed = false;
    /* Which heading matched, so the result can offer the SECTION rather than
     * the page - the difference between "Cookbook: Tables" and the paragraph
     * about sorting. */
    let heading = null;

    for (const term of terms) {
      let best = scoreField(e.title, term, FIELD.title)
        + scoreField(e.code, term, FIELD.code)
        + scoreField(e.text, term, FIELD.text)
        + scoreField(e.terms, term, FIELD.terms)
        + scoreField(e.group, term, FIELD.group);

      for (const [text, anchor] of e.headings || []) {
        const s = scoreField(text, term, FIELD.heading);
        if (!s) continue;
        best += s;
        if (!heading || s > heading.score) heading = { text, anchor, score: s };
      }

      if (!best) { missed = true; break; }
      total += best;
    }
    if (missed || !total) continue;

    /* A shorter entry that scored the same is the better answer: the words
     * are a larger part of what it is about. */
    hits.push({ entry: e, score: total - Math.min(4, (e.title || '').length / 40), heading });
  }

  hits.sort((a, b) => b.score - a.score || (a.entry.title || '').localeCompare(b.entry.title || ''));
  return hits.slice(0, limit);
}

/**
 * The hits, grouped the way the index declares its areas — Documentation
 * first, then each sample corpus — with at most `perGroup` in each, so one
 * corpus of 636 ports cannot bury the four pages that explain them.
 */
export function grouped(hits, { perGroup = 8 } = {}) {
  const order = [];
  const byGroup = new Map();
  /* How many a group HAS, beside how many it shows. A reader who typed
   * "table" and sees eight is looking at eight of two hundred and thirty-one,
   * and the difference between those two numbers is the difference between
   * "that is all there is" and "there is a whole shelf of this". */
  const total = new Map();
  for (const hit of hits) {
    const key = hit.entry.area === 'docs' ? 'Documentation' : hit.entry.group;
    if (!byGroup.has(key)) { byGroup.set(key, []); order.push(key); }
    total.set(key, (total.get(key) ?? 0) + 1);
    const rows = byGroup.get(key);
    if (rows.length < perGroup) rows.push(hit);
  }
  /* Documentation first whenever it is in the answer at all. The reader who
   * typed a word that is both a chapter and a control wants the explanation
   * before the seven hundred examples of it. */
  order.sort((a, b) => (a === 'Documentation' ? -1 : b === 'Documentation' ? 1 : 0));
  return order.map((label) => ({ label, hits: byGroup.get(label), total: total.get(label) }));
}

/** The index, fetched once. Callers await this on the first keystroke, never
 *  at load: a reader who does not search pays nothing for the box. */
let pending = null;
export function loadIndex(url = INDEX_URL, { fetchFn = globalThis.fetch } = {}) {
  pending ??= fetchFn(url)
    .then((r) => (r.ok ? r.json() : Promise.reject(new Error(`search index: HTTP ${r.status}`))))
    .catch((e) => { pending = null; throw e; });
  return pending;
}

/** `text` split into the parts that matched `query` and the parts that did
 *  not, as `[string, boolean][]` — for a result row that shows WHY it is in
 *  the list. Markup is the caller's business; three documents draw it three
 *  ways and none of them wants a string of HTML from here. */
export function highlight(text, query) {
  const terms = [...new Set(words(query))].sort((a, b) => b.length - a.length);
  const hay = text || '';
  if (!terms.length || !hay) return [[hay, false]];
  const marks = [];
  const low = hay.toLowerCase();
  for (const term of terms) {
    let from = 0;
    for (;;) {
      const at = low.indexOf(term, from);
      if (at < 0) break;
      marks.push([at, at + term.length]);
      from = at + term.length;
    }
  }
  if (!marks.length) return [[hay, false]];
  marks.sort((a, b) => a[0] - b[0]);
  const out = [];
  let at = 0;
  for (const [start, end] of marks) {
    if (end <= at) continue;
    const from = Math.max(start, at);
    if (from > at) out.push([hay.slice(at, from), false]);
    out.push([hay.slice(from, end), true]);
    at = end;
  }
  if (at < hay.length) out.push([hay.slice(at), false]);
  return out;
}

/* ── THE LAST THING YOU SEARCHED FOR ────────────────────────────────────────
 *
 * A search that finds something ends on another page — often on another
 * deployment — and the box that opens there is a new one, empty. A reader
 * comparing three samples of the same control typed the same word three times,
 * which is the search asking them to remember what they had just told it.
 *
 * So the query is written down when a hit is opened, and the next box on this
 * origin opens with it, SELECTED, so the first keystroke replaces it rather
 * than appending to it: the field is a suggestion, not a state to clear.
 *
 * It is the one-origin-one-localStorage the theme and the position memory
 * already use, and it is CHECKED rather than followed — anything on this
 * origin can write anything into that key. A string, short enough to have been
 * typed, and recent: after half an hour a prefilled field is a question the
 * reader has stopped asking.
 */

const QUERY_KEY = 'abap2ui5-playground:search';
const QUERY_TTL = 30 * 60 * 1000;
const QUERY_MAX = 120;

/** Write down what was typed, as a hit is opened. An empty or absurd query
 *  clears the memory rather than storing itself. */
export function rememberQuery(query) {
  if (typeof localStorage === 'undefined') return;
  const q = (query || '').trim();
  try {
    if (!q || q.length > QUERY_MAX) localStorage.removeItem(QUERY_KEY);
    else localStorage.setItem(QUERY_KEY, JSON.stringify({ q, at: Date.now() }));
  } catch {
    /* A refused or full storage. The reader types it again, as before. */
  }
}

/** What to open the box with, or `''` — which is every case that is not a
 *  recent query written by this box. */
export function recallQuery() {
  if (typeof localStorage === 'undefined') return '';
  let record = null;
  try {
    record = JSON.parse(localStorage.getItem(QUERY_KEY) || 'null');
  } catch {
    return ''; /* not JSON: not something this wrote */
  }
  if (!record || typeof record.q !== 'string' || typeof record.at !== 'number') return '';
  const age = Date.now() - record.at;
  /* Backwards too. A clock that moved, or a timestamp somebody put in the
   * future, is not an age this trusts. */
  if (!(age >= 0 && age < QUERY_TTL)) return '';
  const q = record.q.trim();
  return q && q.length <= QUERY_MAX ? q : '';
}
