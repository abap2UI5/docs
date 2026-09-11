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

/* WORDS THAT DECIDE NOTHING. "how do i install" is one question about one
 * word, and the other three were being scored like it: `a` and `i` are a
 * prefix of something in every entry, `to` and `do` in most, and each one
 * earned the entry a title-hit's worth of points for a word nobody was asking
 * about. The best answer to that query was Client API › check_on_init, and
 * Installation was not in the first four. They are dropped from the matching
 * as long as a real word is left - a query that is ONLY stop words ("is it",
 * or a reader typing "a") still searches for what it says. The list is the
 * short one: a word wrongly dropped is a search that finds nothing. */
const STOP = new Set(('a an and are as at be by can do does for from has have how i in into is it its'
  + ' my not of on or that the this to use using what when where which who will with you your').split(' '));

/* One term as it is matched. The plural is the singular with an `s` on it -
 * "tables" found 33 entries and "table" 202, and every one of the 169 it
 * missed was about tables. The `s` comes off and the prefix match does the
 * rest: "table" is the start of "tables". Not from `ss` (class, address), and
 * not from anything short enough for the rule to be a guess. Only the letters
 * a reader types are ever stemmed; a class name or a control name in the index
 * is matched as it is. */
const stem = (w) => (w.length > 3 && /^\p{L}+s$/u.test(w) && !w.endsWith('ss') ? w.slice(0, -1) : w);

/** The words a query is matched by, in order, and the ones it was not. */
export function queryTerms(query) {
  const all = [...new Set(words(query))];
  const kept = all.filter((w) => !STOP.has(w));
  return (kept.length ? kept : all).map(stem);
}

/* How much a word found in each field is worth. A title hit beats everything:
 * the entries are short and their titles are what a reader is trying to
 * remember. `code` is the ABAP class name, which is either exactly what
 * somebody pasted in or irrelevant - hence high, and only on a prefix. */
const FIELD = { title: 10, code: 8, heading: 5, text: 2, terms: 2, group: 1 };

function scoreField(value, term, weight) {
  if (!value) return 0;
  const hay = normalise(value);
  const at = hay.indexOf(term);
  if (at < 0) return joined(hay, term, weight);
  /* Where the word sits decides how much of the weight it earns: the whole
   * field, the start of it, the start of any word in it, or somewhere inside
   * a longer word - `list` in `ActionListItem` is a real hit and a weaker one
   * than `list` in `List Report`. A term of one or two letters - `ui`, `f4`,
   * `v2` - counts at a word start only: inside a word it is a coincidence
   * (`ui` is in `build`), not a hit. */
  if (hay === term) return weight * 3;
  if (at === 0) return weight * 2;
  if (/[^\p{L}\p{N}]/u.test(hay[at - 1] || '')) return weight;
  if (term.length <= 2) {
    const next = hay.indexOf(term, at + 1);
    return next > 0 && /[^\p{L}\p{N}]/u.test(hay[next - 1]) ? weight : 0;
  }
  return weight / 2;
}

/* THE FIELD WITH ITS SPACES TAKEN OUT. The manual writes "Message Box" and
 * the UI5 control is MessageBox; the reader types either. "messagebox" found
 * the three samples whose title is spelled that way and not the chapter whose
 * heading is two words - nor "selectdialog" a single page, nor "valuehelp" the
 * Value Help chapter. A term that is in no word of the field is looked for in
 * the field closed up, and earns what a word-start hit would: it is one, with
 * the space on the other side of the keyboard. Five letters or more, so that
 * `sap` cannot land across "is a p…". The other way round costs nothing:
 * "select dialog" already finds "SelectDialog" by prefix. */
function joined(hay, term, weight) {
  if (term.length < 5 || !/\s/.test(hay)) return 0;
  const tight = hay.replace(/[^\p{L}\p{N}._]+/gu, '');
  const at = tight.indexOf(term);
  if (at < 0) return 0;
  if (tight === term) return weight * 3;
  return at === 0 ? weight * 2 : weight / 2;
}

/* THE WORDS NEXT TO EACH OTHER. Two terms scored one at a time make "abap
 * cloud" prefer Toolchain › abap-cleaner (a title hit on `abap`, a mention of
 * `cloud`) over In a Nutshell › ABAP Cloud, which is the two words the reader
 * typed, in that order, as a heading. A field that carries the phrase whole
 * earns a bonus of twice its weight on top - three times when the phrase IS
 * the field, which is the title the reader was remembering. The phrase is the
 * query as typed ("drag and drop" is a title) and the query as matched ("drag
 * drop"), whichever the field has. */
function phraseBonus(value, phrases, weight) {
  if (!value || !phrases.length) return 0;
  const hay = normalise(value).replace(/\s+/g, ' ');
  for (const p of phrases) {
    if (hay === p) return weight * 3;
    if (hay.includes(p)) return weight * 2;
  }
  return 0;
}

function phrasesOf(query, terms) {
  if (terms.length < 2) return [];
  const typed = words(query).join(' ');
  const matched = terms.join(' ');
  return typed === matched ? [typed] : [typed, matched];
}

/** One entry scored against the terms, or null if a term is in none of it. */
function scoreEntry(e, terms, phrases) {
  let total = 0;
  /* Which heading matched, so the result can offer the SECTION rather than
   * the page - the difference between "Cookbook: Tables" and the paragraph
   * about sorting. */
  let heading = null;
  /* Did anything but the long tail match. A hit made of `terms` alone is a
   * page that mentions the word somewhere, which is an answer and a weak one;
   * the box can draw it as such. */
  let strong = false;

  for (const term of terms) {
    const head = scoreField(e.title, term, FIELD.title)
      + scoreField(e.code, term, FIELD.code)
      + scoreField(e.text, term, FIELD.text);
    let best = head + scoreField(e.terms, term, FIELD.terms) + scoreField(e.group, term, FIELD.group);
    if (head) strong = true;

    for (const [text, anchor] of e.headings || []) {
      const s = scoreField(text, term, FIELD.heading);
      if (!s) continue;
      best += s;
      strong = true;
      if (!heading || s > heading.score) heading = { text, anchor, score: s };
    }

    if (!best) return null;
    total += best;
  }
  if (!total) return null;

  total += phraseBonus(e.title, phrases, FIELD.title) + phraseBonus(e.text, phrases, FIELD.text);
  for (const [text, anchor] of e.headings || []) {
    const s = phraseBonus(text, phrases, FIELD.heading);
    if (!s) continue;
    total += s;
    if (!heading || s > heading.score) heading = { text, anchor, score: s };
  }

  /* A shorter entry that scored the same is the better answer: the words
   * are a larger part of what it is about. */
  return { entry: e, score: total - Math.min(4, (e.title || '').length / 40), heading, strong };
}

function rank(entries, terms, phrases, limit) {
  const hits = [];
  for (const e of entries) {
    const hit = scoreEntry(e, terms, phrases);
    if (hit) hits.push(hit);
  }
  hits.sort((a, b) => b.score - a.score || (a.entry.title || '').localeCompare(b.entry.title || ''));
  return hits.slice(0, limit);
}

/* ── WHEN NOTHING MATCHES ──────────────────────────────────────────────────
 *
 * "tabel" answered with nothing, and so did "table sort backend" - one for a
 * transposed pair of letters, the other for one word too many. Both are the
 * box telling a reader who is one keystroke from the answer that the project
 * has no page about it. So, and only when the terms as typed find nothing:
 *
 *   1. one word at a time is set aside, rarest first - the word that matched
 *      the fewest entries is the one most likely to be the mistake - until
 *      something answers;
 *   2. a single word that still answers nothing is tried against every word in
 *      every title, one edit away (a letter dropped, added, changed or two of
 *      them swapped): "tabel" is "table", "dialgo" is "dialog".
 *
 * Whatever answered is reported on the result (`hits.relaxedTo`), because a
 * list that silently answers a different question than the one typed is worse
 * than an empty one. Titles only, for the spelling: the long tail of every
 * page is six thousand words, and a near-miss of one of those is a different
 * word more often than a typo of this one.
 */

/** How many entries carry the term anywhere - the measure of a term that is
 *  probably the wrong one. */
function reach(entries, term) {
  let n = 0;
  for (const e of entries) {
    if (scoreEntry(e, [term], [])) n++;
  }
  return n;
}

const vocabularies = new WeakMap();
function vocabulary(entries) {
  let v = vocabularies.get(entries);
  if (!v) {
    v = new Set();
    /* The titles' words and their singulars - "Tables" is a title, and the
     * word a typo is one edit from is "table". */
    for (const e of entries) for (const w of words(e.title)) if (w.length > 2) { v.add(w); v.add(stem(w)); }
    vocabularies.set(entries, v);
  }
  return v;
}

/** Is `b` within one edit of `a` - a letter added, dropped or changed, or two
 *  adjacent ones swapped. Bounded, so it costs nothing on the words that are
 *  not close: most of the vocabulary is out on the length check alone. */
function oneEditAway(a, b) {
  if (a === b) return false;
  const d = a.length - b.length;
  if (d > 1 || d < -1) return false;
  if (d) {
    const [long, short] = d > 0 ? [a, b] : [b, a];
    let i = 0;
    while (i < short.length && long[i] === short[i]) i++;
    return long.slice(i + 1) === short.slice(i);
  }
  let i = 0;
  while (i < a.length && a[i] === b[i]) i++;
  if (i === a.length) return false;
  if (a.slice(i + 1) === b.slice(i + 1)) return true;
  return a[i] === b[i + 1] && a[i + 1] === b[i] && a.slice(i + 2) === b.slice(i + 2);
}

function nearest(entries, term) {
  if (term.length < 4) return null;
  let best = null;
  for (const w of vocabulary(entries)) {
    if (!oneEditAway(term, w)) continue;
    /* Of two candidates, the one that starts the way the reader started:
     * "tabel" is one edit from "table" and one from "label", and nobody who
     * typed a `t` meant an `l`. Then the one on more titles - the word this
     * project actually uses. */
    const n = reach(entries, w) + (w[0] === term[0] ? entries.length : 0);
    if (!best || n > best.n) best = { w, n };
  }
  return best?.w ?? null;
}

/**
 * The entries that match `query`, best first.
 *
 * Every term must be found somewhere in an entry, or the entry is out: typing
 * a second word is how a reader narrows a result list, and a search that
 * treats the words as alternatives grows the list instead, which reads as the
 * box ignoring what you typed. When that leaves nothing, the query is relaxed
 * (above) and the list says so: `hits.relaxedTo` is the query that answered.
 */
export function search(entries, query, { limit = 30 } = {}) {
  let terms = queryTerms(query);
  if (!terms.length) return [];

  let hits = rank(entries, terms, phrasesOf(query, terms), limit);
  if (hits.length) return hits;

  while (terms.length > 1) {
    /* The word on the fewest entries goes; of two on as few, the one typed
     * last - the reader's most recent word is the one most likely one too
     * many. The rest keep their order, which is the reader's. */
    let out = 0;
    terms.forEach((t, i) => {
      if (i && reach(entries, t) <= reach(entries, terms[out])) out = i;
    });
    terms = terms.filter((_, i) => i !== out);
    hits = rank(entries, terms, [terms.join(' ')], limit);
    if (hits.length) { hits.relaxedTo = terms.join(' '); return hits; }
  }

  const near = nearest(entries, terms[0]);
  if (near) {
    hits = rank(entries, [near], [], limit);
    if (hits.length) hits.relaxedTo = near;
  }
  return hits;
}

/**
 * The hits, grouped the way the index declares its areas — with at most
 * `perGroup` in each, so one corpus of 636 ports cannot bury the four pages
 * that explain them.
 *
 * The groups come in the order of their best hit. The documentation used to
 * come first whenever it was in the answer at all, and for "dialog" that put
 * Value Help, Popup, PDF and Lock - four pages that mention the word - above
 * forty-seven samples that are dialogs. It still leads whenever it has a real
 * answer: a documentation hit within half of the best hit anywhere is offered
 * first, because the reader who typed a word that is both a chapter and a
 * control wants the explanation before the seven hundred examples of it. A
 * page that only mentions the word comes after the samples that are about it.
 */
export function grouped(hits, { perGroup = 8 } = {}) {
  const byGroup = new Map();
  /* How many a group HAS, beside how many it shows. A reader who typed
   * "table" and sees eight is looking at eight of two hundred and thirty-one,
   * and the difference between those two numbers is the difference between
   * "that is all there is" and "there is a whole shelf of this". */
  for (const hit of hits) {
    const key = hit.entry.area === 'docs' ? 'Documentation' : hit.entry.group;
    let g = byGroup.get(key);
    if (!g) { g = { label: key, hits: [], total: 0, best: hit.score }; byGroup.set(key, g); }
    g.total++;
    if (hit.score > g.best) g.best = hit.score;
    if (g.hits.length < perGroup) g.hits.push(hit);
  }
  const groups = [...byGroup.values()];
  const top = groups.reduce((m, g) => Math.max(m, g.best), 0);
  groups.sort((a, b) => {
    if (a.label === 'Documentation' && a.best * 2 >= top) return -1;
    if (b.label === 'Documentation' && b.best * 2 >= top) return 1;
    return b.best - a.best;
  });
  return groups.map(({ label, hits: rows, total }) => ({ label, hits: rows, total }));
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
  /* What is marked is what was MATCHED: the stop words are not, and "tables"
   * typed marks "table" in "Tables" - which is the prefix the match was on. */
  const terms = [...new Set(queryTerms(query))].sort((a, b) => b.length - a.length);
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
