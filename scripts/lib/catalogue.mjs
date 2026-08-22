/*
 * catalogue — the one parser for a SAMPLES.md row, and the one place a sample
 * corpus is counted (from a checkout, or from the catalogue.json each sample
 * repository publishes, in that order - see countCatalogue).
 *
 * Extracted from link-samples.mjs so it can be tested without running it: the
 * script resolves a samples checkout and rewrites pages at import time, and a
 * parser that has silently stopped matching TWICE deserves a test of its own
 * (test/catalogue.test.mjs).
 *
 * The row shape is maintained in abap2UI5/samples, /samples-controls and
 * /samples-stack - three repositories, none of them this one - and read here
 * and in abap2UI5/mcp-server. It is a contract between five programs.
 */
import fs from 'node:fs';
import path from 'node:path';

/* One catalogue row, as generate-samples-md.js writes it:
 *
 *   | **Basics I** — Hello World<br><sub>hello world minimal</sub> | [`Z2UI5_CL_SMP_APP_493`](src/01/z2ui5_cl_smp_app_493.clas.abap) |
 *
 * The bold half is only there when the row carries a header of its own (the
 * generator drops one that would just repeat its section heading), so both
 * shapes have to parse.
 *
 * What follows the title is a run of `<br>` blocks, and they are matched as a
 * GROUP rather than counted or typed: the small type in `<sub>` (keywords,
 * then the `@docs` links this script maintains) and, since the samples
 * repository gave every app a `" @summary`, a block of NORMAL type carrying
 * that sentence:
 *
 *   | **Basics I** — Hello World<br>The smallest app that runs.<br><sub>hello world minimal</sub> | [`Z2UI5_CL_SMP_APP_493`](...) |
 *
 * The narrower pattern that expected `<br><sub>` blocks ONLY matched no rows
 * at all the day the sentence arrived - every page's samples block then read
 * "not in the sample catalogue", which is a wrong answer rather than a broken
 * run. That is why this matches loosely: the row shape is maintained in
 * another repository (three of them now), and a block this script does not
 * know about must cost it nothing. */
const ROW = /^\|\s*(?:\*\*(?<title>[^*]+)\*\*\s*(?:—|--)\s*)?(?<sub>[^|<]*?)\s*(?<small>(?:<br>(?:<[a-z]+>[^<]*<\/[a-z]+>|[^<]*))*)\s*\|\s*\[`(?<cls>[A-Z0-9_]+)`\]\((?<path>[^)]+)\)\s*\|/;

/* Where each sample repository's catalogue is, if it is at hand at all. The
 * `samples` row is the list link-samples.mjs has always resolved against, so a
 * checkout that works for one works for the other; CI puts it in `.samples`.
 *
 * Nothing here is required. A count that cannot be taken is reported as `null`
 * and the caller leaves the number out - a checkout step in CI is
 * continue-on-error, a contributor's clone has no siblings at all, and a stale
 * number in a file an agent cites is worse than no number at all. */
const HOMES = {
  samples: ['SAMPLES_HOME', '.samples', '../samples', '../abap2UI5-samples'],
  'samples-controls': ['SAMPLES_CONTROLS_HOME', '.samples-controls', '../samples-controls'],
  'samples-stack': ['SAMPLES_STACK_HOME', '.samples-stack', '../samples-stack'],
};

/* Each sample repository also commits the same catalogue as one JSON document,
 * `catalogue.json` at its root, generated over there from the same scan that
 * writes its SAMPLES.md. That file is reachable without a checkout, which the
 * human page's raw URL also is - but the JSON one declares its rows instead of
 * asking us to re-parse a rendered table, so it is the one fetched. */
const PUBLISHED = (repo) => `https://raw.githubusercontent.com/abap2UI5/${repo}/main/catalogue.json`;

/** How many samples a parsed catalogue.json lists - by counting its entries,
 *  NEVER by reading a `counts` field. The three repositories keep their
 *  entries under different keys (`samples` here and in samples-stack, `ports`
 *  in samples-controls), so what identifies an entry is its shape, the same
 *  way the SAMPLES.md parser identifies a row: something with a class name and
 *  a pointer to its source file. A `counts` field is a claim about the
 *  entries; where the entries themselves are in hand, counting them is the
 *  answer that cannot have gone stale separately. */
export function countEntries(catalogue) {
  if (!catalogue || typeof catalogue !== 'object') return 0;
  let n = 0;
  for (const value of Object.values(catalogue)) {
    if (!Array.isArray(value)) continue;
    for (const entry of value) {
      if (!entry || typeof entry !== 'object') continue;
      const cls = entry.class;
      const file = entry.file ?? entry.path;
      if (typeof cls === 'string' && cls.trim() && typeof file === 'string' && file.trim()) n++;
    }
  }
  return n;
}

/** How many apps `repo` lists today: `{ count, source }`, or null if no
 *  catalogue could be reached at all.
 *
 *  The chain, first answer wins:
 *
 *  1. a local checkout's `catalogue.json` - the repository's own
 *     machine-readable self-description, counted entry by entry
 *  2. a local checkout's `SAMPLES.md`, counted through the same parser the
 *     sample links go through - the fallback for a checkout from before
 *     `catalogue.json` existed, and for the sparse CI checkouts that fetch
 *     only `SAMPLES.md`
 *  3. the `catalogue.json` the repository publishes on its default branch,
 *     fetched - so the file built with no checkout at hand carries the same
 *     figure, not no figure
 *  4. null - the caller leaves the number out
 *
 *  Within a checkout, `catalogue.json` outranks the SAMPLES.md parse
 *  deliberately: the two answer slightly different questions (in
 *  abap2UI5/samples, SAMPLES.md also lists the src/00 system area and the
 *  helper classes; its catalogue.json scopes itself to the portable src/01
 *  set), and steps 1 and 3 read the SAME file, so whichever path runs, the
 *  published figure is the one the repository claims for itself. Step 3 must
 *  never fail a build: a 404 (the file not committed yet), a timeout or an
 *  unreachable network all cost the figure, nothing else. */
export async function countCatalogue(repo, root, { fetchFn = globalThis.fetch } = {}) {
  const dirs = HOMES[repo];
  if (!dirs) throw new Error(`no catalogue location known for ${repo}`);
  for (const dir of dirs) {
    const at = dir.endsWith('_HOME') ? process.env[dir] : path.join(root, dir);
    if (!at) continue;
    const json = path.join(at, 'catalogue.json');
    if (fs.existsSync(json)) {
      let count = 0;
      try { count = countEntries(JSON.parse(fs.readFileSync(json, 'utf8'))); } catch { /* fall through */ }
      if (count > 0) return { count, source: 'checkout catalogue.json' };
    }
    const file = path.join(at, 'SAMPLES.md');
    if (!fs.existsSync(file)) continue;
    const size = parseCatalogue(fs.readFileSync(file, 'utf8')).size;
    if (size > 0) return { count: size, source: 'checkout SAMPLES.md' };
  }
  try {
    const res = await fetchFn(PUBLISHED(repo), { signal: AbortSignal.timeout(10_000) });
    if (res && res.ok) {
      const count = countEntries(JSON.parse(await res.text()));
      if (count > 0) return { count, source: 'published catalogue.json' };
    }
  } catch { /* no network, no number - never a broken build */ }
  return null;
}

/** class name (lower case) -> { label, path } for every app in the catalogue. */
export function parseCatalogue(text) {
  const byClass = new Map();
  let section = '';
  for (const line of text.split('\n')) {
    const head = /^#{2,3}\s+(.+?)\s*$/.exec(line);
    if (head) { section = head[1].replace(/[*`]/g, '').replace(/\s+—.*$/, '').trim(); continue; }
    const m = ROW.exec(line);
    if (!m) continue;
    const g = m.groups;
    // the row's own header plus its short text; a row without a header of its
    // own is titled by the section it sits under, or it would read as an
    // orphaned fragment out here
    const label = [g.title?.trim(), (g.sub || '').trim()].filter(Boolean).join(' — ') || section;
    byClass.set(g.cls.toLowerCase(), { label, path: g.path, section });
  }
  return byClass;
}
