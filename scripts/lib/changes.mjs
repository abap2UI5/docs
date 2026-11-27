/*
 * changes — the framework's changelog as data, and the names an entry carries.
 *
 * Why this exists. Fifteen gates hold the manual against the framework, and
 * every one of them is a question about NAMES or FORM: does `client->hash_set`
 * exist, does the example compile, is the page wrapped. None can read a
 * sentence. So when 1.151.0 changed what `hash_set( )` does without a
 * listener, what `set_title` titles and how often `set_config_http_post` is
 * asked, every name stayed valid, every gate stayed green, and some twenty-five
 * pages went on describing 1.150 (found by reading all of them, 2026-11-27).
 *
 * What IS decidable is the worklist: an entry of the changelog that changes or
 * removes behaviour names what it is about, and the pages that mention those
 * names are the pages to read again. `report-changes.mjs` prints that list.
 * It is a report and not a gate on purpose - whether a sentence is still true
 * is a judgement, and a gate that turns red after every release until
 * somebody touches forty files teaches people to touch files.
 *
 * The changelog's shape (changelog.txt at the framework's root):
 *
 *   2026-11-25 v1.151.0
 *   -------------------
 *   ! changed      * fixed      + added      - removed
 */

export const changelogPath = 'changelog.txt';

const KINDS = { '!': 'changed', '*': 'fixed', '+': 'added', '-': 'removed' };

/** The releases of a changelog, newest first as the file has them:
 *  `{ version, date, entries: [{ kind, text }] }`. The `unreleased` block is
 *  a release without a date, version `unreleased`. */
export function parseChangelog(text) {
  const releases = [];
  let current = null;
  const lines = text.split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const underlined = /^-{3,}\s*$/.test(lines[i + 1] ?? '');
    const dated = line.match(/^(\d{4}-\d{2}-\d{2})\s+v(\d+\.\d+\.\d+)\s*$/);
    if (underlined && dated) {
      current = { version: dated[2], date: dated[1], entries: [] };
      releases.push(current);
      i++;
      continue;
    }
    if (underlined && /^unreleased\s*$/i.test(line)) {
      current = { version: 'unreleased', date: null, entries: [] };
      releases.push(current);
      i++;
      continue;
    }
    if (underlined) { current = null; i++; continue; }  // "Legend" and the title
    const entry = line.match(/^([!*+-]) (.+)$/);
    if (current && entry) current.entries.push({ kind: KINDS[entry[1]], text: entry[2].trim() });
  }
  return releases;
}

/* Words with an underscore that are not names of anything: none today. The
 * list is here so the first false hit has a place to go. */
const NOT_A_NAME = new Set([]);

/** The names an entry is about: every token with an underscore
 *  (`draft_keep_per_chain`, `z2ui5_if_ui5_exit`, `START_TIMER`), each half of
 *  a component (`cs_event-set_title` is `cs_event` and `set_title`) and a call
 *  without one (`touch( )`, the parenthesis directly after the word - "again
 *  (use ...)" is prose). Lower case, in the order they appear, once each.
 *  A plain word is never a name here: "drafts" or "timers" would list half
 *  the manual. */
export function namesIn(text) {
  const names = [];
  const add = (raw) => {
    const name = raw.toLowerCase();
    if (name.length < 4 || NOT_A_NAME.has(name) || names.includes(name)) return;
    names.push(name);
  };
  const token = /(?<![A-Za-z0-9_])_?[A-Za-z][A-Za-z0-9_]*(?:\(\s*[^()]*\))?/g;
  for (const [match] of text.matchAll(token)) {
    const call = /\(/.test(match);
    const word = match.replace(/\s*\(.*$/, '');
    if (word.includes('_')) add(word);
    else if (call) add(word);
  }
  return names;
}

/** Where `name` stands on a page: 1-based line numbers. Whole names only, any
 *  case - `set_title` is not found in `set_title_launchpad`, `touch` not in
 *  `touched`. */
export function linesNaming(markdown, name) {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const re = new RegExp(`(?<![A-Za-z0-9_])${escaped}(?![A-Za-z0-9_])`, 'i');
  const hits = [];
  markdown.split(/\r?\n/).forEach((line, i) => { if (re.test(line)) hits.push(i + 1); });
  return hits;
}
