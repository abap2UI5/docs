/*
 * catalogue — the one parser for a SAMPLES.md row.
 *
 * Extracted from link-samples.mjs so it can be tested without running it: the
 * script resolves a samples checkout and rewrites pages at import time, and a
 * parser that has silently stopped matching TWICE deserves a test of its own
 * (test/catalogue.test.mjs).
 *
 * The row shape is maintained in abap2UI5/samples, /samples-controls and
 * /samples-stack - three repositories, none of them this one - and read here
 * and in abap2UI5/ai-mcp. It is a contract between five programs.
 */

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
