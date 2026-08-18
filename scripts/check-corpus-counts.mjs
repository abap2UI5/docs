#!/usr/bin/env node
/*
 * check-corpus-counts — the sample-catalogue page states four numbers, and
 * this is what holds them to the catalogues.
 *
 * `resources/samples.md` is the page that answers "which of the three sample
 * repositories do I search?", and it opens with a total: "614 working apps, in
 * three repositories", then one count per repository in the table below. Every
 * one of those is typed by hand, and every one of them describes a repository
 * whose CI cannot see this page.
 *
 * They had drifted - the page said 431 for samples-controls and 615 in total
 * while the published catalogue said 430 and the three summed to 614. Small,
 * and exactly the kind of small that a reader has no way to detect: a figure
 * on the page that introduces the catalogues is taken on trust.
 *
 * generate-llms.mjs solved the same problem for the generated file by counting
 * instead of typing, and leaving the number out when the catalogue is absent.
 * A prose page cannot do that - the sentence is written, not generated - so
 * the number stays and this checks it.
 *
 * Counted through `countCatalogue`, the same parser link-samples.mjs resolves
 * against, so the answer is always "what this repository can resolve" and
 * never a second opinion.
 *
 * A catalogue that is not checked out is NOT a failure: the count is skipped
 * and the run says which ones it could not take. The total needs all three, so
 * it is skipped unless all three are there. A check must not go red because a
 * sibling repository is absent, and must not claim to have verified something
 * it did not.
 *
 *   node scripts/check-corpus-counts.mjs      (npm run check:counts)
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { countCatalogue } from './lib/catalogue.mjs';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const PAGE = 'docs/resources/samples.md';

/* Each claim names the repository it is about and the pattern that carries the
 * number. Declared one by one rather than scanned, so that this file has no
 * opinion about "1.71" or any other figure on the page, and so that adding a
 * count to the page is a decision somebody makes here. */
const CLAIMS = [
  { repo: 'samples', find: /\/samples\/blob\/main\/SAMPLES\.md\)\s*\|\s*(\d+)\s*\|/ },
  { repo: 'samples-controls', find: /\/samples-controls\/blob\/main\/SAMPLES\.md\)\s*\|\s*(\d+)\s*\|/ },
  { repo: 'samples-stack', find: /\/samples-stack\/blob\/main\/SAMPLES\.md\)\s*\|\s*(\d+)\s*\|/ },
];

const TOTAL = { find: /\*\*(\d+) working apps, in three repositories\.\*\*/ };

const file = path.join(ROOT, PAGE);
if (!fs.existsSync(file)) {
  console.error(`${PAGE} is not here — this check names it directly`);
  process.exit(1);
}
const text = fs.readFileSync(file, 'utf8');

const problems = [];
const notes = [];
let checked = 0;

const truth = {};
for (const claim of CLAIMS) {
  const found = text.match(claim.find);
  if (!found) {
    /* The table was restructured. Failing is right: silently no longer
     * checking a number that is still on the page is the outcome to avoid. */
    problems.push(
      `${PAGE}: no match for ${claim.find} (${claim.repo})\n`
      + '    the row carrying this count changed shape — update the pattern here,'
      + ' or drop the claim if the number is gone',
    );
    continue;
  }

  const actual = countCatalogue(claim.repo, ROOT);
  if (actual === null) {
    notes.push(`${claim.repo}: catalogue not here — ${found[1]} not verified`);
    continue;
  }

  truth[claim.repo] = actual;
  checked += 1;
  if (Number(found[1]) !== actual) {
    problems.push(`${PAGE}: says ${found[1]} for ${claim.repo}, its catalogue lists ${actual}`);
  }
}

const totalFound = text.match(TOTAL.find);
if (!totalFound) {
  problems.push(
    `${PAGE}: no match for ${TOTAL.find}\n`
    + '    the opening sentence changed shape — update the pattern here',
  );
} else if (Object.keys(truth).length === CLAIMS.length) {
  const sum = Object.values(truth).reduce((a, b) => a + b, 0);
  checked += 1;
  if (Number(totalFound[1]) !== sum) {
    problems.push(`${PAGE}: opens with ${totalFound[1]} apps in total, the three catalogues list ${sum}`);
  }
} else {
  notes.push(`total: not verified — it needs all three catalogues`);
}

console.log(`check-corpus-counts: ${CLAIMS.length + 1} claim(s) on ${PAGE}, ${checked} checked`);
for (const n of notes) console.log(`  ${n}`);

if (problems.length) {
  console.error(`\n${problems.length} problem(s):`);
  for (const p of problems) console.error(`  ${p}`);
  process.exit(1);
}
if (!checked) {
  console.log('no catalogue reachable — not a failure, but nothing was verified');
} else {
  console.log('every count matches its catalogue - OK');
}
