#!/usr/bin/env node
// The search index the boxes of the family read: every page of this site, every
// sample in the three catalogues and the rest of the family (repositories,
// tools, the sister sites, the API - scripts/lib/search-family.mjs), one JSON
// document at /docs/search-index.json.
//
// Runs inside `docs:build`, before vitepress, so the file is in docs/public/
// when the build copies it - the same arrangement as generate-llms.mjs, and
// for the same reason: it is a projection of the pages and the catalogues next
// to it, so it is generated on every build and gitignored rather than
// committed stale.
//
// A missing sample catalogue or sister site costs its rows - and is reported:
// every area a complete index carries must have entries, and none may have
// shrunk by more than half against the index published right now (fetched
// best-effort). Under CI (`CI` set, as on every GitHub Actions run) a missing
// or shrunken area fails the build, so a deploy cannot publish a search that
// lost the addons without anybody noticing; on a laptop it is a warning. The
// pages are stricter still: a build that indexed no page of its own site has
// not lost a source, it has found nothing, and it fails everywhere.
//
// Usage: node scripts/generate-search.mjs

import fs from 'node:fs';
import path from 'node:path';
import { ROOT, DOCS } from './lib/pages.mjs';
import { buildIndex, areaProblems, publishedAreas, PUBLISHED_INDEX } from './lib/search-index.mjs';

const OUT = path.join(DOCS, 'public', 'search-index.json');

const index = await buildIndex(ROOT, { log: (line) => console.log(line) });

const docs = index.entries.filter((e) => e.area === 'docs').length;
const samples = index.entries.filter((e) => e.area === 'samples').length;
const rest = index.entries.length - docs - samples;

/* The floor. An index with no page in it is a broken walk - the sidebar moved,
 * or config.mjs stopped exporting one - and an empty search box that reports
 * nothing wrong is how a reader concludes the site has no page about their
 * question. */
if (docs === 0) {
  console.error('generate-search: not one page of this site is in the index.');
  console.error('The pages come from the sidebar in docs/.vitepress/config.mjs — has it moved?');
  process.exit(1);
}

/* The other floor: every area there, none of them halved. A source that
 * failed is logged above and costs its rows; this is where it stops being
 * silent. */
const published = await publishedAreas();
if (!published) console.log(`  (the published index at ${PUBLISHED_INDEX} could not be read - no comparison against it)`);
const problems = areaProblems(index.areas, { published });
if (problems.length) {
  const strict = Boolean(process.env.CI);
  const say = strict ? console.error : console.warn;
  say(`generate-search: the index is not whole (${problems.length} area(s)):`);
  for (const p of problems) say(`  ${p}`);
  if (strict) {
    say('A source of the index failed (see the lines above). Under CI this fails the build rather than publish a smaller search.');
    process.exit(1);
  }
  say('WARNING: not failing outside CI - the same index would fail the deploy.');
}

fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(OUT, JSON.stringify(index));

const kb = Math.round(fs.statSync(OUT).size / 1024);
console.log(`search-index.json: ${docs} page(s) + ${samples} sample(s) + ${rest} from the rest of the family (${kb} kB)`);
