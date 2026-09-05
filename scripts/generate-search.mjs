#!/usr/bin/env node
// The search index the bar reads: every page of this site, every sample in the
// three catalogues, one JSON document at /docs/search-index.json.
//
// Runs inside `docs:build`, before vitepress, so the file is in docs/public/
// when the build copies it - the same arrangement as generate-llms.mjs, and
// for the same reason: it is a projection of the pages and the catalogues next
// to it, so it is generated on every build and gitignored rather than
// committed stale.
//
// A missing sample catalogue costs its rows and never the build. The pages are
// different: a build that indexed no page of its own site has not found a
// catalogue, it has found nothing, and it fails.
//
// Usage: node scripts/generate-search.mjs

import fs from 'node:fs';
import path from 'node:path';
import { ROOT, DOCS } from './lib/pages.mjs';
import { buildIndex } from './lib/search-index.mjs';

const OUT = path.join(DOCS, 'public', 'search-index.json');

const index = await buildIndex(ROOT, { log: (line) => console.log(line) });

const docs = index.entries.filter((e) => e.area === 'docs').length;
const samples = index.entries.length - docs;

/* The floor. An index with no page in it is a broken walk - the sidebar moved,
 * or config.mjs stopped exporting one - and an empty search box that reports
 * nothing wrong is how a reader concludes the site has no page about their
 * question. */
if (docs === 0) {
  console.error('generate-search: not one page of this site is in the index.');
  console.error('The pages come from the sidebar in docs/.vitepress/config.mjs — has it moved?');
  process.exit(1);
}

fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(OUT, JSON.stringify(index));

const kb = Math.round(fs.statSync(OUT).size / 1024);
console.log(`search-index.json: ${docs} page(s) + ${samples} sample(s) (${kb} kB)`);
