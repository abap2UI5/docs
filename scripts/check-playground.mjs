#!/usr/bin/env node
// Is every example without a Run button buttonless ON PURPOSE?
//
// `docs/.vitepress/playground.mjs` decides which fenced example the reader can
// start in an embedded playground, and its rules deliberately fail towards no
// button. The gap that leaves — an example nobody ever measured looking
// exactly like an example that can never run — used to live in a hand-written
// ledger in AGENTS.md, which is to say it went stale.
//
// Now it is decided: every complete `z2ui5_if_app` class on a page either has
// a button from the rule engine, or carries a marker directly above its fence
// naming what it needs that a browser has not got:
//
//   <!-- playground: no Run button — SELECTs from VBAK, which no browser database has -->
//
// One without either fails this check. So does a stale marker — one above an
// example that HAS a button, or attached to nothing — so intent can never
// quietly outlive the example it was written about.
//
// What stays a hand measurement, and says so in AGENTS.md: whether a BUTTONED
// example actually starts in a real playground. This script keeps the
// bookkeeping between those measurements honest; it cannot run ABAP.
//
// Usage: node scripts/check-playground.mjs [--list]
//   --list also prints every deliberate exclusion with both its reasons
//          (the engine's, and the page's) — the measurement worklist.

import { readFileSync, readdirSync, statSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { auditPage } from './lib/playground-coverage.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const DOCS = join(ROOT, 'docs');
const LIST = process.argv.includes('--list');

/* The same walk as check-examples.mjs: `public` holds a generated raw-markdown
 * copy of every page, and judging those would double every count. */
const walk = (dir) =>
  readdirSync(dir).flatMap((e) => {
    const p = join(dir, e);
    if (e === '.vitepress' || e === 'node_modules' || e === 'public') return [];
    return statSync(p).isDirectory() ? walk(p) : [p];
  });

const total = { classes: 0, apps: 0, buttons: 0, excluded: 0 };
const undecided = [];
const stale = [];
const excluded = [];

for (const file of walk(DOCS).filter((f) => f.endsWith('.md')).sort()) {
  const page = file.slice(ROOT.length + 1);
  const result = auditPage(readFileSync(file, 'utf8'));
  total.classes += result.classes;
  total.apps += result.apps;
  total.buttons += result.buttons;
  total.excluded += result.excluded.length;
  for (const one of result.undecided) undecided.push({ page, ...one });
  for (const one of result.stale) stale.push({ page, what: one });
  for (const one of result.excluded) excluded.push({ page, ...one });
}

console.log(
  `check-playground: ${total.apps} complete app class(es) on the site — `
  + `${total.buttons} with a Run button, ${total.excluded} excluded on purpose`
  + ` (${total.classes - total.apps} more complete class(es) are not apps)`,
);

if (LIST) {
  console.log('\nexcluded on purpose:');
  for (const { page, name, why, reason } of excluded) {
    console.log(`  - ${page}: ${name}`);
    console.log(`      the engine: ${why}`);
    console.log(`      the page:   ${reason}`);
  }
}

let failed = false;

if (undecided.length) {
  failed = true;
  console.error(`\n${undecided.length} app example(s) have no Run button and no marker saying why:\n`);
  for (const { page, name, why } of undecided) {
    console.error(`  - ${page}: ${name}\n      the engine refuses it: ${why}`);
  }
  console.error('\nEither the example can run — then measure it in a real playground (AGENTS.md');
  console.error('says how) and adjust it or the rules until it gets its button — or it cannot,');
  console.error('and the page says so, directly above the fence:');
  console.error('\n  <!-- playground: no Run button — <what it needs that a browser has not got> -->');
}

if (stale.length) {
  failed = true;
  console.error(`\n${stale.length} stale marker(s):\n`);
  for (const { page, what } of stale) console.error(`  - ${page}: ${what}`);
  console.error('\nA marker records why an example cannot run. When that stops being true —');
  console.error('the example got its button, or moved, or went — the marker goes with it.');
}

if (failed) process.exit(1);
console.log('every buttonless app example carries its reason. Nothing is excluded by accident.');
