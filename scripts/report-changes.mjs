#!/usr/bin/env node
// Which pages to read again after a release.
//
// A report, not a gate: `npm run check` does not call it (scripts/lib/
// changes.mjs says why). For every entry of the framework's changelog that
// CHANGES, REMOVES or ADDS something, the names it carries and every page of the
// manual that mentions one - with the date the page was last changed, so a
// page nobody opened since the release stands out.
//
//   node scripts/report-changes.mjs                    the newest release
//   node scripts/report-changes.mjs --release 1.149.0  another one
//   node scripts/report-changes.mjs --unreleased       what main has on top
//   node scripts/report-changes.mjs --all              fixed entries too
//   node scripts/report-changes.mjs --stale            only pages not changed since the release
//
// The changelog is read where the API gates read the interface: from
// abap2UI5 main on github.com, or out of a clone with A2UI5_HOME.

import { readFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { dirname, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { fetchInterface, interfaceSource } from './lib/client-interface.mjs';
import { frameworkRef } from './lib/release.mjs';
import { changelogPath, linesNaming, namesIn, parseChangelog } from './lib/changes.mjs';
import { manualPages } from './lib/prose.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const flag = (name) => args.includes(name);
const releaseAt = args.indexOf('--release');

// The records of what changed are not pages to correct: they are where the
// change is written down.
const RECORDS = /resources\/(changelog|whats_new_[^/]+)\.md$/;

const ref = frameworkRef();
let text;
try {
  text = await fetchInterface(ref, { file: changelogPath });
} catch (err) {
  console.error(`report-changes: could not read ${interfaceSource(ref, undefined, changelogPath)} - ${err.message}`);
  console.error('  set A2UI5_HOME to a clone of abap2UI5 to read it without the network.');
  process.exit(1);
}
console.log(`read ${interfaceSource(ref, undefined, changelogPath)}`);

const releases = parseChangelog(text);
const wanted = flag('--unreleased') ? 'unreleased'
  : releaseAt !== -1 ? args[releaseAt + 1]
    : releases.find((r) => r.version !== 'unreleased')?.version;
const release = releases.find((r) => r.version === wanted);
if (!release) {
  console.error(`report-changes: no release ${wanted} in the changelog (${releases.length} parsed).`);
  process.exit(1);
}

// Added entries are read too: "set_title titles the shell as well" and
// "message_box_display( icon ) is back" stood under + in 1.151.0, and both
// made a sentence of the manual false.
const kinds = flag('--all') ? ['changed', 'removed', 'added', 'fixed'] : ['changed', 'removed', 'added'];
const entries = release.entries.filter((e) => kinds.includes(e.kind));

const pages = manualPages(join(ROOT, 'docs')).filter((p) => !RECORDS.test(p))
  .map((file) => ({ file, rel: relative(ROOT, file), text: readFileSync(file, 'utf8') }));

const changedOn = new Map();
const lastChange = (rel) => {
  if (!changedOn.has(rel)) {
    let date = '';
    try {
      date = execFileSync('git', ['-C', ROOT, 'log', '-1', '--format=%cs', '--', rel], { encoding: 'utf8' }).trim();
    } catch { /* not a checkout: the dates are left out */ }
    changedOn.set(rel, date);
  }
  return changedOn.get(rel);
};

console.log(`\n${release.version}${release.date ? ` (${release.date})` : ''}: ${entries.length} entr${entries.length === 1 ? 'y' : 'ies'} (${kinds.join(', ')})\n`);

let listed = 0;
let stale = 0;
for (const entry of entries) {
  const names = namesIn(entry.text);
  const short = entry.text.length > 110 ? `${entry.text.slice(0, 107)}...` : entry.text;
  console.log(`${entry.kind}: ${short}`);
  if (!names.length) { console.log('  (names nothing - read it and decide by hand)\n'); continue; }
  const rows = [];
  for (const page of pages) {
    const hits = names.map((name) => ({ name, lines: linesNaming(page.text, name) })).filter((h) => h.lines.length);
    if (!hits.length) continue;
    const date = lastChange(page.rel);
    const old = Boolean(release.date && date && date < release.date);
    if (flag('--stale') && !old) continue;
    rows.push({ page, hits, date, old });
  }
  if (!rows.length) console.log(`  no page names ${names.join(', ')}`);
  for (const row of rows) {
    listed++;
    if (row.old) stale++;
    const where = row.hits.map((h) => `${h.name} (${h.lines.slice(0, 4).join(', ')}${h.lines.length > 4 ? ', ...' : ''})`).join('; ');
    console.log(`  ${row.old ? '!' : ' '} ${row.page.rel}  ${row.date || ''}\n      ${where}`);
  }
  console.log('');
}

console.log(`report-changes: ${listed} page mention(s) to read against ${release.version}`
  + (release.date ? `, ${stale} on a page not changed since ${release.date} (marked !)` : '') + '.');
console.log('             A page that is listed is not wrong - it is where a sentence about the change would be.');
