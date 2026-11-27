#!/usr/bin/env node
/*
 * generate-deprecations — the deprecations page as data, generated from it.
 *
 * resources/deprecations.md is the one place this site says what was
 * deprecated, removed or moved out of abap2UI5, and it is written by hand.
 * Tools need the same facts without parsing a page: the linter reported
 * `z2ui5_cl_xml_view` as "planned to move" months after it had moved, and
 * abaplint says `Class z2ui5_cl_pop_to_confirm not found` with no word on
 * where it went. So the table is published a second time, as
 *
 *   docs/public/api/deprecations.json
 *       https://abap2ui5.github.io/docs/api/deprecations.json
 *
 *   { "schema": 1, "source": "<the page>", "matching": "<how a pattern matches>",
 *     "entries": [ { "name", "kind", "of"?, "pattern"?, "status",
 *                    "replacement", "movedTo"?, "since", "note"? } ] }
 *
 * next to client-api.json and committed like it: a tool, or a reader with a
 * checkout and no build, gets the file as it is. The DIRECTION is page to
 * file - the page is the truth, lib/deprecations.mjs says which of its rows
 * are data, and nothing is typed into the JSON. Unlike the API reference
 * this needs no other repository and no network: both halves are in this
 * tree, so --check never skips.
 *
 *   node scripts/generate-deprecations.mjs            rewrite the file
 *   node scripts/generate-deprecations.mjs --check    fail if a rewrite would
 *                                                     change anything
 *   node scripts/generate-deprecations.mjs --list     the rows that stay on
 *                                                     the page only, and why
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { PAGE, parse, render } from './lib/deprecations.mjs';

const ROOT = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUT = 'docs/public/api/deprecations.json';
const CHECK = process.argv.includes('--check');
const LIST = process.argv.includes('--list');

const { entries, skipped, problems } = parse(fs.readFileSync(path.join(ROOT, PAGE), 'utf8'));

if (problems.length) {
  console.error(`${PAGE}: ${problems.length} row(s) of the version table cannot be read as data:\n`);
  for (const p of problems) console.error(`  ${p}`);
  process.exit(1);
}

/* The floor. A reader that matched nothing would write an empty list and the
 * comparison below would hold it fresh for ever. */
if (!entries.length) {
  console.error(`${PAGE}: the version table yielded no entry at all - the first cells stopped being code spans, or lib/deprecations.mjs stopped matching them.`);
  process.exit(1);
}

if (LIST) {
  console.log(`${skipped.length} row(s) of the version table are on the page only:\n`);
  for (const s of skipped) console.log(`  line ${s.line}: ${s.reason}\n    ${s.cell}`);
  console.log('');
}

const next = render(entries);
const file = path.join(ROOT, OUT);
const have = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : '';
const summary = `${entries.length} entries from ${PAGE} (${skipped.length} rows are prose and stay on the page)`;

if (CHECK) {
  if (have !== next) {
    console.error(`${OUT} is stale: ${PAGE} says something the file does not.\nRun: npm run generate:deprecations`);
    process.exit(1);
  }
  console.log(`OK: ${OUT} - ${summary}.`);
} else if (have === next) {
  console.log(`unchanged: ${OUT} - ${summary}.`);
} else {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, next);
  console.log(`written: ${OUT} - ${summary}.`);
}
