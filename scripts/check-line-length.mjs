#!/usr/bin/env node
// A page that is wrapped has to stay wrapped. See scripts/lib/line-length.mjs
// for what that means and why it is not a site-wide column.
//
//   node scripts/check-line-length.mjs           fail on a drifted line
//   node scripts/check-line-length.mjs --list    print which pages are held

import { readFile } from 'node:fs/promises';
import { glob } from 'node:fs/promises';
import { writeFile } from 'node:fs/promises';
import { judge, rewrap, LIMIT } from './lib/line-length.mjs';

const list = process.argv.includes('--list');
const fix = process.argv.includes('--fix');

const pages = [];
for await (const file of glob('docs/**/*.md')) {
  // docs/public holds the GENERATED per-page markdown, which is a projection
  // of the pages next to it and gitignored.
  if (file.startsWith('docs/public/') || file.includes('/.vitepress/')) continue;
  pages.push(file);
}
pages.sort();

let held = 0;
let loose = 0;
const failures = [];
const fixed = [];

for (const file of pages) {
  const source = await readFile(file, 'utf8');
  const verdict = judge(source);
  if (!verdict.wrapped) { loose++; continue; }
  held++;
  if (list) console.log(`  ${file}`);
  if (!verdict.over.length) continue;
  if (fix) {
    const rewrapped = rewrap(source, verdict.over.map((l) => l.line));
    if (rewrapped) {
      await writeFile(file, rewrapped);
      fixed.push({ file, lines: verdict.over.length });
      continue;
    }
  }
  for (const line of verdict.over) {
    failures.push({ file, ...line });
  }
}

// The floor every walking gate here carries: a gate that checked nothing
// reports the same shape as a gate that found nothing wrong.
if (held === 0) {
  console.error(
    `check-line-length: no page of this site counts as wrapped, out of ${pages.length} walked.\n` +
    `             That is not a corpus this gate can judge - the likely cause is the\n` +
    `             page glob, the fence handling, or the limit in scripts/lib/line-length.mjs.`,
  );
  process.exit(1);
}

if (fixed.length) {
  console.log(`check-line-length --fix: rewrapped ${fixed.length} page(s):`);
  for (const f of fixed) console.log(`  ${f.file}  (${f.lines} line(s) were over)`);
  console.log('\nRead the diff: a rewrap changes only whitespace, and it is worth seeing that.');
}

if (failures.length) {
  console.error(
    `check-line-length: ${failures.length} line(s) over ${LIMIT} characters on a page that is otherwise wrapped:\n`,
  );
  for (const f of failures) {
    console.error(`  ${f.file}:${f.line}  ${f.length} characters`);
    console.error(`    ${f.text.slice(0, 100)}${f.text.length > 100 ? '…' : ''}`);
  }
  console.error(
    `\nRewrap the paragraph to the column the rest of the page is set in. An editor\n` +
    `that hands a rewritten paragraph back as one line is how these arrive; the cost\n` +
    `is that the next diff of that paragraph is one changed line instead of three.`,
  );
  process.exit(1);
}

console.log(
  `check-line-length: ${held} wrapped page(s) held to ${LIMIT} characters, ${loose} not wrapped and left alone\n` +
  `             every wrapped page is still wrapped.`,
);
