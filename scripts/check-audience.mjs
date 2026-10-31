#!/usr/bin/env node
// Is every page of the manual still written for a developer? The phrases of
// the other site - the one for the person who approves abap2UI5 - against
// every prose word of every page but the four whose subject they are.
// scripts/lib/audience.mjs is the list and the reason.
//
//   node scripts/check-audience.mjs

import { readFile } from 'node:fs/promises';
import { dirname, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { manualPages } from './lib/prose.mjs';
import { ALLOWED, PHRASES, findBusinessCase } from './lib/audience.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const DOCS = join(ROOT, 'docs');
const findings = [];
let checked = 0;
let skipped = 0;

for (const file of manualPages(DOCS)) {
  const rel = relative(DOCS, file).split('\\').join('/');
  if (ALLOWED.has(rel)) { skipped++; continue; }
  const source = await readFile(file, 'utf8');
  for (const { line, phrase } of findBusinessCase(source)) {
    findings.push({ file: relative(ROOT, file), line, phrase });
  }
  checked++;
}

/* The floor: a walk that matched no page reports exactly like a clean site. */
if (checked === 0) {
  console.error(`check-audience: walked ${checked} page(s). That is not a corpus this gate can judge -`);
  console.error('             the likely cause is the page walk in scripts/lib/prose.mjs.');
  process.exit(1);
}

if (findings.length) {
  console.error(`check-audience: ${findings.length} place(s) where a page argues the case for abap2UI5 instead of`);
  console.error('             teaching it:\n');
  for (const f of findings) console.error(`  ${f.file}:${f.line}  "${f.phrase}"`);
  console.error('\nThe manual is for developers. What abap2UI5 costs, how it compares and who');
  console.error('approves it is https://abap2ui5.github.io/why/ - link that page (with');
  console.error('{target="_self"}, it shares the origin) and take the sentence out. Only');
  console.error(`${[...ALLOWED].join(', ')} may use these words.`);
  process.exit(1);
}

console.log(`check-audience: no page of ${checked} argues the business case (${skipped} allowed to; `
  + `${PHRASES.length} phrases read).`);
