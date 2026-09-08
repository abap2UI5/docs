#!/usr/bin/env node
// Rewrites the manual's prose to the house spelling - American - and changes
// nothing else: the rule and the reader that keeps it away from code, link
// targets, tags and settings are scripts/lib/prose.mjs, and
// test/spelling.test.mjs is the gate this satisfies.
//
// Usage: node scripts/fix-spelling.mjs        (npm run fix:spelling)
//
// Every word it changes is printed with its page and line, so a run is
// reviewable in the terminal before it is reviewed in the diff.

import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { americanize, findBritish, manualPages } from './lib/prose.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
let pages = 0;
let words = 0;
for (const page of manualPages(join(ROOT, 'docs')).sort()) {
  const text = readFileSync(page, 'utf8');
  const hits = findBritish(text);
  if (!hits.length) continue;
  for (const hit of hits) console.log(`${relative(ROOT, page)}:${hit.line}: ${hit.word} -> ${hit.american}`);
  writeFileSync(page, americanize(text));
  pages += 1;
  words += hits.length;
}
console.log(words ? `${words} word(s) on ${pages} page(s) rewritten.` : 'nothing to rewrite.');
