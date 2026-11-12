#!/usr/bin/env node
// The long sentences of the manual, longest first.
//
// A report, not a gate: `npm run check` does not call it. Most readers of
// this manual read English as a second language, and a sentence of fifty
// words with two asides is where they stop. This names those sentences so
// that a page can be shortened on purpose, one at a time.
//
//   node scripts/report-sentences.mjs                 every sentence over 35 words
//   node scripts/report-sentences.mjs --max 30        another limit
//   node scripts/report-sentences.mjs docs/cookbook   one folder or one page
//   node scripts/report-sentences.mjs --summary       one line per page only

import { readFile } from 'node:fs/promises';
import { statSync } from 'node:fs';
import { dirname, join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { manualPages, proseLines } from './lib/prose.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const flag = (name) => args.includes(name);
const maxAt = args.indexOf('--max');
const max = maxAt === -1 ? 35 : Number(args[maxAt + 1]);
const target = args.find((a, i) => !a.startsWith('--') && (maxAt === -1 || i !== maxAt + 1));
const where = resolve(ROOT, target ?? 'docs');
const pages = statSync(where).isDirectory() ? manualPages(where) : [where];

/** The sentences of a page: prose only, a table row and a heading left out
 *  (a cell is not a sentence), a paragraph read across its line breaks. */
export function sentences(markdown) {
  const out = [];
  let paragraph = [];
  let start = 0;
  const flush = () => {
    const text = paragraph.join(' ').replace(/\s+/g, ' ').trim();
    for (const s of text.split(/(?<=[.!?])\s+(?=[A-Z`*[])/)) {
      const words = s.split(' ').filter((w) => /\p{L}/u.test(w)).length;
      if (words) out.push({ line: start, words, text: s });
    }
    paragraph = [];
  };
  proseLines(markdown).forEach((line, i) => {
    const t = line.trim();
    if (!t || /^(#|\||:::|---|<)/.test(t)) return flush();
    if (/^([-*]|\d+\.)\s/.test(t)) flush();
    if (!paragraph.length) start = i + 1;
    paragraph.push(t.replace(/^([-*]|\d+\.)\s+/, ''));
  });
  flush();
  return out;
}

let total = 0;
let long = 0;
const rows = [];
for (const file of pages) {
  const all = sentences(await readFile(file, 'utf8'));
  const over = all.filter((s) => s.words > max);
  total += all.length;
  long += over.length;
  if (over.length) rows.push({ file: relative(ROOT, file), all: all.length, over });
}

rows.sort((a, b) => b.over.length - a.over.length);
for (const { file, all, over } of rows) {
  console.log(`${file}: ${over.length} of ${all} sentences over ${max} words`);
  if (flag('--summary')) continue;
  for (const s of over.sort((a, b) => b.words - a.words)) {
    console.log(`  ${file}:${s.line}  ${s.words} words  ${s.text.slice(0, 90)}…`);
  }
}
console.log(
  `report-sentences: ${long} of ${total} sentences over ${max} words ` +
  `(${total ? ((100 * long) / total).toFixed(1) : 0}%) on ${pages.length} page(s).`,
);
