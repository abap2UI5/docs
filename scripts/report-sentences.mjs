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
//   node scripts/report-sentences.mjs --full          the whole sentence, not its start

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

// Two pages are records, not prose to shorten: the release notes repeat what
// each release said, and the license is a legal text.
const RECORDS = /resources\/(changelog|license)\.md$/;

/** A prose line with every inline code span turned into one word. proseLines
 *  blanks code, which is right for spelling and wrong for counting: a
 *  sentence that starts with a method name would have no first word, and
 *  would not be seen as a sentence start. */
function withCodeWords(original, prose) {
  let out = '';
  for (let i = 0; i < prose.length; i++) {
    if (prose[i] === ' ' && original[i] === '`') {
      const run = original.slice(i).match(/^(`+)[\s\S]*?\1/);
      const length = run ? run[0].length : 1;
      out += ' Code ';
      i += length - 1;
    } else out += prose[i];
  }
  return out;
}

/** The sentences of a page: prose only. A table row, a heading and the alt
 *  text of an image are left out (none is a sentence), and a paragraph is
 *  read across its line breaks. */
export function sentences(markdown) {
  const out = [];
  let paragraph = [];
  let start = 0;
  const flush = () => {
    const text = paragraph.join(' ').replace(/\s+/g, ' ').trim();
    for (const s of text.split(/(?<=[.!?:]\**[)"”]?)\s+(?=[A-Z*[(]|abap2UI5\b)/)) {
      const words = s.split(' ').filter((w) => /\p{L}/u.test(w)).length;
      // a run of nothing but code - a list of names, a signature - is not a sentence
      const prose = s.split(' ').filter((w) => /\p{L}/u.test(w) && w !== 'Code').length;
      if (words && prose > words / 2) out.push({ line: start, words, text: s });
    }
    paragraph = [];
  };
  const original = markdown.split('\n');
  proseLines(markdown).forEach((line, i) => {
    const t = withCodeWords(original[i] ?? '', line).trim();
    if (!t || /^(#|\||:::|---|<|!\[)/.test(t) || /^description:/.test(original[i] ?? '')) return flush();
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
  if (RECORDS.test(file)) continue;
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
    console.log(`  ${file}:${s.line}  ${s.words} words  ${flag('--full') ? s.text : `${s.text.slice(0, 90)}…`}`);
  }
}
console.log(
  `report-sentences: ${long} of ${total} sentences over ${max} words ` +
  `(${total ? ((100 * long) / total).toFixed(1) : 0}%) on ${pages.length} page(s).`,
);
