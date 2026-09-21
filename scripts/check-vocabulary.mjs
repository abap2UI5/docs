#!/usr/bin/env node
// Every prose word of the manual, against a dictionary and against the words
// this project uses that a dictionary does not carry. See
// scripts/lib/vocabulary.mjs for what counts as a word here.
//
//   node scripts/check-vocabulary.mjs            fail on a word nothing knows
//   node scripts/check-vocabulary.mjs --unused   name entries no page uses

import { readFile } from 'node:fs/promises';
import { manualPages } from './lib/prose.mjs';
import { findUnknown, projectWords, WORDS_FILE } from './lib/vocabulary.mjs';

const unused = process.argv.includes('--unused');

const words = projectWords();
const pages = manualPages('docs');
const seen = new Set();
const findings = [];
let checked = 0;

for (const file of pages) {
  const source = await readFile(file, 'utf8');
  for (const { line, word } of findUnknown(source, { words })) {
    findings.push({ file, line, word });
  }
  // what the list is actually earning
  for (const { word } of findUnknown(source, { words: [] })) seen.add(word.toLowerCase());
  checked++;
}

// The floor. A walk that matched no page would report the same shape as a walk
// that found nothing wrong.
if (checked === 0 || words.length === 0) {
  console.error(
    `check-vocabulary: walked ${checked} page(s) against ${words.length} project word(s).\n` +
    `             That is not a corpus this gate can judge - the likely cause is the page\n` +
    `             walk in scripts/lib/prose.mjs or an empty ${WORDS_FILE}.`,
  );
  process.exit(1);
}

if (unused) {
  const stale = words.filter((w) => !seen.has(w.toLowerCase()));
  console.log(
    stale.length
      ? `${stale.length} entr(ies) in vocabulary.txt that no page uses any more:\n  ${stale.join('\n  ')}`
      : 'every entry in vocabulary.txt is earning its line.',
  );
  process.exit(0);
}

if (findings.length) {
  const byWord = new Map();
  for (const f of findings) {
    if (!byWord.has(f.word)) byWord.set(f.word, []);
    byWord.get(f.word).push(`${f.file}:${f.line}`);
  }
  console.error(`check-vocabulary: ${byWord.size} word(s) that neither the dictionary nor this project knows:\n`);
  for (const [word, places] of byWord) {
    console.error(`  ${word}`);
    for (const place of places.slice(0, 4)) console.error(`    ${place}`);
    if (places.length > 4) console.error(`    …and ${places.length - 4} more`);
  }
  console.error(
    `\nIf it is a typo, fix the page. If it is a word this project uses and a\n` +
    `dictionary would not carry - project vocabulary, one of SAP's own words, the\n` +
    `name of a tool or a person - add it to scripts/lib/vocabulary.txt, in the group\n` +
    `it belongs to. A word you are unsure about does not go on the list: the list\n` +
    `absorbing doubt is how a gate like this stops catching anything.`,
  );
  process.exit(1);
}

console.log(
  `check-vocabulary: every prose word on ${checked} page(s) is one the dictionary or\n` +
  `             scripts/lib/vocabulary.txt (${words.length} entries) knows.`,
);
