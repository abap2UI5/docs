#!/usr/bin/env node
// Do the four bars still agree on what they are made of?
//
// The palette, the two type stacks and the two radii are copied by hand
// between this repository and abap2UI5/playground - deliberately, for the
// reasons in scripts/lib/design.mjs - and until this check existed nothing
// compared the copies. They agreed because whoever last touched one remembered
// the other. One value had already drifted: the two font stacks led with
// different families, the same face on macOS and Windows and two different
// ones on Linux, so the same four words in the same bar measured 59/122/78/97px
// on one site and 65/141/87/110 on the next.
//
// A bar in two greys does not read as a bug in the bar. It reads as a bug in
// the OTHER site - which is how the router's 404 read too.
//
// Usage: node scripts/check-design.mjs [--list]
//   Needs a playground checkout (PLAYGROUND_HOME, .playground or ../playground)
//   or the network; without either it FAILS rather than passing quietly.
//   --list prints every shared value and both sides of it.

import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { palette, compare, SHARED, theirStylesheet } from './lib/design.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const LIST = process.argv.includes('--list');

const mine = palette(readFileSync(join(ROOT, 'docs/.vitepress/theme/style.css'), 'utf8'), 'docs');

/* The floor. A palette this walk could not read is not a palette that agrees:
 * the block moved, or was renamed, and every comparison below would then pass
 * on two empty maps. */
if (mine.light.size === 0 || mine.dark.size === 0) {
  console.error('check-design: no palette found in theme/style.css.');
  console.error('The light block is `:root {`, the dark one `.dark {`.');
  console.error('One of them changed shape - fix the pattern in scripts/lib/design.mjs,');
  console.error('or this gate silently stops comparing anything.');
  process.exit(1);
}

const found = await theirStylesheet(ROOT);
if (!found) {
  console.error('check-design: no playground stylesheet to compare against.');
  console.error('Set PLAYGROUND_HOME, clone abap2UI5/playground as a sibling, or run with network.');
  console.error('Passing without the other side would be this gate reporting that one');
  console.error('stylesheet agrees with itself.');
  process.exit(1);
}

const theirs = palette(found.css, 'playground');
if (theirs.light.size === 0) {
  console.error(`check-design: read ${found.source}, and found no palette in it.`);
  console.error('The block moved over there - fix the pattern in scripts/lib/design.mjs.');
  process.exit(1);
}

const drift = compare(mine, theirs);

console.log(
  `check-design: ${SHARED.length} shared value(s), light and dark, against ${found.source}`,
);

if (LIST) {
  for (const scheme of ['light', 'dark']) {
    console.log(`\n  ${scheme}:`);
    for (const row of SHARED) {
      const a = mine[scheme].get(row.ours);
      if (a === undefined) continue;
      console.log(`    ${row.ours.padEnd(24)} ${a}`);
    }
  }
}

if (drift.length) {
  console.error(`\n${drift.length} value(s) have drifted apart:\n`);
  for (const d of drift) {
    console.error(`  ${d.scheme}  ${d.what}`);
    console.error(`      here  ${d.ours}: ${d.mine}`);
    console.error(`      there ${d.theirs}: ${d.yours}`);
  }
  console.error('\nThese four documents are read as one bar by anybody moving between them.');
  console.error('Change both sides in one go - theme/style.css here, src/catalogue/catalogue.css');
  console.error('and src/shell/shell.css over there - or change this table if a value is');
  console.error('deliberately no longer shared.');
  process.exit(1);
}

console.log('the four bars are made of the same values, in both schemes.');
