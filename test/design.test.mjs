/*
 * The values four documents have to agree on, and the reading of a stylesheet
 * that decides whether they do.
 *
 * check:design compares this repository's palette with abap2UI5/playground's.
 * Both halves of that comparison are easy to get wrong in a way that still
 * reports success: a block pattern that matches nothing compares two empty
 * maps, and a comparison that reads blocks literally reports drift where the
 * CASCADE says a reader sees none. Both mistakes were made while writing it —
 * the first found by the gate's own floor, the second by four false positives
 * on values that simply are not redeclared for dark.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { declarations, palette, compare, SHARED } from '../scripts/lib/design.mjs';

/* The two mechanisms, as the two sides really write them: this site is a
 * VitePress application whose appearance script puts `.dark` on the root; the
 * catalogue is a static page that follows the system unless a choice is
 * stored. */
const DOCS = `
:root {
  --bg: #ffffff;
  --accent: #0a6ed1;
}

.dark {
  --bg: #16171a;
  --accent: #4aa3ff;
}

/* four hundred lines later, a second block of the same shape */
:root,
.dark {
  --radius-control: 5px;
  --vp-font-family-base: system-ui, -apple-system,
                         "Segoe UI", sans-serif;
}
`;

const PLAYGROUND = `
:root {
  --bg: #ffffff;
  --accent: #0a6ed1;
  --radius-control: 5px;
  --font-ui: system-ui, -apple-system, "Segoe UI", sans-serif;
}

@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --bg: #16171a;
    --accent: #4aa3ff;
  }
}

:root[data-theme="dark"] {
  --bg: #16171a;
  --accent: #4aa3ff;
}
`;

test('every declaration in a block is read, however it is wrapped', () => {
  const d = declarations('--a: 1px;\n  --b: system-ui,\n       -apple-system;\n');
  assert.equal(d.get('--a'), '1px');
  /* A stack wrapped over three lines has to compare equal to the same stack on
   * one, or the two sides "differ" by whitespace. */
  assert.equal(d.get('--b'), 'system-ui, -apple-system');
});

test('all the blocks of a stylesheet are read, not the first', () => {
  const p = palette(DOCS, 'docs');
  assert.equal(p.light.get('--bg'), '#ffffff');
  assert.equal(p.dark.get('--bg'), '#16171a');
  /* The second `:root` block, four hundred lines down. A walk that stopped at
   * the first reported these as "not declared" - drift that is not there. */
  assert.equal(p.light.get('--radius-control'), '5px');
  assert.match(p.light.get('--vp-font-family-base'), /^system-ui, -apple-system, "Segoe UI", sans-serif$/);
  /* `:root, .dark {` is both blocks, so the value is in the dark one too. */
  assert.equal(p.dark.get('--radius-control'), '5px');
});

test("the playground's dark block is read through its own mechanism", () => {
  const p = palette(PLAYGROUND, 'playground');
  assert.equal(p.light.get('--accent'), '#0a6ed1');
  assert.equal(p.dark.get('--accent'), '#4aa3ff');
});

test('two stylesheets that say the same thing differently do not count as drift', () => {
  const drift = compare(palette(DOCS, 'docs'), palette(PLAYGROUND, 'playground'));
  assert.deepEqual(drift, [], JSON.stringify(drift, null, 1));
});

test('a value not redeclared for dark keeps its light value, on both sides', () => {
  /* The cascade, which is what a reader sees. One side groups the radius into
   * `:root, .dark` and the other declares it in `:root` alone; comparing the
   * blocks literally reported a difference nobody could see. */
  const theirs = palette(PLAYGROUND, 'playground');
  assert.equal(theirs.dark.get('--radius-control'), undefined, 'not in the dark block');
  assert.equal(compare(palette(DOCS, 'docs'), theirs).length, 0);
});

test('a value that really differs is reported, in the scheme it differs in', () => {
  /* replaceAll, not replace: the playground declares its dark values TWICE -
   * once under the media query for a reader who has expressed no choice, once
   * under [data-theme="dark"] for one who has - and it is the second that the
   * gate reads. Changing only the first is a fixture that proves nothing, and
   * for one run this test passed a broken comparison because of it. */
  const drift = compare(palette(DOCS, 'docs'), palette(PLAYGROUND.replaceAll('--accent: #4aa3ff', '--accent: #ff0000'), 'playground'));
  assert.equal(drift.length, 1);
  assert.equal(drift[0].scheme, 'dark');
  assert.equal(drift[0].ours, '--accent');
  assert.equal(drift[0].mine, '#4aa3ff');
  assert.equal(drift[0].yours, '#ff0000');
});

test('a value renamed on one side is drift, not a value that vanished quietly', () => {
  const drift = compare(palette(DOCS, 'docs'), palette(PLAYGROUND.replace('--font-ui:', '--font-family:'), 'playground'));
  assert.equal(drift.length, 2, 'light and dark');
  assert.equal(drift[0].yours, '(not declared)');
});

test('the table names both spellings of every shared value', () => {
  /* The two sides do not spell them the same - this one writes the type stacks
   * as VitePress's own variables, because that is what the theme reads - which
   * is the whole reason the table exists rather than a diff of two files. */
  for (const row of SHARED) {
    assert.match(row.ours, /^--[a-z-]+$/);
    assert.match(row.theirs, /^--[a-z-]+$/);
    assert.ok(row.what.length > 3, `${row.ours} says what it is for`);
  }
  assert.ok(SHARED.some((r) => r.ours !== r.theirs), 'at least one is spelled differently');
});
