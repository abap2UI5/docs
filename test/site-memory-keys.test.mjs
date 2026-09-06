/*
 * The five names the cross-site memory is made of, against the copy the
 * playground keeps.
 *
 * The position memory is written on one deployment and read on another. It
 * works because both sides spell the same five localStorage keys the same
 * way, and it is kept that way BY A COMMENT: "the counterpart is
 * theme/site-memory.js in abap2UI5/docs - change one, change the other" over
 * there, and the same sentence facing back. Nothing compared them.
 *
 * A key renamed on one side is the quietest failure this pair can have. No
 * error, no log, nothing red: the reader presses Samples and lands on the
 * front of the catalogue instead of the sample they were reading, which is
 * exactly what the item does when nothing has ever been stored. It was
 * reported twice in one evening for a different reason, which is what makes
 * the case for pinning it.
 *
 * Needs a playground checkout (PLAYGROUND_HOME, .playground or ../playground).
 * Without one this SKIPS and says so - a check that quietly passes when it
 * checked nothing is the failure mode this repository already has a rule
 * about.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';

const HOMES = ['PLAYGROUND_HOME', '.playground', '../playground'];
const playground = HOMES
  .map((d) => (d === 'PLAYGROUND_HOME' ? process.env[d] : d))
  .find((at) => at && fs.existsSync(path.join(at, 'src', 'shell', 'site-memory.mjs')));

/** Every `abap2ui5-playground:…` name a file spells out, as a set. */
const keysIn = (file) =>
  new Set((fs.readFileSync(file, 'utf8').match(/abap2ui5-playground:[a-z-]+/g) || []));

const OURS = keysIn('docs/.vitepress/theme/site-memory.js');

test('this side spells the five keys the memory is made of', () => {
  assert.deepEqual([...OURS].sort(), [
    'abap2ui5-playground:last-docs',
    'abap2ui5-playground:last-playground',
    'abap2ui5-playground:last-samples',
    'abap2ui5-playground:returning',
    'abap2ui5-playground:scroll',
  ]);
});

test('the playground spells them the same way', { skip: playground ? false : 'no playground checkout' }, () => {
  const theirs = keysIn(path.join(playground, 'src', 'shell', 'site-memory.mjs'));
  assert.deepEqual([...theirs].sort(), [...OURS].sort(),
    'the two copies of the cross-site memory no longer agree on which keys they use');
});

/* The third copy: a per-sample page cannot import a module, so sample-pages.mjs
 * writes the same behaviour inline. It builds the per-site key from a prefix
 * and the link's `data-site`, so what can be compared is the prefix and the two
 * whole names. */
test('a sample page writes to the same places', { skip: playground ? false : 'no playground checkout' }, () => {
  const inline = fs.readFileSync(path.join(playground, 'tools', 'sample-pages.mjs'), 'utf8');
  for (const key of ['abap2ui5-playground:last-', 'abap2ui5-playground:scroll', 'abap2ui5-playground:returning'])
    assert.ok(inline.includes(key), `a per-sample page no longer names ${key}`);
});
