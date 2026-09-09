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

/* The third reader: the per-sample pages. They used to carry the memory as an
 * inline copy in sample-pages.mjs, a page that could not import a module;
 * they import it now, through samples/page.mjs (src/catalogue/page-entry.mjs),
 * so the keys are the one file's. Both shapes are accepted here, because the
 * checkout CI clones is the playground's main and the two repositories do not
 * land on the same day. What must hold either way: a sample page reaches the
 * same five names, through the module or through the copy.
 *
 * WHICH SHAPE IS READ OFF sample-pages.mjs, the one file every checkout has -
 * not off whether page-entry.mjs is there. CI clones the playground SPARSELY
 * (check.yml, deploy.yml: three files), and the day the playground switched
 * shapes the entry file was simply not in the checkout: this test took that
 * for the old shape, looked for the inline keys, and failed the deploy of a
 * commit that had nothing to do with it. Now a page that loads samples/page.mjs
 * says the shape, and a checkout without the entry file is named as such. */
test('a sample page writes to the same places', { skip: playground ? false : 'no playground checkout' }, () => {
  const entry = path.join(playground, 'src', 'catalogue', 'page-entry.mjs');
  const pages = fs.readFileSync(path.join(playground, 'tools', 'sample-pages.mjs'), 'utf8');
  if (pages.includes('samples/page.mjs')) {
    assert.ok(fs.existsSync(entry),
      'the sample pages load samples/page.mjs, so the checkout has to carry src/catalogue/page-entry.mjs - the sparse checkout in check.yml and deploy.yml names it');
    assert.match(fs.readFileSync(entry, 'utf8'), /from "\.\.\/shell\/site-memory\.mjs"/,
      'the sample pages import the memory rather than copying it');
    return;
  }
  for (const key of ['abap2ui5-playground:last-', 'abap2ui5-playground:scroll', 'abap2ui5-playground:returning'])
    assert.ok(pages.includes(key), `a per-sample page no longer names ${key}`);
});
