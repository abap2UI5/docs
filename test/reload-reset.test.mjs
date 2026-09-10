/*
 * A refresh starts over.
 *
 * Reported as: "it now remembers everywhere on the documentation where I was —
 * but when I refresh the page everything should be initial again, the menus
 * folded, when I move between Documentation, Samples and Home."
 *
 * Every memory this site keeps — the chapter menu's shape, how far down the
 * page you were, which page of the other section you left, the last thing you
 * searched for — exists because a click makes a NEW DOCUMENT: it is state one
 * page hands the next so that a journey across pages reads as one. Reload is
 * the reader saying the journey is over, and it is what everyone presses when
 * a page looks wrong; a site that hands back the same half-open menus and the
 * same offset is remembering its way back into the thing they were leaving.
 *
 * Two things this must NOT do, and both are cases below: reset on an ordinary
 * navigation (which would delete the memory on the way to using it), and take
 * the theme with it (a colour scheme is a choice about every page there will
 * ever be, not a place).
 *
 * No browser runs in this suite, so the second half checks the wiring the same
 * way the chapter menu's own file does: against the source that runs it.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

const { arrivedBy, forgetOnReload } = await import('../docs/.vitepress/theme/site-memory.js');

const SECTIONS = 'abap2ui5-playground:docs-sections';
const SEARCH = 'abap2ui5-playground:search';
const THEME = 'abap2ui5-playground:theme';

/** Everything a reader can have on this origin, written down. */
const full = () => ({
  'abap2ui5-playground:last-docs': '/docs/cookbook/model/trees',
  'abap2ui5-playground:last-samples': '/playground/samples/?q=table',
  'abap2ui5-playground:last-playground': '/playground/#code',
  'abap2ui5-playground:scroll': '{"/docs/cookbook/model/trees":1840}',
  'abap2ui5-playground:returning': '{"to":"/docs/","at":0}',
  [SECTIONS]: '{"Cookbook":true,"Cookbook / Model":true}',
  [SEARCH]: '{"q":"table","at":0}',
  [THEME]: 'dark',
});

/** The store the module reads, as a stub, for one call. */
function withStore(store, run) {
  const before = globalThis.localStorage;
  globalThis.localStorage = {
    getItem: (k) => (k in store ? store[k] : null),
    setItem: (k, v) => { store[k] = String(v); },
    removeItem: (k) => { delete store[k]; },
  };
  try {
    return run();
  } finally {
    globalThis.localStorage = before;
  }
}

test('a reload forgets every place the reader was', () => {
  const store = full();
  const forgot = withStore(store, () => forgetOnReload([SECTIONS, SEARCH], 'reload'));
  assert.equal(forgot, true);
  assert.deepEqual(Object.keys(store), [THEME],
    'the menus, the offsets, the four bar items and the search box all start again');
});

test('...and the theme is not a place', () => {
  const store = full();
  withStore(store, () => forgetOnReload([SECTIONS, SEARCH], 'reload'));
  assert.equal(store[THEME], 'dark',
    'a refresh that turned the site white again would be the bug this fixes');
});

test('an ordinary navigation keeps all of it — that is the whole feature', () => {
  for (const how of ['navigate', 'back_forward', 'prerender', '']) {
    const store = full();
    const forgot = withStore(store, () => forgetOnReload([SECTIONS, SEARCH], how));
    assert.equal(forgot, false, `${how || 'an unknown arrival'} is not a refresh`);
    assert.deepEqual(store, full(), `${how || 'an unknown arrival'} left the memory alone`);
  }
});

test('a browser that will not say how you arrived is not a refresh', () => {
  /* Node has no navigation entry, which is the same shape as a browser too old
     for one: arrivedBy answers "" and nothing is forgotten. */
  assert.equal(arrivedBy(), '');
  const store = full();
  withStore(store, () => forgetOnReload([SECTIONS, SEARCH]));
  assert.deepEqual(store, full());
});

test('a refused storage costs nothing', () => {
  const before = globalThis.localStorage;
  globalThis.localStorage = { removeItem: () => { throw new Error('denied'); } };
  try {
    assert.equal(forgetOnReload([SECTIONS], 'reload'), true);
  } finally {
    globalThis.localStorage = before;
  }
});

const SITE = readFileSync(join(ROOT, 'scripts/site-js/site.js'), 'utf8');

test('the page forgets BEFORE anything reads the memory', () => {
  const at = (needle) => {
    const i = SITE.indexOf(needle);
    assert.notEqual(i, -1, `${needle} is no longer in site.js`);
    return i;
  };
  const reset = at('forgetOnReload([');
  for (const reader of ['restoreScroll()', 'rememberHere(', 'function lift()', '(function tree()']) {
    assert.ok(reset < at(reader), `${reader} would read a store the reset had not emptied yet`);
  }
});

test('the two keys the page owns are the two keys the page uses', () => {
  assert.match(SITE, /const SECTIONS_KEY = 'abap2ui5-playground:docs-sections';/);
  assert.match(SITE, /const SEARCH_KEY = 'abap2ui5-playground:search';/);
  assert.match(SITE, /forgetOnReload\(\[SECTIONS_KEY, SEARCH_KEY\]\)/);
  /* The chapter menu reads the same constant it is reset by - it was spelled
     twice in this file, which is one place for the two to drift apart. */
  assert.match(SITE, /const KEY = SECTIONS_KEY;/);
  assert.equal((SITE.match(/abap2ui5-playground:docs-sections/g) || []).length, 1);
});
