/*
 * The cross-site position memory, and specifically the half of it that decides
 * whether a STORED value may be used at all.
 *
 * The bar's Samples item is lifted to whatever the sample catalogue was last
 * left on, and that value comes out of a localStorage shared by three
 * deployments on one origin. Anything running on that origin can write to it,
 * and a stale value outlives the page it named — so `lastVisited` does not
 * return what it reads, it returns the FALLBACK unless what it reads resolves,
 * on this origin, to somewhere still inside the link that was written.
 *
 * That is the part a browser test on this repository cannot reach: `vitepress
 * preview` serves this site on localhost and the catalogue link points at
 * abap2ui5.github.io, so every case below takes the different-origin branch
 * and nothing else is ever exercised. Here `location` is a stub, which is what
 * lets the same-origin cases be the ones that are checked.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';

const KEY = 'abap2ui5-playground:last-samples';
const SAMPLES = 'https://abap2ui5.github.io/playground/samples/';

/** The globals the module reads, as this origin and one stored value. */
function on(origin, stored, run) {
  const before = {
    location: globalThis.location,
    localStorage: globalThis.localStorage,
  };
  globalThis.location = { href: origin + '/docs/get_started/about.html', origin };
  globalThis.localStorage = {
    getItem: (k) => (k === KEY && stored !== undefined ? stored : null),
    setItem: () => {},
  };
  try {
    return run();
  } finally {
    globalThis.location = before.location;
    globalThis.localStorage = before.localStorage;
  }
}

const { lastVisited, rememberHere } = await import('../docs/.vitepress/theme/site-memory.js');

const SITE = 'https://abap2ui5.github.io';

test('nothing stored keeps the link that was written', () => {
  assert.equal(on(SITE, undefined, () => lastVisited('samples', SAMPLES)), SAMPLES);
});

test('a page of the catalogue is followed, filters and all', () => {
  const last = '/playground/samples/?q=table&lib=sap.m';
  assert.equal(on(SITE, last, () => lastVisited('samples', SAMPLES)), last);
});

test("a sample's own page is followed", () => {
  const last = '/playground/samples/z2ui5_cl_smp_app_001/';
  assert.equal(on(SITE, last, () => lastVisited('samples', SAMPLES)), last);
});

test('a hash and a query survive the round trip', () => {
  const last = '/playground/samples/all/?src=samples#z2ui5_cl_smp_app_042';
  assert.equal(on(SITE, last, () => lastVisited('samples', SAMPLES)), last);
});

/* The four that must NOT be followed. Each one resolves to something outside
 * the link that was written, and each one is a link the bar would otherwise
 * have handed a reader. */
test('a protocol-relative value cannot move the link to another host', () => {
  assert.equal(on(SITE, '//example.invalid/x', () => lastVisited('samples', SAMPLES)), SAMPLES);
});

test('a value that normalises out of the section is refused', () => {
  assert.equal(on(SITE, '/playground/samples/../../evil', () => lastVisited('samples', SAMPLES)), SAMPLES);
});

test('a value outside the section is refused even without traversal', () => {
  assert.equal(on(SITE, '/docs/resources/logo.html', () => lastVisited('samples', SAMPLES)), SAMPLES);
});

test('a javascript: value is refused', () => {
  assert.equal(on(SITE, 'javascript:alert(1)', () => lastVisited('samples', SAMPLES)), SAMPLES);
});

/* And the case the preview server is always in: a link to another host shares
 * no storage, so there is nothing of that site to restore. */
test('a link on another origin keeps the link that was written', () => {
  const last = '/playground/samples/?q=table';
  assert.equal(on('http://localhost:4173', last, () => lastVisited('samples', SAMPLES)), SAMPLES);
});

test('an unknown site name reads nothing and changes nothing', () => {
  assert.equal(on(SITE, '/playground/samples/', () => lastVisited('nowhere', SAMPLES)), SAMPLES);
});

/* The writing half. It is two lines, and the one thing worth pinning is that a
 * storage that throws costs the caller nothing - it runs inside the router
 * hook on every route change. */
test('a storage that refuses everything does not throw', () => {
  const before = { location: globalThis.location, localStorage: globalThis.localStorage };
  globalThis.location = { href: `${SITE}/docs/x.html`, origin: SITE, pathname: '/docs/x.html', search: '', hash: '' };
  globalThis.localStorage = {
    getItem() { throw new Error('denied'); },
    setItem() { throw new Error('denied'); },
  };
  try {
    assert.doesNotThrow(() => rememberHere('docs'));
    assert.doesNotThrow(() => lastVisited('samples', SAMPLES));
  } finally {
    globalThis.location = before.location;
    globalThis.localStorage = before.localStorage;
  }
});

test('a scope wider than the link is what restores a section from a deep href', () => {
  /* The other three bars point Documentation at the first page of the manual
   * and still come back to wherever the reader was in it. Without a scope the
   * containment test is against that one page, so every restore falls back -
   * a memory that silently does not work. Written here with the key this file
   * stubs; the case is the shape, not the section. */
  const deep = 'https://abap2ui5.github.io/playground/samples/all/';
  const last = '/playground/samples/z2ui5_cl_smp_app_001/';
  assert.equal(on(SITE, last, () => lastVisited('samples', deep)), deep,
    'without a scope, a sibling page is not inside the link');
  assert.equal(on(SITE, last, () => lastVisited('samples', deep, SAMPLES)), last);
  /* And the scope is a fence, not a door: outside it, the written link stands. */
  assert.equal(on(SITE, '/docs/cookbook/view/definition.html', () => lastVisited('samples', deep, SAMPLES)), deep);
});
