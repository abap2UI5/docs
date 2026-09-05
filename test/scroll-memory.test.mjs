/*
 * Coming back to a page means coming back to WHERE ON IT you were.
 *
 * The bar's Documentation and Samples items already restored the page. They
 * restored the top of it, which on the manual's long chapters and on a
 * catalogue of 770 rows is most of the way to not having remembered anything:
 * the reader who was at sample 400, looked something up and pressed Samples,
 * arrived at sample 1.
 *
 * The offset is now written per path, and restored ON ARRIVAL BY THE BAR AND
 * NOWHERE ELSE — that is the whole design, and it is what these cases are
 * about. A page that restored on every load would fight the browser's own
 * back-and-forward restoration and would drop a reader who followed an
 * ordinary link into the middle of a page with nothing to explain it. So the
 * bar writes one record naming where it is sending them; the page that IS
 * that, arriving within seconds, honours it; everything else ignores it.
 *
 * A browser test on this repository cannot reach any of it: `vitepress
 * preview` serves this site on localhost while the neighbouring deployments
 * are on abap2ui5.github.io, so every case would take the different-origin
 * branch. Here `location` and `localStorage` are stubs.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';

const SITE = 'https://abap2ui5.github.io';

/** The globals the module reads, with a real (in-memory) store behind them. */
function at(path, store = {}, run = () => {}) {
  const before = { location: globalThis.location, localStorage: globalThis.localStorage };
  const [pathname, search] = path.split(/(?=\?)/);
  globalThis.location = {
    href: SITE + path,
    origin: SITE,
    pathname,
    search: search || '',
    hash: '',
  };
  globalThis.localStorage = {
    getItem: (k) => (k in store ? store[k] : null),
    setItem: (k, v) => { store[k] = String(v); },
    removeItem: (k) => { delete store[k]; },
  };
  try {
    return run(store);
  } finally {
    globalThis.location = before.location;
    globalThis.localStorage = before.localStorage;
  }
}

const { rememberScroll, scrollOf, handOff, takeHandoff } =
  await import('../docs/.vitepress/theme/site-memory.js');

const SCROLL = 'abap2ui5-playground:scroll';
const BACK = 'abap2ui5-playground:returning';

test('an offset is written for the page it was taken on, and read back', () => {
  const store = {};
  at('/docs/cookbook/tables', store, () => rememberScroll(1200));
  at('/docs/cookbook/tables', store, () => {
    assert.equal(scrollOf(), 1200);
  });
  /* Another page of the same site is another entry, not the same one. */
  at('/docs/get_started/about', store, () => {
    assert.equal(scrollOf(), 0);
  });
});

test('the catalogue keeps one offset per filtered list, because the URL is the list', () => {
  const store = {};
  at('/playground/samples/?q=table', store, () => rememberScroll(900));
  at('/playground/samples/', store, () => assert.equal(scrollOf(), 0));
  at('/playground/samples/?q=table', store, () => assert.equal(scrollOf(), 900));
});

test('the map does not grow without bound', () => {
  const store = {};
  for (let i = 0; i < 30; i++) at(`/docs/p${i}`, store, () => rememberScroll(i + 1));
  const kept = Object.keys(JSON.parse(store[SCROLL]));
  assert.equal(kept.length, 12);
  /* The twelve most recent, and the oldest is what fell off. */
  assert.ok(kept.includes('/docs/p29'));
  assert.ok(!kept.includes('/docs/p0'));
});

test('the bar hands off, and the page it names restores', () => {
  const store = {};
  at('/playground/samples/', store, () => rememberScroll(4000));
  at('/docs/cookbook/tables', store, () => handOff('https://abap2ui5.github.io/playground/samples/'));
  at('/playground/samples/', store, () => {
    assert.equal(takeHandoff(), 4000);
  });
});

test('arriving anywhere else ignores the record', () => {
  /* It is written before a navigation that may never happen - a middle click,
   * a reader who went somewhere else instead. */
  const store = {};
  at('/playground/samples/', store, () => rememberScroll(4000));
  at('/docs/cookbook/tables', store, () => handOff('/playground/samples/'));
  at('/docs/resources/api', store, () => assert.equal(takeHandoff(), null));
});

test('reading it consumes it, so the next page does not inherit the journey', () => {
  const store = {};
  at('/playground/samples/', store, () => rememberScroll(4000));
  at('/docs/', store, () => handOff('/playground/samples/'));
  at('/playground/samples/', store, () => {
    assert.equal(takeHandoff(), 4000);
    assert.equal(takeHandoff(), null, 'the second read');
  });
});

test('a stale record is not a journey', () => {
  const store = {};
  at('/playground/samples/', store, () => rememberScroll(4000));
  store[BACK] = JSON.stringify({ to: '/playground/samples/', at: Date.now() - 120_000 });
  at('/playground/samples/', store, () => assert.equal(takeHandoff(), null));
});

test('a hash the reader named beats an offset this remembered', () => {
  const store = {};
  at('/docs/cookbook/tables', store, () => rememberScroll(1200));
  at('/docs/cookbook/tables', store, () => handOff('/docs/cookbook/tables'));
  const before = globalThis.location;
  at('/docs/cookbook/tables', store, () => {
    globalThis.location.hash = '#popups';
    assert.equal(takeHandoff(), null);
  });
  globalThis.location = before;
});

test('a link to another host writes no record: it shares no storage', () => {
  const store = {};
  at('/docs/', store, () => handOff('https://github.com/abap2UI5/abap2UI5'));
  assert.equal(store[BACK], undefined);
});

test('what comes out of storage is checked, not followed', () => {
  /* Anything on this origin can write there, and scrollTo takes whatever it is
   * given. Every one of these is a value that is not an offset. */
  for (const junk of ['"top"', '{"/docs/":"1e9"}', '{"/docs/":-40}', '{"/docs/":99999999}', 'null', '[1,2]', 'not json at all']) {
    const store = { [SCROLL]: junk };
    at('/docs/', store, () => assert.equal(scrollOf(), 0, junk));
  }
});

test('a record that is not a record restores nothing', () => {
  for (const junk of ['null', '{}', '{"to":"/docs/"}', '{"to":5,"at":1}', 'nonsense']) {
    const store = { [BACK]: junk, [SCROLL]: JSON.stringify({ '/docs/': 500 }) };
    at('/docs/', store, () => assert.equal(takeHandoff(), null, junk));
  }
});

test('a page with no stored offset restores nothing rather than the top', () => {
  /* `null`, not 0: the caller scrolls only when there is somewhere to scroll
   * to, and "back to the top" is what happens anyway. */
  const store = {};
  at('/docs/', store, () => handOff('/docs/'));
  at('/docs/', store, () => assert.equal(takeHandoff(), null));
});
