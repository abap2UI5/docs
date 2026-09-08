/*
 * The bar, and when an item in it goes back instead.
 *
 * Reported as "the Playground tab — when I go to it the app is always run
 * again, although it had already run before" - and then again for the front
 * door, whose Try it out now example is a playground in a frame. An item is a
 * link, so a press builds a new document: the ABAP runtime boots and the app
 * starts from the top. Measured against a local copy with no network in the
 * way, 2.4 to 2.8 seconds every time, and the app's own state gone with the
 * document. No browser keeps a running page across a FORWARD navigation; the
 * back/forward cache is the only mechanism that keeps one at all, and it
 * applies to a page the reader has been on.
 *
 * So when the page an item opens is still in the tab's history, the item goes
 * to it there. Which entry that is comes out of `entryOf` (theme/site-memory.js),
 * over what navigation.entries() answers - the half with no DOM in it, checked
 * here against histories of every shape. The click itself is source read as
 * text, the way test/cross-site.test.mjs reads the Run bar's link: every item
 * of the bar, the Navigation API first, the one case a page can know without
 * it (opened from that page, history not grown since) as the fallback, and a
 * way out if the step goes nowhere.
 *
 * Whether the browser then hands the page back alive is the browser's call and
 * cannot be measured here at all - this sandbox's headless Chromium has the
 * back/forward cache switched off, and a page with nothing in it is not
 * restored either. What can be checked is that the shortcut is never worse
 * than the link: same destination, and a way out if the step back does
 * nothing.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const { entryOf } = await import('../docs/.vitepress/theme/site-memory.js');

const SITE = 'https://abap2ui5.github.io';
const PLAYGROUND = `${SITE}/playground/`;
/** A history as navigation.entries() answers it: same-origin entries, in
 *  order, each with a url and a key. */
const history = (...urls) => urls.map((url, i) => ({ url: url && new URL(url, SITE).href, key: `k${i}`, index: i }));

test('the playground one step behind is found', () => {
  const h = history('/playground/', '/docs/get_started/about.html');
  assert.equal(entryOf(h, 1, PLAYGROUND, h[1].url), 'k0');
});

test('...and two steps behind, which a plain step back could not reach', () => {
  const h = history('/playground/', '/docs/get_started/about.html', '/docs/cookbook/model.html');
  assert.equal(entryOf(h, 2, PLAYGROUND, h[2].url), 'k0');
});

test('...and one step in FRONT, for a reader who left it with the Back button', () => {
  const h = history('/docs/get_started/about.html', '/playground/');
  assert.equal(entryOf(h, 0, PLAYGROUND, h[0].url), 'k1');
});

test('the nearest of two playgrounds wins, and behind wins a tie', () => {
  const h = history('/playground/', '/docs/a.html', '/playground/', '/docs/b.html', '/playground/');
  assert.equal(entryOf(h, 3, PLAYGROUND, h[3].url), 'k2', 'k2 and k4 are both one step away; behind first');
  assert.equal(entryOf(h, 1, PLAYGROUND, h[1].url), 'k0');
});

test('the page is the path AND the query: a playground on another sample is another page', () => {
  const opened = `${PLAYGROUND}?src=https%3A%2F%2Fraw.githubusercontent.com%2Fx%2Fy.clas.abap`;
  const h = history('/playground/', '/docs/a.html');
  assert.equal(entryOf(h, 1, opened, h[1].url), null, 'the entry is the empty editor, the link is a sample');
  const h2 = history(opened, '/docs/a.html');
  assert.equal(entryOf(h2, 1, opened, h2[1].url), 'k0');
});

test('the fragment is not part of it - the playground drops the code from its own URL once read', () => {
  const h = history('/playground/', '/docs/a.html');
  assert.equal(entryOf(h, 1, `${PLAYGROUND}#code=AAAA`, h[1].url), 'k0');
  const h2 = history('/playground/#code=AAAA', '/docs/a.html');
  assert.equal(entryOf(h2, 1, PLAYGROUND, h2[1].url), 'k0');
});

test('index.html and the directory are one page', () => {
  const h = history('/playground/index.html', '/docs/a.html');
  assert.equal(entryOf(h, 1, PLAYGROUND, h[1].url), 'k0');
});

test('a sample page lives under the same path and is a different page', () => {
  const h = history('/playground/samples/z2ui5_cl_demo_app_001/', '/docs/a.html');
  assert.equal(entryOf(h, 1, PLAYGROUND, h[1].url), null);
});

test('the current entry itself is never the answer', () => {
  const h = history('/playground/');
  assert.equal(entryOf(h, 0, PLAYGROUND, h[0].url), null);
});

test('an entry the API will not describe, and a link that is not a URL, are simply not matches', () => {
  const h = [{ url: null, key: 'k0', index: 0 }, ...history('/docs/a.html').map((e) => ({ ...e, key: 'k1', index: 1 }))];
  assert.equal(entryOf(h, 1, PLAYGROUND, h[1].url), null, 'a cross-origin entry has no url');
  assert.equal(entryOf(history('/playground/', '/docs/a.html'), 1, 'javascript:alert(1)', `${SITE}/docs/a.html`), null);
  assert.equal(entryOf([], 0, PLAYGROUND, PLAYGROUND), null);
});

/* ---- the click, as written --------------------------------------------- */
const SRC = readFileSync(join(ROOT, 'scripts/site-js/site.js'), 'utf8');
const shortcut = SRC.slice(
  SRC.indexOf('(function barBehindYou()'),
  SRC.indexOf('/* Where on the page, not only which page.'),
);

test('the shortcut exists and is about every item of the bar', () => {
  assert.ok(shortcut.length > 200, 'the block should still be there to reason about');
  assert.match(shortcut, /closest\?\.\('\.bar-nav a\[href\]'\)/, 'one rule for the four items, not four');
  assert.doesNotMatch(shortcut, /data-site="playground"/, 'the Playground item is not a special case any more');
});

test('the item for the page the reader is on is left to the browser', () => {
  assert.match(shortcut, /if \(want === bare\(new URL\(location\.href\)\)\) return;/,
    'Documentation on every page of the manual points at this page; stepping back to an older copy of it would be a surprise');
  assert.match(shortcut, /if \(a\.target && a\.target !== '_self'\) return;/,
    'a link that opens elsewhere has nothing to go back to');
});

test('a page handed back alive spends the scroll record written on the way out', () => {
  assert.match(shortcut, /addEventListener\('pageshow', \(e\) => \{ if \(e\.persisted\) takeHandoff\(\); \}\)/,
    'a restored page is where the reader left it; the record must not reach the next arrival');
  assert.match(SRC, /import \{ entryOf, [^}]*takeHandoff \} from '\.\/site-memory\.js'/);
});

test('it asks the Navigation API first, over the whole same-origin history', () => {
  assert.match(shortcut, /nav\.entries\(\)/, 'navigation.entries() is the history it searches');
  assert.match(shortcut, /entryOf\(nav\.entries\(\), nav\.currentEntry\?\.index \?\? -1, href\)/,
    'the same function the tests above hold, on the lifted href');
  assert.match(shortcut, /nav\.traverseTo\(key\)\.committed/, 'a traversal, with its rejection as the signal');
  assert.match(SRC, /import \{ entryOf, /, 'entryOf comes from theme/site-memory.js like the rest of the memory');
});

test('without it, only a reader who came from that page itself steps back', () => {
  assert.match(shortcut, /document\.referrer/, 'where this document was opened from is the whole condition');
  assert.match(shortcut, /bare\(new URL\(document\.referrer\)\) === bare\(new URL\(href, location\.href\)\)/,
    'the whole page - a sample page lives under the same path and is a different page');
  assert.match(shortcut, /history\.length < 2/,
    'a tab the playground OPENED has nothing behind it, and a dead press is worse than a reload');
  assert.match(shortcut, /history\.length !== behind/,
    'an anchor or a text fragment pushed since means one step back is something else');
  assert.match(shortcut, /history\.back\(\)/);
});

test('a press that means "in a new tab" still means that', () => {
  for (const key of ['metaKey', 'ctrlKey', 'shiftKey', 'altKey']) {
    assert.ok(shortcut.includes(`e.${key}`), `${key} must keep the browser's own handling`);
  }
  assert.match(shortcut, /e\.button !== 0/);
});

test('a step that does not navigate still follows the link', () => {
  assert.match(shortcut, /setTimeout\(\(\) => \{ location\.href = href; \}, \d+\)/,
    'the fallback is what keeps this from ever being a dead press');
  assert.match(shortcut, /addEventListener\('pagehide', \(\) => clearTimeout\(fallback\), \{ once: true \}\)/,
    'a cached document must not fire it on the way back in and bounce the reader out');
  assert.match(shortcut, /\.catch\(\(\) => \{ clearTimeout\(fallback\); location\.href = href; \}\)/,
    'a traversal the browser refuses is followed as the link, at once');
});
