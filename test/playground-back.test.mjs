/*
 * The bar's Playground item, and the one case where it goes back instead.
 *
 * Reported as "the Playground tab — when I go to it the app is always run
 * again, although it had already run before". It is a link, so a press builds
 * a new document: the ABAP runtime boots and the app starts from the top.
 * Measured against a local copy with no network in the way, 2.4 to 2.8 seconds
 * every time, and the app's own state gone with the document. No browser keeps
 * a running page across a FORWARD navigation; the back/forward cache is the
 * only mechanism that keeps one at all, and it applies to going back.
 *
 * So when the playground is the page the reader just came from, the item goes
 * back to it. Measured on the built site, all three cases: arrived from the
 * playground -> history.back() and the playground again; arrived from another
 * docs page -> the link, untouched; arrived from a SAMPLE page, which lives
 * under the same path -> the link, because that is a different page.
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
const SITE = readFileSync(join(ROOT, 'scripts/site-js/site.js'), 'utf8');
const shortcut = SITE.slice(
  SITE.indexOf('(function playgroundBehindYou()'),
  SITE.indexOf('/* Where on the page, not only which page.'),
);

test('the shortcut exists and is about the Playground item', () => {
  assert.ok(shortcut.length > 200, 'the block should still be there to reason about');
  assert.match(shortcut, /a\[data-site="playground"\]/);
});

test('it only fires for a reader who came from the playground itself', () => {
  assert.match(shortcut, /document\.referrer/, 'where this document was opened from is the whole condition');
  assert.match(shortcut, /came\.origin !== home\.origin \|\| bare\(came\) !== bare\(home\)/,
    'a sample page lives under the same path and is a different page');
  assert.match(shortcut, /history\.length < 2/,
    'a tab the playground OPENED has nothing behind it, and a dead press is worse than a reload');
  assert.match(shortcut, /history\.length !== behind/,
    'an anchor or a text fragment pushed since means one step back is something else');
});

test('a press that means "in a new tab" still means that', () => {
  for (const key of ['metaKey', 'ctrlKey', 'shiftKey', 'altKey']) {
    assert.ok(shortcut.includes(`e.${key}`), `${key} must keep the browser's own handling`);
  }
  assert.match(shortcut, /e\.button !== 0/);
});

test('a step back that does not navigate still follows the link', () => {
  assert.match(shortcut, /setTimeout\(\(\) => \{ location\.href = href; \}, \d+\)/,
    'the fallback is what keeps this from ever being a dead press');
  assert.match(shortcut, /addEventListener\('pagehide', \(\) => clearTimeout\(fallback\), \{ once: true \}\)/,
    'a cached document must not fire it on the way back in and bounce the reader out');
});
