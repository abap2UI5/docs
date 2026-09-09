/*
 * Nothing on this site may keep a page out of the back/forward cache.
 *
 * The bar steps back to a page that is still behind the reader so that the
 * browser can hand it back alive - the front door with its running example,
 * a chapter with its Run panel (site.js, test/bar-back.test.mjs). The browser
 * declines the moment a page registers an unload listener, and Firefox
 * declines on beforeunload too; a Cache-Control of no-store declines it
 * everywhere. None of these has a use on a static manual, and the one place
 * the site would learn it had grown one is a console line on the page that
 * came back rebuilt. So the sources are read for them here, before a deploy.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readdirSync, readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const THEME = join(ROOT, 'docs', '.vitepress', 'theme');
const SOURCES = [
  join(ROOT, 'scripts', 'site-js', 'site.js'),
  join(ROOT, 'scripts', 'build-site.mjs'),
  ...readdirSync(THEME).filter((f) => /\.(?:js|vue)$/.test(f)).map((f) => join(THEME, f)),
];

const KEEPS_OUT = [
  [/addEventListener\(\s*['"](?:before)?unload['"]/, 'an unload or beforeunload listener'],
  [/\bon(?:before)?unload\s*=/, 'an unload or beforeunload handler property'],
  [/\bonunload=|\bonbeforeunload=/, 'an inline unload attribute'],
  [/no-store/, 'a Cache-Control of no-store'],
  [/http-equiv="Cache-Control"/i, 'a Cache-Control written into the page'],
];

test('the sources are there to be read', () => {
  assert.ok(SOURCES.length >= 6, `${SOURCES.length} files - the theme directory or the entry module moved`);
});

for (const file of SOURCES) {
  test(`${file.slice(ROOT.length + 1)} keeps no page out of the back/forward cache`, () => {
    const src = readFileSync(file, 'utf8');
    for (const [re, what] of KEEPS_OUT) {
      assert.doesNotMatch(src, re, `${what} keeps every page it lands on out of the cache, and the bar's step back becomes a reload`);
    }
  });
}
