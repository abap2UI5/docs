/*
 * The chapter menu, and whose shape it keeps.
 *
 * The menu is a tree of <details>; every page is a fresh document, so what a
 * reader unfolds is written to localStorage and put back on the next page.
 * That much worked. What it wrote was the wrong thing.
 *
 * `toggle` on a <details> is QUEUED, not synchronous. The old code attached
 * one `toggle` listener per section after putting the menu back — so the
 * events fired BY putting it back arrived at that listener, and every
 * navigation wrote the path down to wherever the reader had landed, as though
 * they had opened each of those sections by hand. Measured on the built site
 * over five pages with the menu never touched once: the store went from 1
 * entry to 8. The reader's own choices were still in there, buried under a
 * trail of rooms they had walked through — which is what "it remembers
 * something, just not what I did" looks like from a chair.
 *
 * And two sections could share one entry: the key was the section's link or
 * its text, and four sections in this menu point at their own first page,
 * which is also a section. One key, two <details> — the store came back with
 * the same key twice.
 *
 * No browser runs in this suite, so the second half of this file checks the
 * wiring that was measured working. The first half checks the invariant
 * itself, against the real sidebar: every section's key is its own.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import config from '../docs/.vitepress/config.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

/** Every section of the real menu, keyed the way build-site.mjs keys it. */
function sectionKeys(items, trail = [], out = []) {
  for (const i of items) {
    if (!i.items) continue;
    out.push([...trail, i.text].join(' / '));
    sectionKeys(i.items, [...trail, i.text], out);
  }
  return out;
}

test('every section in the menu has a key of its own', () => {
  const keys = sectionKeys(config.themeConfig.sidebar);
  assert.ok(keys.length > 20, `the menu should have real depth, found ${keys.length} sections`);
  const seen = new Map();
  for (const k of keys) seen.set(k, (seen.get(k) ?? 0) + 1);
  const shared = [...seen].filter(([, n]) => n > 1).map(([k]) => k);
  assert.deepEqual(shared, [], 'two sections with one key move together and cannot be told apart');
});

test('the old key would NOT have been its own — this is what was fixed', () => {
  /* Kept as the reason the trail exists: with `link || text` this menu has
     four collisions today, so the change is not a preference. */
  const linkKeys = (items, out = []) => {
    for (const i of items) {
      if (!i.items) continue;
      out.push(i.link || i.text);
      linkKeys(i.items, out);
    }
    return out;
  };
  const seen = new Map();
  for (const k of linkKeys(config.themeConfig.sidebar)) seen.set(k, (seen.get(k) ?? 0) + 1);
  assert.ok([...seen.values()].some((n) => n > 1),
    'if this ever passes, the sidebar changed - not the reasoning');
});

const SITE = readFileSync(join(ROOT, 'scripts/site-js/site.js'), 'utf8');
const menu = SITE.slice(SITE.indexOf('(function tree()'), SITE.indexOf('---- copy a listing'));

test('only a click writes the menu down, never a toggle', () => {
  assert.equal(/addEventListener\('toggle'/.test(menu), false,
    'toggle is queued, so it also fires for the sections the page itself opened');
  assert.match(menu, /addEventListener\('click'/,
    'a click on a summary is the reader, and it is the only thing that is');
  assert.match(menu, /closest\?\.\('summary'\)/, 'and only a click on a section heading');
});

test('what is written is one decision, not the whole visible menu', () => {
  assert.match(menu, /chosen\[group\.dataset\.key\] = group\.open/,
    'the section that was clicked, and the state it now has');
  assert.match(menu, /JSON\.stringify\(chosen\)/, 'the store is those decisions and nothing else');
  assert.equal(/groups\.filter\(\(g\) => g\.open\)/.test(menu), false,
    'writing every open section is what let a navigation pass for a reader');
});

test('a section nobody touched keeps the shape the build gave it', () => {
  assert.match(menu, /hasOwnProperty\.call\(chosen, g\.dataset\.key\)/,
    'absent is a third state, and it means "the build decides"');
});

test('the way to the current page is opened, and never written down', () => {
  const here = menu.slice(menu.indexOf('const here ='));
  const untilWrite = here.slice(0, here.indexOf('const write ='));
  assert.match(untilWrite, /el\.open = true/, 'a menu that hid the page you are on would be worse');
  assert.equal(/chosen\[/.test(untilWrite), false, 'walking somewhere is not choosing it');
});
