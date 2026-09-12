/*
 * The line above the title on every page of the manual — "Documentation ›
 * Cookbook › Model" — and specifically the walk that produces it.
 *
 * It is derived from the sidebar rather than written a second time, which is
 * the whole point of it: the bar used to carry a Guide dropdown restating the
 * sidebar and it drifted twice. What can still go wrong is the WALK, and it
 * goes wrong quietly — a trail that loses a level, or stops one short, on a
 * page nobody opened this week. The shapes below are the shapes config.mjs
 * actually contains, and the last test walks the real sidebar so that a
 * section restructured there cannot leave this file passing against a fiction.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';

import { key, trailFor } from '../docs/.vitepress/theme/crumbs.js';
import config from '../docs/.vitepress/config.mjs';

/** Just the words, which is what a reader sees. */
const words = (sidebar, page) => trailFor(sidebar, page).map((c) => c.text);

test('one spelling for a page, whatever it was written as', () => {
  assert.equal(key('/get_started/about'), '/get_started/about');
  assert.equal(key('/get_started/about.md'), '/get_started/about');
  assert.equal(key('/get_started/about.html'), '/get_started/about');
  // The sidebar writes the walkthrough's index with a trailing slash and the
  // file is `index.md`; both have to arrive at the same string.
  assert.equal(key('/tutorials/walkthrough/'), '/tutorials/walkthrough');
  assert.equal(key('/tutorials/walkthrough/index.md'), '/tutorials/walkthrough');
  assert.equal(key('/cookbook/model/trees#binding'), '/cookbook/model/trees');
  assert.equal(key('/'), '/');
  assert.equal(key(''), '/');
  // The trail is built by putting a slash in front of a `relativePath`; a
  // value that already had one must not become a path matching nothing.
  assert.equal(key('//cookbook/model/trees'), '/cookbook/model/trees');
});

test('the deepest entry that opens the page wins', () => {
  // Every one of these three names /a/one, and the answer is the deepest.
  const sidebar = [
    {
      text: 'Section',
      link: '/a/one',
      items: [
        { text: 'Group', link: '/a/one', items: [{ text: 'Page', link: '/a/one' }] },
      ],
    },
  ];
  assert.deepEqual(words(sidebar, 'a/one.md'), ['Documentation', 'Section', 'Group']);
});

test('a crumb the sidebar gives nowhere to open carries no link', () => {
  // Quickstart, in config.mjs: a label over two steps, not a page.
  const sidebar = [
    {
      text: 'Getting Started',
      link: '/get_started/about',
      items: [{ text: 'Quickstart', items: [{ text: 'Hello World', link: '/get_started/hello_world' }] }],
    },
  ];
  const trail = trailFor(sidebar, 'get_started/hello_world.md');
  assert.deepEqual(trail.map((c) => c.text), ['Documentation', 'Getting Started', 'Quickstart']);
  assert.equal(trail[1].link, '/get_started/about');
  assert.equal(trail[2].link, undefined);
});

test('a page no sidebar names still says which document it is in', () => {
  assert.deepEqual(words([{ text: 'Section', link: '/a/one' }], 'somewhere/else.md'), ['Documentation']);
  assert.deepEqual(words([], 'anything.md'), ['Documentation']);
  assert.deepEqual(words(undefined, 'anything.md'), ['Documentation']);
});

test('the first crumb is the manual, and it opens the manual', () => {
  const [first] = trailFor(config.themeConfig.sidebar, 'cookbook/model/trees.md');
  assert.equal(first.text, 'Documentation');
  // The same page SiteNav.vue's Documentation item points at.
  assert.equal(first.link, '/get_started/about');
});

test('an object sidebar is walked under the longest matching prefix', () => {
  // Not the shape this repository uses today; the day it grows a second
  // sidebar, the trail should not silently come from the first one listed.
  const sidebar = {
    '/': [{ text: 'Everything', link: '/a/one' }],
    '/deep/': [{ text: 'Deep', link: '/deep/one', items: [{ text: 'Inner', link: '/deep/two' }] }],
  };
  assert.deepEqual(words(sidebar, 'deep/two.md'), ['Documentation', 'Deep']);
  assert.deepEqual(words(sidebar, 'a/one.md'), ['Documentation']);
});

test('the real sidebar files the pages where a reader would look for them', () => {
  const sidebar = config.themeConfig.sidebar;
  const cases = [
    ['get_started/about.md', ['Documentation', 'Getting Started']],
    ['get_started/hello_world.md', ['Documentation', 'Getting Started']],
    ['tutorials/walkthrough/step-4.md', ['Documentation', 'Tutorial', 'Walkthrough']],
    ['cookbook/model/trees.md', ['Documentation', 'Cookbook', 'Model']],
    ['cookbook/model/expression_binding.md', ['Documentation', 'Cookbook', 'Model', 'Binding']],
    ['cookbook/view/definition.md', ['Documentation', 'Cookbook', 'View']],
  ];
  for (const [page, expected] of cases) assert.deepEqual(words(sidebar, page), expected, page);
});

test('every page the sidebar names gets a trail longer than the word Documentation', () => {
  // The section headers duplicate their first chapter's link, so a walk that
  // matched shallowly would leave a whole level of pages with no trail at all.
  //
  // The one page entitled to the single crumb is a top-level section's OWN
  // page - the Cookbook's index, the Advanced Topics index - which no row
  // under the section repeats: its title IS the section, so "Documentation"
  // is everything above it. Pinned as exactly one crumb rather than skipped,
  // so a walk that gave such a page a trail of two would be as wrong here as
  // a walk that gave a chapter one.
  const seen = [];
  const walk = (items) => (items || []).forEach((i) => { if (i.link) seen.push(i.link); walk(i.items); });
  walk(config.themeConfig.sidebar);
  const below = (items) => (items || []).flatMap((i) => [i.link, ...below(i.items)]).filter(Boolean);
  const own = new Set(config.themeConfig.sidebar
    .filter((s) => s.link && !below(s.items).includes(s.link))
    .map((s) => s.link));
  assert.ok(own.size > 0, 'expected at least one section with a page of its own');
  assert.ok(seen.length > 100, `expected the manual's sidebar, got ${seen.length} entries`);
  for (const link of seen) {
    const trail = trailFor(config.themeConfig.sidebar, link);
    if (own.has(link)) assert.equal(trail.length, 1, `${link} is a section's own page and gets the one crumb`);
    else assert.ok(trail.length > 1, `no trail for ${link}`);
  }
});
