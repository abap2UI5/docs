/*
 * The search: what goes into the index, and what comes back out of it.
 *
 * Two halves, both pinned here because both are the kind of thing that breaks
 * quietly. An index built from the wrong field still builds; a matcher that
 * lost its ranking still answers. Neither reports anything, and the box goes
 * on looking like a working search box while it hands back the wrong page.
 *
 * The third half - does the box open, does the arrow key move - is a browser's
 * question and not one this repository can ask; it is the same limit that
 * keeps the position memory's round trip in the playground's Playwright suite.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { sampleEntries, docEntries } from '../scripts/lib/search-index.mjs';
import { search, grouped, highlight } from '../docs/.vitepress/theme/search-engine.js';
import { terms, headings } from '../scripts/lib/pages.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

/* One row of each of the three catalogue shapes. They are three repositories'
 * files, kept over there, and they do NOT agree on their field names - which
 * is the whole reason sampleEntries reads several. */
const SAMPLES = {
  samples: [{
    class: 'z2ui5_cl_smp_app_493',
    file: 'src/01/z2ui5_cl_smp_app_493.clas.abap',
    category: 'Basics',
    title: 'Basics I',
    description: 'Hello World, the Smallest App',
    summary: 'The smallest app that runs.',
    keywords: ['hello', 'world', 'minimal'],
  }],
};
const CONTROLS = {
  ports: [{
    class: 'z2ui5_cl_smpc_app_398',
    file: 'src/01/01/z2ui5_cl_smpc_app_398.clas.abap',
    library: 'sap.m',
    entity: 'sap.m.Carousel',
    title: 'Carousel',
    summary: 'A sample of a Carousel that contains images.',
    keywords: 'carousel images swipe',
  }],
};
const STACK = {
  samples: [{
    class: 'Z2UI5_CL_SMPS_APP_010',
    path: 'src/z2ui5_cl_smps_app_010.clas.abap',
    technology: 'OData',
    title: 'OData V2 Service',
    summary: 'An app talking to an OData service.',
    keywords: ['odata', 'v2'],
  }],
};

test('a sample entry is built from whichever fields its repository uses', () => {
  const [basics] = sampleEntries(SAMPLES, 'Samples');
  assert.equal(basics.area, 'samples');
  assert.equal(basics.group, 'Samples');
  assert.equal(basics.title, 'Basics I — Hello World, the Smallest App');
  assert.equal(basics.code, 'z2ui5_cl_smp_app_493');
  /* The class name decides the URL, and the pages the catalogue publishes are
   * lower case - an upper-case class in the JSON (samples-stack writes them
   * that way) must not produce a link to a page that is not there. */
  const [stack] = sampleEntries(STACK, 'Stack');
  assert.equal(stack.url, 'https://abap2ui5.github.io/playground/samples/z2ui5_cl_smps_app_010/');

  const [carousel] = sampleEntries(CONTROLS, 'Controls');
  assert.match(carousel.terms, /carousel/);
  assert.match(carousel.terms, /sap\.m/, 'the library and the entity are searchable');
  /* `terms` is the words the other fields do NOT already carry. A summary
   * repeated into it is a third of the index's weight bought twice. */
  assert.doesNotMatch(carousel.terms, /images that contains/);
});

test('every catalogue shape is recognised, and nothing else is', () => {
  assert.equal(sampleEntries(SAMPLES, 'x').length, 1);
  assert.equal(sampleEntries(CONTROLS, 'x').length, 1);
  assert.equal(sampleEntries(STACK, 'x').length, 1);
  /* The shape rule is `class` + `file`/`path`, shared with countEntries: the
   * search and the figure in llms.txt must not disagree about what a sample
   * is. A list of something else in the same file is not one. */
  assert.equal(sampleEntries({ categories: ['sap.m', 'sap.ui.table'] }, 'x').length, 0);
  assert.equal(sampleEntries({ counts: { total: 636 } }, 'x').length, 0);
});

test('a page is indexed by its headings and by its words', () => {
  const body = '# Carousels\n\nHow to build one.\n\n## Adding pages\n\nUse the builder.\n';
  const [page] = docEntries([{ section: 'Cookbook', text: 'Carousels', link: '/cookbook/carousel' }], () => body);
  assert.equal(page.area, 'docs');
  assert.equal(page.group, 'Cookbook');
  assert.equal(page.title, 'Carousels');
  assert.deepEqual(page.headings, [['Adding pages', 'adding-pages']]);
  assert.equal(page.url, 'https://abap2ui5.github.io/docs/cookbook/carousel.html');
  /* A directory index is served at the trailing slash and must not become
   * `<dir>/.html`, which is a 404 the reader meets after the search worked. */
  const [index] = docEntries([{ section: 'Tutorial', text: 'Walkthrough', link: '/tutorials/walkthrough/' }], () => body);
  assert.equal(index.url, 'https://abap2ui5.github.io/docs/tutorials/walkthrough/');
});

test('the words of a page are distinct, and the noise is left out', () => {
  const words = terms('# Title\n\nThe carousel and the CAROUSEL, with a `client->nav_app_call( )` and 42.\n').split(' ');
  assert.ok(words.includes('carousel'));
  assert.equal(words.filter((w) => w === 'carousel').length, 1, 'once, whatever the case');
  assert.ok(words.includes('nav_app_call'), 'an API name is exactly what somebody types');
  assert.ok(!words.includes('the') && !words.includes('and'), 'words that are on every page identify none');
  assert.ok(!words.includes('42'));
});

test('a heading anchor is the one VitePress generates', () => {
  assert.deepEqual(headings('## Why abap2UI5?\n').map((h) => h.anchor), ['why-abap2ui5']);
  /* `check_on_event` is `id="check-on-event"` in the built resources/api.html:
   * the underscore is a dash in an anchor, whatever it is in the name. */
  assert.deepEqual(headings('## `client->view_display( )`\n').map((h) => h.anchor), ['client-view-display']);
  /* A fenced block is code, not an outline: a comment starting with ## in an
   * ABAP example is not a section of the page. */
  assert.equal(headings('```abap\n## not a heading\n```\n').length, 0);
});

/* ------------------------------------------------------------ the matcher */

const INDEX = [
  { area: 'docs', group: 'Cookbook', title: 'Popups', text: 'How to open one.', headings: [['Anchored popovers', 'anchored-popovers']], terms: 'popup popover dialog anchored', url: '/docs/cookbook/popup.html' },
  { area: 'docs', group: 'Resources', title: 'Client API', text: 'Every method.', headings: [], terms: 'popup nav_app_call carousel', url: '/docs/resources/api.html' },
  { area: 'samples', group: 'Controls', title: 'Carousel', text: 'A carousel with images.', code: 'z2ui5_cl_smpc_app_398', terms: 'carousel images sap.m', url: '/playground/samples/z2ui5_cl_smpc_app_398/' },
  { area: 'samples', group: 'Samples', title: 'Popup — Message Box', text: 'A message box.', code: 'z2ui5_cl_smp_app_100', terms: 'popup message box', url: '/playground/samples/z2ui5_cl_smp_app_100/' },
];

test('a title beats a mention, whatever area it is in', () => {
  const [best] = search(INDEX, 'popup');
  assert.equal(best.entry.title, 'Popups');
});

test('a class name finds its sample', () => {
  const hits = search(INDEX, 'z2ui5_cl_smpc_app_398');
  assert.equal(hits.length, 1);
  assert.equal(hits[0].entry.title, 'Carousel');
});

test('a second word narrows the answer rather than widening it', () => {
  /* The failure this pins: terms treated as alternatives. Typing more would
   * then return MORE, which reads as the box ignoring what you typed. */
  const one = search(INDEX, 'popup');
  const two = search(INDEX, 'popup message');
  assert.ok(two.length < one.length);
  assert.equal(two[0].entry.code, 'z2ui5_cl_smp_app_100');
  assert.equal(search(INDEX, 'popup carousel zzz').length, 0, 'every word has to be somewhere');
});

test('a heading hit says which heading, so the link can carry its anchor', () => {
  const [hit] = search(INDEX, 'anchored');
  assert.equal(hit.entry.title, 'Popups');
  assert.equal(hit.heading.anchor, 'anchored-popovers');
});

test('the documentation is offered before seven hundred samples of it', () => {
  const groups = grouped(search(INDEX, 'popup'));
  assert.equal(groups[0].label, 'Documentation');
  assert.deepEqual(groups.map((g) => g.label).sort(), ['Controls', 'Documentation', 'Samples'].filter((l) => groups.some((g) => g.label === l)).sort());
});

test('one corpus cannot bury the others', () => {
  const many = Array.from({ length: 40 }, (_, i) => ({
    area: 'samples', group: 'Controls', title: `List ${i}`, text: '', code: `z2ui5_cl_smpc_app_${i}`, terms: 'list', url: `/x/${i}/`,
  }));
  many.push({ area: 'docs', group: 'Cookbook', title: 'Lists', text: '', headings: [], terms: 'list', url: '/docs/list.html' });
  const groups = grouped(search(many, 'list'));
  assert.equal(groups[0].label, 'Documentation');
  assert.ok(groups[1].hits.length <= 6);
});

test('an empty query is not a search for everything', () => {
  assert.deepEqual(search(INDEX, ''), []);
  assert.deepEqual(search(INDEX, '   '), []);
});

test('the highlight marks what was typed and nothing else', () => {
  assert.deepEqual(highlight('Popups and popovers', 'popup'), [['Popup', true], ['s and popovers', false]]);
  assert.deepEqual(highlight('Carousel', 'zzz'), [['Carousel', false]]);
  assert.deepEqual(highlight('', 'popup'), [['', false]]);
  /* Two terms that overlap must not produce overlapping runs, or the row is
   * rendered with a piece of its own title repeated. */
  const parts = highlight('carousel', 'car carousel');
  assert.equal(parts.map(([t]) => t).join(''), 'carousel');
});

test('the search box in the bar carries the cross-site attribute on a sample hit', () => {
  /* The same rule as the Run bar's link and for the same reason: a sample hit
   * leaves this deployment for the catalogue, and a link the router swallows
   * lands on this site's 404 (scripts/lib/cross-site.mjs). The gate cannot see
   * it - the results are built in a browser - so it is checked as source. */
  const box = readFileSync(join(ROOT, 'docs/.vitepress/theme/SearchBox.vue'), 'utf8');
  const fn = box.slice(box.indexOf('function hrefOf'), box.indexOf('function go'));
  assert.match(fn, /target:\s*'_self'/);
});
