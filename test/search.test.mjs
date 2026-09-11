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
import { sampleEntries, docEntries, trimTerms, enrich } from '../scripts/lib/search-index.mjs';
import { search, grouped, highlight, queryTerms } from '../docs/.vitepress/theme/search-engine.js';
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
  /* Every word has to be somewhere - and when one is nowhere, the answer is
   * for the words that are, and SAYS so (below). */
  const three = search(INDEX, 'popup carousel zzz');
  assert.equal(three.relaxedTo, 'popup carousel');
  assert.ok(three.length);
});

/* ── THE WORDS THAT DECIDE NOTHING ─────────────────────────────────────────
 *
 * "how do i install" was answered with Client API › check_on_init, and
 * Installation was not in the first four: `a` and `i` are a prefix of
 * something in every entry, `to` and `do` in most, and each earned an entry a
 * title-hit's worth of points for a word nobody was asking about. */
const MANUAL = [
  { area: 'docs', group: 'Getting Started', title: 'Installation', text: 'How to install it.', headings: [], terms: 'abapgit pull', url: '/docs/install.html' },
  { area: 'docs', group: 'Resources', title: 'Client API', text: 'Every method, in one place.', headings: [['check_on_init', 'check-on-init'], ['Do I need this?', 'do-i-need-this']], terms: 'init', url: '/docs/api.html' },
  { area: 'docs', group: 'Cookbook', title: 'In a Nutshell', text: 'What it is.', headings: [['ABAP Cloud', 'abap-cloud'], ['How It Works', 'how-it-works']], terms: 'cloud', url: '/docs/nutshell.html' },
  { area: 'docs', group: 'Advanced', title: 'abapGit', text: 'Apps as artifacts.', headings: [['Toolchain', 'toolchain']], terms: 'cloud steampunk', url: '/docs/abapgit.html' },
];

test('a stop word is not a search term while a real word is there', () => {
  assert.deepEqual(queryTerms('how do i install'), ['install']);
  assert.deepEqual(queryTerms('drag and drop'), ['drag', 'drop']);
  const [best] = search(MANUAL, 'how do i install');
  assert.equal(best.entry.title, 'Installation');
  /* A query that is nothing but stop words still asks what it says. */
  assert.deepEqual(queryTerms('is it'), ['is', 'it']);
  assert.ok(search(MANUAL, 'i').length, 'a single letter still searches');
});

test('a letter or two counts at the start of a word only', () => {
  /* `ui` is in `build`, and that is a coincidence, not a hit. */
  const [page] = [{ area: 'docs', title: 'Build', text: '', headings: [], terms: 'guide', url: '/x' }];
  assert.equal(search([page], 'ui').length, 0);
  assert.equal(search([{ ...page, title: 'UI5 Versions' }], 'ui').length, 1);
});

test('the words next to each other beat the words one at a time', () => {
  /* "abap cloud" preferred abapGit (a title hit on `abap`, a mention of
   * `cloud`) over the heading that IS the two words typed. */
  const [best] = search(MANUAL, 'abap cloud');
  assert.equal(best.entry.title, 'In a Nutshell');
  assert.equal(best.heading.anchor, 'abap-cloud');
});

test('the plural finds what the singular finds', () => {
  const rows = [
    { area: 'samples', group: 'Controls', title: 'Table - Editable', text: '', code: 'z2ui5_cl_smpc_app_570', terms: 'table', url: '/a' },
    { area: 'docs', group: 'Cookbook', title: 'Tables', text: '', headings: [], terms: '', url: '/b' },
  ];
  assert.equal(search(rows, 'tables').length, 2, '"tables" found 33 entries and "table" 202, and all 169 were about tables');
  assert.deepEqual(queryTerms('class address'), ['class', 'address'], 'a double s is not a plural');
  assert.deepEqual(queryTerms('z2ui5_cl_smpc_apps'), ['z2ui5_cl_smpc_apps'], 'only letters a reader types are stemmed');
});

test('a compound finds the two words it is made of', () => {
  const rows = [
    { area: 'docs', group: 'Cookbook', title: 'Message', text: '', headings: [['Message Box', 'message-box'], ['Message Toast', 'message-toast']], terms: '', url: '/m' },
    { area: 'samples', group: 'Controls', title: 'Select Dialog', text: '', code: 'z2ui5_cl_smpc_app_103', terms: '', url: '/s' },
    { area: 'samples', group: 'Controls', title: 'Message Box', text: '', code: 'z2ui5_cl_smpc_app_278', terms: '', url: '/t' },
  ];
  const [box, page] = search(rows, 'messagebox');
  assert.equal(box.entry.title, 'Message Box', 'the title that IS the word, closed up, first');
  assert.equal(page.entry.title, 'Message');
  assert.equal(page.heading.anchor, 'message-box', 'and the heading, with its anchor');
  assert.equal(search(rows, 'selectdialog')[0].entry.title, 'Select Dialog');
  /* Three or four letters across a space are a coincidence - "sap" is in
   * "is a p…" - and are not looked for. */
  assert.equal(search([{ area: 'docs', title: 'Is a Page', text: '', headings: [], terms: '', url: '/p' }], 'sap').length, 0);
});

test('a typo is corrected against the titles, and the list says so', () => {
  const rows = [
    { area: 'samples', group: 'Controls', title: 'Label Properties', text: '', code: 'z2ui5_cl_smpc_app_058', terms: 'label', url: '/l' },
    { area: 'docs', group: 'Cookbook', title: 'Tables', text: '', headings: [], terms: '', url: '/t' },
  ];
  /* "tabel" is one edit from "table" and one from "label"; nobody who typed a
   * `t` meant an `l`. */
  const hits = search(rows, 'tabel');
  assert.equal(hits.relaxedTo, 'table');
  assert.equal(hits[0].entry.title, 'Tables');
  assert.equal(search(rows, 'dialgo').length, 0, 'nothing within one edit is an empty list, not a guess');
  assert.equal(search(rows, 'tab').relaxedTo, undefined, 'a hit as typed is never second-guessed');
  assert.equal(search(rows, 'zzzz').relaxedTo, undefined);
});

test('the word that matched nothing is the one set aside', () => {
  const hits = search(MANUAL, 'install steampunk');
  assert.equal(hits.length, 0 + 1, 'both words are somewhere, on different pages: nothing matches both');
  assert.equal(hits.relaxedTo, 'install', 'the rarer word goes first - it is the one most likely wrong');
});

test('a page that only mentions the word comes after the samples that are about it', () => {
  /* "dialog": Value Help, Popup, PDF and Lock - four pages that mention the
   * word - stood above forty-seven samples that are dialogs. */
  const rows = [
    { area: 'docs', group: 'Cookbook', title: 'PDF', text: '', headings: [], terms: 'dialog', url: '/pdf' },
    { area: 'samples', group: 'Controls', title: 'Dialog - Fullscreen', text: '', code: 'z2ui5_cl_smpc_app_274', terms: '', url: '/d' },
  ];
  const groups = grouped(search(rows, 'dialog'));
  assert.deepEqual(groups.map((g) => g.label), ['Controls', 'Documentation']);
  assert.equal(groups[1].hits[0].strong, false, 'and the row knows it is a mention');
  /* But a real answer in the manual still leads - the chapter before the
   * seven hundred examples. */
  rows[0].headings = [['Print Dialog', 'print-dialog']];
  assert.equal(grouped(search(rows, 'dialog'))[0].label, 'Documentation');
});

test('a synonym finds the entry that says it the other way', () => {
  const rows = [
    { area: 'docs', title: 'Input', text: 'An editable field.', headings: [], terms: 'field' },
    { area: 'docs', title: 'Value Help', text: '', headings: [], terms: 'ddic' },
    { area: 'docs', title: 'Editable', text: '', headings: [], terms: 'readonly' },
  ];
  enrich(rows);
  assert.match(rows[0].terms, /\breadonly\b/);
  assert.match(rows[1].terms, /\bf4\b/);
  assert.equal(rows[2].terms, 'readonly', 'not added twice');
  /* And it works end to end: "readonly" found nothing, and every page about
   * it says `editable`. */
  assert.equal(search(rows, 'readonly').length, 2);
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
  const groups = grouped(search(many, 'list', { limit: 500 }));
  assert.equal(groups[0].label, 'Documentation');
  assert.ok(groups[1].hits.length <= 8);
  /* And it SAYS how many it is holding back, which is the difference between
   * "that is all there is" and "there is a shelf of this". The count is of the
   * hits it was GIVEN - which is why the box asks search( ) for a high limit
   * and lets this do the capping, rather than counting a slice of thirty. */
  assert.equal(groups[1].total, 40);
  assert.equal(groups[0].total, 1);
});

test('an empty query is not a search for everything', () => {
  assert.deepEqual(search(INDEX, ''), []);
  assert.deepEqual(search(INDEX, '   '), []);
});

test('the highlight marks what was matched and nothing else', () => {
  assert.deepEqual(highlight('Popups and popovers', 'popup'), [['Popup', true], ['s and popovers', false]]);
  /* Not the stop word, and the plural as the prefix it was matched on. */
  assert.deepEqual(highlight('Tables and rows', 'tables and'), [['Table', true], ['s and rows', false]]);
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

/* ── THE LAST THING YOU SEARCHED FOR ────────────────────────────────────────
 *
 * A hit opens another page, often on another deployment, and the box that
 * opens there used to be empty: a reader comparing three samples of the same
 * control typed the same word three times. So the query is written down as a
 * hit is opened and the next box starts with it - and, because it comes back
 * out of a localStorage anything on this origin can write to, what comes back
 * is checked rather than used. */
const { rememberQuery, recallQuery } = await import('../docs/.vitepress/theme/search-engine.js');

const QUERY_KEY = 'abap2ui5-playground:search';

/** The one global these two touch, as a store this test can look inside. */
function stored(store, run) {
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

test('a query survives the hit that was opened on it', () => {
  const store = {};
  stored(store, () => rememberQuery('table select dialog'));
  assert.equal(stored(store, () => recallQuery()), 'table select dialog');
});

test('an empty query clears the memory rather than storing itself', () => {
  const store = {};
  stored(store, () => rememberQuery('carousel'));
  stored(store, () => rememberQuery('   '));
  assert.equal(store[QUERY_KEY], undefined);
  assert.equal(stored(store, () => recallQuery()), '');
});

test('a query the reader has stopped asking is not offered back', () => {
  /* Half an hour. A field prefilled with yesterday's question is a worse
   * starting point than an empty one. */
  const store = { [QUERY_KEY]: JSON.stringify({ q: 'carousel', at: Date.now() - 31 * 60 * 1000 }) };
  assert.equal(stored(store, () => recallQuery()), '');
});

test('what comes back out of storage is checked, not used', () => {
  for (const junk of [
    'not json',
    'null',
    '"carousel"',
    JSON.stringify({ q: 'carousel' }),
    JSON.stringify({ q: 42, at: Date.now() }),
    JSON.stringify({ q: 'carousel', at: 'now' }),
    /* A timestamp in the future is not an age, and a query longer than
     * anything anybody typed into a 320px field is not one either. */
    JSON.stringify({ q: 'carousel', at: Date.now() + 60_000 }),
    JSON.stringify({ q: 'x'.repeat(500), at: Date.now() }),
  ]) {
    assert.equal(stored({ [QUERY_KEY]: junk }, () => recallQuery()), '', junk.slice(0, 40));
  }
});

test('no storage at all is an empty box, not a broken one', () => {
  const before = globalThis.localStorage;
  globalThis.localStorage = undefined;
  try {
    assert.equal(recallQuery(), '');
    rememberQuery('carousel'); /* must not throw */
  } finally {
    globalThis.localStorage = before;
  }
});

test('a page\'s terms leave out what its title and headings carry, and what every page carries', () => {
  const page = (title, headings, terms) => ({ area: 'docs', title, text: '', headings: headings.map((h) => [h, h]), terms });
  const entries = [
    page('Carousel', ['Adding pages'], 'carousel adding pages client framework builder'),
    page('Table', ['Sorting'], 'table sorting client framework rows'),
    page('Tree', ['Nodes'], 'tree nodes client framework'),
    page('Popups', [], 'popup client dialog'),
  ];
  trimTerms(entries);
  /* "client" is on all four pages and "framework" on three of four: neither
   * narrows anything down. "builder" is on one and stays. */
  assert.equal(entries[0].terms, 'builder');
  assert.equal(entries[1].terms, 'rows');
  /* A word the title or a heading carries is matched there already. */
  assert.doesNotMatch(entries[0].terms, /carousel|adding|pages/);
  /* A sample's terms are not touched: the rule is about the manual's pages. */
  const sample = { area: 'samples', terms: 'client framework' };
  trimTerms([sample, ...entries]);
  assert.equal(sample.terms, 'client framework');
});
