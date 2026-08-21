/*
 * The catalogue parser, pinned by a row of every shape the three sample
 * repositories generate.
 *
 * This exists because the parser has now silently stopped matching TWICE, and
 * both times the symptom was a WRONG ANSWER rather than a red run: every page's
 * declaration came back as "`z2ui5_cl_smp_app_493` is not in the sample
 * catalogue — renamed, deleted, or a typo", which is exactly the sentence this
 * check exists to say truthfully. First when the catalogue grew the `@docs`
 * links under the keywords, then when it grew the `" @summary` sentence in
 * normal type above them.
 *
 * The row shape is maintained in abap2UI5/samples, /samples-controls and
 * /samples-stack - three repositories, none of them this one - so the fixture
 * below is a CONTRACT, not a sample of today's output. A block nobody here has
 * seen before must cost this parser nothing.
 *
 *   node --test test/
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { parseCatalogue, countCatalogue, countEntries } from '../scripts/lib/catalogue.mjs';

const ROWS = [
  '## Basics',
  '',
  '| Sample | Class |',
  '|---|---|',
  // title + sub only (the oldest shape)
  '| **Basics I** — Hello World | [`Z2UI5_CL_SMP_APP_493`](src/01/z2ui5_cl_smp_app_493.clas.abap) |',
  // + keywords
  '| **Basics II** — Data Binding<br><sub>binding input button</sub> | [`Z2UI5_CL_SMP_APP_494`](src/01/z2ui5_cl_smp_app_494.clas.abap) |',
  // + summary, + keywords, + docs (today's shape, all four blocks)
  '| **Popup** — Value Help<br>The value help, both halves.<br><sub>f4 search help</sub><br><sub>docs: [cookbook/expert_more/value_help](https://abap2ui5.github.io/docs/cookbook/expert_more/value_help)</sub> | [`Z2UI5_CL_SMP_APP_009`](src/01/z2ui5_cl_smp_app_009.clas.abap) |',
  // no header of its own - titled by its section (samples-controls rows, too)
  '| App state<br>Turns the running app into a URL.<br><sub>bookmark restore url</sub> | [`Z2UI5_CL_SMP_APP_321`](src/00/97/z2ui5_cl_smp_app_321.clas.abap) |',
  // a block this parser has never seen: it must not stop the row parsing
  '| **Future** — Something<br><span>a block from a later generator</span><br><sub>terms</sub> | [`Z2UI5_CL_SMP_APP_999`](src/01/z2ui5_cl_smp_app_999.clas.abap) |',
].join('\n');

test('every row shape the three catalogues generate parses into a pointer', () => {
  const byClass = parseCatalogue(ROWS);
  assert.deepEqual(
    [...byClass.keys()].sort(),
    ['z2ui5_cl_smp_app_009', 'z2ui5_cl_smp_app_321', 'z2ui5_cl_smp_app_493',
      'z2ui5_cl_smp_app_494', 'z2ui5_cl_smp_app_999'],
  );

  assert.equal(byClass.get('z2ui5_cl_smp_app_493').label, 'Basics I — Hello World');
  assert.equal(byClass.get('z2ui5_cl_smp_app_493').path, 'src/01/z2ui5_cl_smp_app_493.clas.abap');

  // the summary and the docs links are metadata, not part of the label
  assert.equal(byClass.get('z2ui5_cl_smp_app_009').label, 'Popup — Value Help');

  /* A row without a header of its own is titled by its section - the
   * generators drop a header that would only repeat the heading above it. */
  assert.equal(byClass.get('z2ui5_cl_smp_app_321').label, 'App state');
  assert.equal(byClass.get('z2ui5_cl_smp_app_321').section, 'Basics');
});

test('a page link survives a block this parser has never seen', () => {
  // the whole point: the shape is maintained elsewhere, so an unknown block
  // must degrade to "ignored", never to "this sample does not exist"
  const byClass = parseCatalogue(ROWS);
  assert.ok(byClass.has('z2ui5_cl_smp_app_999'), 'the row with an unknown block was dropped');
  assert.equal(byClass.get('z2ui5_cl_smp_app_999').path, 'src/01/z2ui5_cl_smp_app_999.clas.abap');
});

/* The corpus sizes in the generated llms.txt are counted rather than typed,
 * and a build may fail to reach a catalogue. Both outcomes are legitimate;
 * what must never happen is a number that is not the count.
 *
 * The chain a count comes down (see countCatalogue): a checkout's
 * catalogue.json, then its SAMPLES.md, then the catalogue.json the repository
 * publishes on its default branch, then no number. The tests below hold one
 * fixture against every link, and against the two ways the last link is
 * allowed to fail: a 404 and no network at all. None of them may ever touch
 * the real network - every one passes its own fetchFn. */

/* catalogue.json, one fixture per repository shape. These mirror the real
 * files: the entries live under a different key in each repository
 * (`samples`, `ports`, `samples` again), other top-level arrays are not
 * entries (`family`, `packages`), and each `counts` field DELIBERATELY LIES
 * here - the parser must count the entries, never repeat a claim about them. */
const JSON_SAMPLES = JSON.stringify({
  repository: 'abap2UI5/samples',
  family: [{ repository: 'abap2UI5/samples', classPrefix: 'z2ui5_cl_smp_app_' }],
  counts: { samples: 999, categories: 23 },
  samples: [
    { class: 'Z2UI5_CL_SMP_APP_493', file: 'src/01/z2ui5_cl_smp_app_493.clas.abap', title: 'Hello World' },
    { class: 'Z2UI5_CL_SMP_APP_494', file: 'src/01/z2ui5_cl_smp_app_494.clas.abap', title: 'Data Binding' },
  ],
});
const JSON_CONTROLS = JSON.stringify({
  repo: 'abap2UI5/samples-controls',
  counts: { entries: 999, byStatus: { checked: 61 } },
  ports: [
    { class: 'Z2UI5_CL_SMPC_APP_001', file: 'src/01/z2ui5_cl_smpc_app_001.clas.abap', deviations: [] },
    { class: 'Z2UI5_CL_SMPC_APP_002', file: 'src/01/z2ui5_cl_smpc_app_002.clas.abap', deviations: [] },
    { class: 'Z2UI5_CL_SMPC_APP_003', file: 'src/01/z2ui5_cl_smpc_app_003.clas.abap', deviations: [] },
  ],
});
const JSON_STACK = JSON.stringify({
  repo: 'abap2UI5/samples-stack',
  packages: [{ package: 'src/odata_v2', technology: 'OData' }],
  samples: [
    { class: 'Z2UI5_CL_SMPS_APP_100', path: 'src/odata_v2/z2ui5_cl_smps_app_100.clas.abap' },
  ],
});

// a fetchFn is a fixture too: what it returns, and whether it was asked at all
const served = (body) => async () => ({ ok: true, status: 200, text: async () => body });
const missing = async () => ({ ok: false, status: 404, text: async () => 'Not Found' });
const offline = async () => { throw new TypeError('fetch failed'); };
const forbidden = async (url) => { throw new Error(`network reached for ${url} although a checkout was at hand`); };

const scratch = () => {
  // an explicit checkout wins over the sibling directories, and a contributor
  // who has one set would otherwise be told the wrong number by these tests
  delete process.env.SAMPLES_HOME;
  delete process.env.SAMPLES_CONTROLS_HOME;
  delete process.env.SAMPLES_STACK_HOME;
  return fs.mkdtempSync(path.join(os.tmpdir(), 'a2ui5-catalogue-'));
};

test('a catalogue.json is counted entry by entry, and its counts field is never believed', () => {
  assert.equal(countEntries(JSON.parse(JSON_SAMPLES)), 2);
  assert.equal(countEntries(JSON.parse(JSON_CONTROLS)), 3);
  assert.equal(countEntries(JSON.parse(JSON_STACK)), 1);
  // no entries in hand and a counts field full of claims: the count is 0,
  // which the chain treats as "this file answered nothing", not as a figure
  assert.equal(countEntries({ counts: { samples: 40 }, samples: [] }), 0);
  assert.equal(countEntries({ samples: [{ class: '', file: 'x' }, { class: 'Z', file: '' }, { title: 'no pointer' }] }), 0);
});

test('a corpus size is counted where the checkout is, without asking the network', async (t) => {
  const dir = scratch();
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }));
  fs.mkdirSync(path.join(dir, '.samples'));
  fs.writeFileSync(path.join(dir, '.samples', 'SAMPLES.md'), ROWS);

  assert.deepEqual(
    await countCatalogue('samples', dir, { fetchFn: forbidden }),
    { count: 5, source: 'checkout SAMPLES.md' },
  );
});

test("a checkout's own catalogue.json outranks the SAMPLES.md parse", async (t) => {
  /* Deliberate, and the reason the checkout path and the fetch path cannot
   * disagree: both read the file the repository generates about itself. In
   * abap2UI5/samples the two sources really differ - SAMPLES.md also lists
   * the src/00 system area and the helpers, catalogue.json scopes itself to
   * the portable src/01 set - so this ordering IS the published figure. */
  const dir = scratch();
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }));
  fs.mkdirSync(path.join(dir, '.samples'));
  fs.writeFileSync(path.join(dir, '.samples', 'SAMPLES.md'), ROWS); // 5 rows
  fs.writeFileSync(path.join(dir, '.samples', 'catalogue.json'), JSON_SAMPLES); // 2 entries

  assert.deepEqual(
    await countCatalogue('samples', dir, { fetchFn: forbidden }),
    { count: 2, source: 'checkout catalogue.json' },
  );
});

test('no checkout: the published catalogue.json is fetched and counted the same way', async (t) => {
  const dir = scratch();
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }));

  assert.deepEqual(
    await countCatalogue('samples-controls', dir, { fetchFn: served(JSON_CONTROLS) }),
    { count: 3, source: 'published catalogue.json' },
  );
  assert.deepEqual(
    await countCatalogue('samples-stack', dir, { fetchFn: served(JSON_STACK) }),
    { count: 1, source: 'published catalogue.json' },
  );
});

test('an unreachable published catalogue costs the figure, never the build', async (t) => {
  const dir = scratch();
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }));

  // the file is not committed yet, or the repository moved: 404
  assert.equal(await countCatalogue('samples', dir, { fetchFn: missing }), null);
  // no network at all - the shape every deploy had before the fallback
  assert.equal(await countCatalogue('samples', dir, { fetchFn: offline }), null);
  // a half-written or wrong file must count for nothing, not throw
  assert.equal(await countCatalogue('samples', dir, { fetchFn: served('not json {') }), null);
  assert.equal(await countCatalogue('samples', dir, { fetchFn: served('{"counts":{"samples":97}}') }), null);
});
