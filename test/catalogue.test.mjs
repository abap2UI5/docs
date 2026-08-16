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
import { parseCatalogue } from '../scripts/lib/catalogue.mjs';

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
