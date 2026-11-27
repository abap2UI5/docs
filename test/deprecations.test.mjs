/*
 * The reader behind docs/public/api/deprecations.json.
 *
 * The file is generated from a table written for people, so what has to be
 * pinned is less what the reader takes than what it REFUSES: a row with prose
 * in its first cell names things that were not removed (`the UI5 options of
 * message_box_display( )`), and a reader one rule wider would publish them
 * as removed - into a linter that reports them as errors.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { PAGE, SOURCE, parse, render, names, classify, status, plain } from '../scripts/lib/deprecations.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const page = readFileSync(join(ROOT, PAGE), 'utf8');

const table = (...rows) => ['## Version status', '', '| What you have | What to write | Status |', '|---|---|---|', ...rows, ''].join('\n');

test('a first cell is data only when it is code spans and nothing else', () => {
  assert.deepEqual(names('`a( )`, `b( )`').spans.map((s) => s.code), ['a( )', 'b( )']);
  assert.equal(names('`a( )` / `b( )`').separator, '/');
  assert.equal(names('`Z2UI5_T_02` (the DDIC structure)').spans[0].remark, 'the DDIC structure');
  for (const prose of [
    'the UI5 options of `message_toast_display( )` / `message_box_display( )`',
    'raw JavaScript in `follow_up_action( )`',
    '`view_model_update( )` and its four variants',
    '`z2ui5.Util`, `z2ui5.Formatter`, module `z2ui5/Util`',
    'built-in popups',
    '`a( )`,',
  ]) assert.equal(names(prose).prose, true, prose);
});

test('what a code span names', () => {
  assert.deepEqual(classify('z2ui5_cl_xml_view'), [{ name: 'z2ui5_cl_xml_view', kind: 'class' }]);
  assert.deepEqual(classify('z2ui5_cl_pop_*'), [{ name: 'z2ui5_cl_pop_*', kind: 'class', pattern: true }]);
  assert.deepEqual(classify('z2ui5_cx_util_error'), [{ name: 'z2ui5_cx_util_error', kind: 'class' }]);
  assert.deepEqual(classify('z2ui5_if_types=>…'), [{ name: 'z2ui5_if_types', kind: 'interface' }]);
  assert.deepEqual(classify('z2ui5_if_app~check_sticky'), [{ name: 'z2ui5_if_app~check_sticky', kind: 'attribute' }]);
  assert.deepEqual(classify('cs_event-hash_set'), [{ name: 'cs_event-hash_set', kind: 'constant' }]);
  assert.deepEqual(classify('Z2UI5_T_91'), [{ name: 'z2ui5_t_91', kind: 'dictionary-object' }]);
  assert.deepEqual(classify('_bind_edit( )'), [{ name: '_bind_edit', kind: 'method' }]);
  assert.deepEqual(classify('_bind( custom_mapper = … custom_filter = … )'), [
    { name: 'custom_mapper', kind: 'parameter', of: '_bind' },
    { name: 'custom_filter', kind: 'parameter', of: '_bind' },
  ]);
  assert.deepEqual(classify('_event( s_ctrl-check_allow_multi_req )'), [{ name: 's_ctrl-check_allow_multi_req', kind: 'field', of: '_event' }]);
  assert.deepEqual(classify('client->get( )-viewname'), [{ name: 'viewname', kind: 'field', of: 'get' }]);
  for (const unknown of ['check_initialized', 'Formatter.round2DP', 'window.z2ui5', 'z2ui5.cc.Timer', '_bind( val )']) {
    assert.equal(classify(unknown), null, unknown);
  }
});

test('the status cell', () => {
  assert.deepEqual(status('1.143.0'), { status: 'deprecated', since: '1.143.0' });
  assert.deepEqual(status('**removed**, 1.151.0'), { status: 'removed', since: '1.151.0' });
  assert.deepEqual(status('deprecated 1.142.0, **removed**, 1.151.0'), { status: 'removed', since: '1.151.0', deprecated: '1.142.0' });
  assert.deepEqual(status('**moved out**, 1.151.0'), { status: 'moved', since: '1.151.0' });
  assert.equal(status('gone'), null);
  assert.equal(status('removed, 1.151.0'), null);
});

test('a cell as plain text keeps where a link leads', () => {
  assert.equal(plain('`a( )` — see [Hash](/cookbook/hash#x)'), 'a( ) — see Hash (https://abap2ui5.github.io/docs/cookbook/hash#x)');
  assert.equal(plain('see [Utility classes](#utility-classes)'), `see Utility classes (${SOURCE}#utility-classes)`);
  assert.equal(plain('[W](../cookbook/model/w)'), 'W (https://abap2ui5.github.io/docs/cookbook/model/w)');
  assert.equal(plain('with *Retry* and **bold**'), 'with Retry and bold');
  assert.equal(plain('`z2ui5_cl_popup_*` from here'), 'z2ui5_cl_popup_* from here');
});

test('a row of several names is several entries, paired only when written as pairs', () => {
  const { entries, problems } = parse(table(
    '| `set_push_state( )`, `cs_event-set_push_state` | `hash_set( )`, `cs_event-hash_set` | **removed**, 1.144.1 |',
    '| `z2ui5_cl_util_api*`, `z2ui5_cl_pop_bal` | `z2ui5_cl_util` / `z2ui5_cl_util_ext` | **removed**, 1.142.0 |',
  ));
  assert.deepEqual(problems, []);
  assert.deepEqual(entries.map((e) => [e.name, e.replacement]), [
    ['set_push_state', 'hash_set( )'],
    ['cs_event-set_push_state', 'cs_event-hash_set'],
    ['z2ui5_cl_util_api*', 'z2ui5_cl_util / z2ui5_cl_util_ext'],
    ['z2ui5_cl_pop_bal', 'z2ui5_cl_util / z2ui5_cl_util_ext'],
  ]);
});

test('a name deprecated on one row and moved on another is one entry', () => {
  const { entries, problems } = parse(table(
    '| `z2ui5_cl_xml_view` | `z2ui5_cl_ui5_view_builder` | 1.143.0 |',
    '| `z2ui5_cl_xml_view`, `z2ui5_cl_xml_view_cc` | install [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) | **moved out**, 1.151.0 |',
  ));
  assert.deepEqual(problems, []);
  assert.deepEqual(entries, [
    {
      name: 'z2ui5_cl_xml_view',
      kind: 'class',
      status: 'moved',
      replacement: 'z2ui5_cl_ui5_view_builder',
      movedTo: 'abap2UI5/abap2UI5-ext',
      since: '1.151.0',
      note: 'deprecated since 1.143.0; install abap2UI5-ext (https://github.com/abap2UI5/abap2UI5-ext)',
    },
    {
      name: 'z2ui5_cl_xml_view_cc',
      kind: 'class',
      status: 'moved',
      replacement: '',
      movedTo: 'abap2UI5/abap2UI5-ext',
      since: '1.151.0',
      note: 'install abap2UI5-ext (https://github.com/abap2UI5/abap2UI5-ext)',
    },
  ]);
});

test('what cannot be read is a problem, never a silent gap', () => {
  const bad = (row) => parse(table(row)).problems.length;
  assert.equal(bad('| `check_initialized` | `check_on_init( )` | **removed**, 1.143.0 |'), 1, 'an unknown shape');
  assert.equal(bad('| `a( )` | `b( )` | soon |'), 1, 'an unreadable status');
  assert.equal(bad('| `z2ui5_cl_a` | install it | **moved out**, 1.151.0 |'), 1, 'moved, to nowhere');
  assert.equal(parse(table('| `a( )` | x | **removed**, 1.1.0 |', '| `a( )` | y | **removed**, 1.2.0 |')).problems.length, 1, 'removed twice');
  assert.equal(parse(table('| `z2ui5_cl_a` | [r](https://github.com/o/r) | **moved out**, 1.1.0 |', '| `z2ui5_cl_a` | y | **removed**, 1.2.0 |')).problems.length, 1, 'moved AND removed');
  assert.equal(parse('# no table here\n').problems.length, 1, 'no table');
});

test('a prose row and a removal without a release stay on the page', () => {
  const { entries, skipped, problems } = parse(table(
    '| the UI5 options of `message_toast_display( )` / `message_box_display( )` | set them on the control | **removed**, 1.144.1 |',
    '| `a( )` | `b( )` | **removed** |',
  ));
  assert.deepEqual(problems, []);
  assert.deepEqual(entries, []);
  assert.equal(skipped.length, 2);
});

/* ---- the page itself ------------------------------------------------------ */

const real = parse(page);
const find = (name) => real.entries.find((e) => e.name === name);

test('the page reads without a problem, and not as nothing', () => {
  assert.deepEqual(real.problems, []);
  assert.ok(real.entries.length >= 40, `${real.entries.length} entries`);
});

test('the three names every old example uses', () => {
  assert.equal(find('z2ui5_cl_xml_view').status, 'moved');
  assert.equal(find('z2ui5_cl_xml_view').movedTo, 'abap2UI5/abap2UI5-ext');
  assert.equal(find('z2ui5_cl_xml_view').replacement, 'z2ui5_cl_ui5_view_builder');
  assert.equal(find('_bind_edit').status, 'removed');
  assert.equal(find('_bind_edit').replacement, '_bind( )');
  assert.equal(find('z2ui5_cl_pop_*').pattern, true);
  assert.match(find('z2ui5_cl_pop_*').replacement, /^z2ui5_cl_popup_\* from the popups add-on/);
});

test('no method that still exists is listed because a prose row named it', () => {
  for (const alive of ['message_box_display', 'message_toast_display', 'follow_up_action', 'cs_event-control_by_id']) {
    assert.equal(find(alive), undefined, alive);
  }
});

test('every entry is one name, lower case, with a release', () => {
  const keys = new Set();
  for (const e of real.entries) {
    assert.equal(e.name, e.name.toLowerCase(), e.name);
    assert.match(e.since, /^\d+\.\d+\.\d+$/, e.name);
    assert.ok(['deprecated', 'removed', 'moved'].includes(e.status), e.name);
    assert.equal(e.status === 'moved', 'movedTo' in e, e.name);
    assert.equal(e.name.includes('*'), e.pattern === true, e.name);
    const key = [e.kind, e.of ?? '', e.name].join(' ');
    assert.ok(!keys.has(key), `${key} twice`);
    keys.add(key);
  }
});

test('the committed file is what the page generates', () => {
  assert.equal(readFileSync(join(ROOT, 'docs/public/api/deprecations.json'), 'utf8'), render(real.entries));
});
