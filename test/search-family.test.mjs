/*
 * The rest of the family in the search index (scripts/lib/search-family.mjs)
 * and the three things the matcher answers about a query beside its ranking
 * (parseScope, jumps, elsewhere and actions in scripts/lib/search-engine.js).
 *
 * The sources are other repositories and the GitHub API, so what is pinned
 * here is what this side does with what they send: a feed that leads off the
 * project is not published, an archived repository is not offered, the frozen
 * package is not taught, and a source that is down costs its rows and never
 * the build.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
  siteEntries, repoEntries, sourceEntries, clientApiEntries, lineOf, destinationEntries, familyEntries, SITES,
} from '../scripts/lib/search-family.mjs';
import { search, grouped, jumps, elsewhere, actions, parseScope } from '../scripts/lib/search-engine.js';

const INSIGHTS = SITES.find((s) => s.area === 'insights');

test('a feed of a sister site becomes entries of its area, and only what leads to the project', () => {
  const rows = siteEntries({
    site: 'Insights',
    entries: [
      { title: '#21 The Lock Is Gone by the Next Click', text: 'ENQUEUE inside one roundtrip.', headings: [['Why', 'why'], ['bad']], terms: 'enqueue lock', url: 'https://abap2ui5.github.io/insights/21-the-lock/' },
      { title: 'Elsewhere', text: 'a page of somebody else', url: 'https://example.com/' },
      { title: '', url: 'https://abap2ui5.github.io/insights/x/' },
      { title: 'No url' },
      { title: 'Addon', url: 'https://abap2ui5-addons.github.io/popups/', links: [{ label: 'Repository', url: 'https://github.com/abap2UI5-addons/popups' }, { label: 'Off', url: 'https://example.com/' }] },
    ],
  }, INSIGHTS);
  assert.deepEqual(rows.map((r) => r.title), ['#21 The Lock Is Gone by the Next Click', 'Addon']);
  assert.equal(rows[0].area, 'insights');
  assert.equal(rows[0].group, 'Insights');
  assert.deepEqual(rows[0].headings, [['Why', 'why']], 'a malformed heading is dropped');
  assert.deepEqual(rows[1].links, [{ label: 'Repository', url: 'https://github.com/abap2UI5-addons/popups' }], 'a link off the project is dropped');
  assert.deepEqual(siteEntries(null, INSIGHTS), [], 'a feed that is not one is nothing');
  const [section] = siteEntries({ entries: [{ title: 'Cost', url: 'https://abap2ui5.github.io/why/#what-it-costs', headings: [['Licence', 'what-it-costs']] }] }, INSIGHTS);
  assert.equal(section.headings, undefined, 'a url with an anchor carries no headings of its own');
});

test('a repository is offered with its README and issues, an archived one is not', () => {
  const rows = repoEntries([
    { full_name: 'abap2UI5/linter', description: 'A static code analysis tool', topics: ['abap'], html_url: 'https://github.com/abap2UI5/linter', homepage: 'https://abap2ui5.github.io/linter/' },
    { full_name: 'abap2UI5/old', archived: true },
    { full_name: 'abap2UI5-addons/popups', description: 'Value-Help, Search-Help', homepage: 'https://example.com/' },
  ]);
  assert.deepEqual(rows.map((r) => r.code), ['abap2ui5/linter', 'abap2ui5-addons/popups']);
  assert.deepEqual(rows[0].links.map((l) => l.label), ['README', 'Issues', 'Site']);
  assert.deepEqual(rows[1].links.map((l) => l.label), ['README', 'Issues'], 'a homepage off the project is no action');
  assert.equal(rows[1].url, 'https://github.com/abap2UI5-addons/popups');
});

test('the framework source leaves out the frozen package', () => {
  const rows = sourceEntries([
    { path: 'src/02/z2ui5_if_client.intf.abap', type: 'blob' },
    { path: 'src/01/04/z2ui5_cl_ui5_view_builder.clas.abap', type: 'blob' },
    { path: 'src/01/04/z2ui5_cl_ui5_view_builder.clas.xml', type: 'blob' },
    { path: 'src/99/z2ui5_cl_xml_view.clas.abap', type: 'blob' },
    { path: 'src/01', type: 'tree' },
  ]);
  assert.deepEqual(rows.map((r) => r.code), ['z2ui5_if_client', 'z2ui5_cl_ui5_view_builder']);
  assert.equal(rows[1].url, 'https://github.com/abap2UI5/abap2UI5/blob/main/src/01/04/z2ui5_cl_ui5_view_builder.clas.abap');
});

test('a client method leads to its section and to its line', () => {
  const api = {
    source: 'https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_client.intf.abap',
    methods: [{ name: 'view_display', group: 'Main view', doc: ['Display the MAIN view. A new main view is a new screen.'], parameters: [{ name: 'val' }] }],
    constants: [{ name: 'cs_event', doc: [], members: [{ name: 'popup_close' }] }],
  };
  const page = '# Client API\n\n## Main view\n\n### `view_display`\n\ntext\n\n## Constants\n\n### `cs_event`\n';
  const source = 'INTERFACE z2ui5_if_client.\n  CONSTANTS:\n    BEGIN OF cs_event,\n  METHODS view_display\n    IMPORTING val TYPE clike.\n';
  const [method, constant] = clientApiEntries(api, page, source);
  assert.equal(method.title, 'client->view_display( )');
  assert.equal(method.text, 'Display the MAIN view.');
  assert.equal(method.url, 'https://abap2ui5.github.io/docs/resources/api.html#view-display');
  assert.equal(method.links[0].url, `${api.source}#L4`);
  assert.equal(constant.code, 'cs_event');
  assert.equal(lineOf(source, 'nothing_like_it'), 0);
  assert.equal(clientApiEntries(api, page, '')[0].links[0].url, api.source, 'no source, no line - still the file');
});

test('a source that is down costs its rows, never the build', async () => {
  const lines = [];
  const down = async () => ({ ok: false, status: 503, json: async () => ({}) });
  const { entries, areas } = await familyEntries({ fetchFn: down, interfaceFn: async () => { throw new Error('offline'); }, log: (l) => lines.push(l) });
  assert.ok(entries.some((e) => e.area === 'tools'), 'where to go needs no network');
  assert.ok(entries.some((e) => e.area === 'api' && e.group === 'API'), 'the client API is committed here');
  assert.ok(!areas.some((a) => a.area === 'repos'), 'no repositories without the API');
  assert.ok(lines.some((l) => /Repositories: not reachable/.test(l)), 'and the log says so');
});

/* ── the matcher ─────────────────────────────────────────────────────────── */

const ENTRIES = [
  ...destinationEntries(),
  ...repoEntries([{ full_name: 'abap2UI5/linter', description: 'A static code analysis tool', topics: [] }, { full_name: 'abap2UI5-addons/popups', description: 'Value-Help, Search-Help, Transport', topics: [] }]),
  { area: 'samples', group: 'Controls', title: 'Carousel', text: 'A carousel.', code: 'z2ui5_cl_smpc_app_398', url: 'https://abap2ui5.github.io/playground/samples/z2ui5_cl_smpc_app_398/', links: [{ label: 'Source', url: 'https://github.com/abap2UI5/samples-controls/blob/main/src/x.clas.abap' }] },
  { area: 'api', group: 'API', title: 'client->view_display( )', text: 'Display the MAIN view.', code: 'view_display', url: 'https://abap2ui5.github.io/docs/resources/api.html#view-display' },
  { area: 'docs', group: 'Cookbook', title: 'Popup', text: 'One popup and what closes it.', url: 'https://abap2ui5.github.io/docs/cookbook/popup.html' },
];

test('a scope narrows the search to its area, and alone it lists the area', () => {
  assert.deepEqual(parseScope('repo: popup'), { area: 'repos', query: 'popup' });
  assert.deepEqual(parseScope('z2ui5: x'), { area: null, query: 'z2ui5: x' }, 'a word that is no scope is a search');
  assert.deepEqual(search(ENTRIES, 'repo: popup').map((h) => h.entry.area), ['repos']);
  assert.ok(search(ENTRIES, 'popup').some((h) => h.entry.area === 'docs'), 'unscoped, the page is there too');
  assert.deepEqual(search(ENTRIES, 'repos:').map((h) => h.entry.title), ['linter', 'popups']);
  assert.deepEqual(grouped(search(ENTRIES, 'repo:')).map((g) => g.label), ['Repositories']);
});

test('a query that names something jumps to it', () => {
  const at = (q) => jumps(ENTRIES, q).map((j) => (j.entry ? j.entry.url : j.url));
  assert.deepEqual(at('#1234'), ['https://github.com/abap2UI5/abap2UI5/issues/1234']);
  assert.deepEqual(at('issue 42'), ['https://github.com/abap2UI5/abap2UI5/issues/42']);
  assert.deepEqual(at('Z2UI5_CL_SMPC_APP_398'), ['https://abap2ui5.github.io/playground/samples/z2ui5_cl_smpc_app_398/']);
  assert.deepEqual(at('view_display'), ['https://abap2ui5.github.io/docs/resources/api.html#view-display']);
  assert.deepEqual(at('linter'), ['https://github.com/abap2UI5/linter']);
  assert.deepEqual(at('abap2UI5-addons/popups'), ['https://github.com/abap2UI5-addons/popups']);
  assert.deepEqual(at('layout editor'), ['https://abap2ui5.github.io/layout-editor/']);
  assert.match(at('z2ui5_cl_nowhere')[0], /^https:\/\/github\.com\/search\?q=.*z2ui5_cl_nowhere&type=code$/, 'a class in no entry: the code search');
  assert.deepEqual(at('how do i close a popup'), [], 'a question is answered by the list');
  assert.deepEqual(at('#12a'), []);
});

test('the way out: the query handed to GitHub, for what no index carries', () => {
  const out = elsewhere('repo: popup close');
  assert.deepEqual(out.map((o) => o.label), ['Code', 'Issues', 'Discussions']);
  for (const o of out) {
    assert.match(o.url, /^https:\/\/github\.com\/search\?q=/);
    assert.match(decodeURIComponent(o.url), /org:abap2UI5 .*popup close|org:abap2UI5-addons\) popup close/, 'the query without its scope');
  }
  assert.match(out[0].url, /&type=code$/);
  assert.match(out[1].url, /&type=issues$/);
  assert.match(out[2].url, /&type=discussions$/);
  assert.deepEqual(elsewhere('  '), []);
});

test('the actions of a hit: what it carries, and a repository searched for the query', () => {
  const carousel = ENTRIES.find((e) => e.title === 'Carousel');
  assert.deepEqual(actions(carousel, 'carousel').map((a) => a.label), ['Source']);
  const linter = ENTRIES.find((e) => e.code === 'abap2ui5/linter');
  assert.deepEqual(actions(linter, 'rule').map((a) => a.label), ['README', 'Issues', 'Search code']);
  assert.match(actions(linter, 'rule').at(-1).url, /repo%3Aabap2ui5%2Flinter%20rule&type=code$/);
  assert.deepEqual(actions(linter, 'linter').map((a) => a.label), ['README', 'Issues'], 'its own name is no code search');
  assert.deepEqual(actions(undefined), []);
});
