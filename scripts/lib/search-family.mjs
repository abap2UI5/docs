/*
 * search-family — the rest of the project in the one search index.
 *
 * search-index.mjs builds the two areas this repository owns or has always
 * carried: the pages of the manual and the three sample catalogues. That
 * answered "where is it explained" and "where is an example of it". It did
 * not answer the other questions a reader brings to a box on the home page:
 * WHERE do I go (a repository, a tool, a site of the family), WHAT does the
 * rest of the family say about it (the essays, the page for decision makers,
 * the addons), and WHERE in the code is it (a client method, a class of the
 * framework, down to the file and the line).
 *
 * So, beside the two:
 *
 *   tools     the places of the family a reader goes to rather than reads -
 *             the playground, the layout editor, the AI builder - with the
 *             words people use for them. Data, here, below.
 *   repos     every repository of github.com/abap2UI5 and abap2UI5-addons,
 *             from the GitHub API: name, description, topics. A hit opens the
 *             repository; its README and issues are actions beside it.
 *   insights  the essays, why the page for decision makers, addons the
 *   why       addons and the developer tools - each from the search.json its
 *   addons    site publishes (one shape on every site, see SITES below).
 *   api       the client API (docs/public/api/client-api.json, committed and
 *             held fresh by check:api-reference), each method leading to its
 *             section of resources/api.md and to its line in
 *             z2ui5_if_client; and every class and interface of the
 *             framework outside the frozen package, leading to its file.
 *
 * EVERY SOURCE MAY FAIL. The GitHub API, a sister site, raw.githubusercontent:
 * an unreachable one costs its rows and is reported, never the build - the
 * rule the sample catalogues already follow. An index with fewer areas is a
 * worse search; a failed deploy of the manual over a sister site's outage is
 * a worse documentation.
 *
 * Issues and discussions are deliberately NOT indexed: a nightly copy of them
 * would be a day old when it mattered most. The box hands the query to
 * GitHub's own search for those instead (`elsewhere( )` in search-engine.js).
 */
import fs from 'node:fs';
import path from 'node:path';
import { DOCS, headings } from './pages.mjs';
import { fetchInterface } from './client-interface.mjs';

const GITHUB = 'https://github.com';
const API = 'https://api.github.com';
export const ORGS = ['abap2UI5', 'abap2UI5-addons'];

/** The places a reader GOES to. `terms` are the words people use for them,
 *  which is the whole reason these are entries and not tiles on a page. */
export const DESTINATIONS = [
  { title: 'Playground', text: 'Write ABAP in the browser and run the app beside it - no SAP system needed.', url: 'https://abap2ui5.github.io/playground/', terms: 'try online browser editor run abap without system sandbox' },
  { title: 'Layout Editor', text: 'Design a UI5 view by drag and drop, then export it as an ABAP class.', url: 'https://abap2ui5.github.io/layout-editor/', terms: 'drag drop designer wysiwyg view editor layout builder' },
  { title: 'AI App Builder', text: 'Describe the app you need; the AI writes it and you download it for abapGit.', url: 'https://abap2ui5.github.io/playground/ai/', terms: 'ai llm generate app chatgpt claude prompt builder' },
  { title: 'AI Agent', text: 'Let the AI operate a running app: describe the task in the chat and watch it work.', url: 'https://abap2ui5.github.io/playground/pilot/', terms: 'ai agent pilot automate operate chat' },
  { title: 'Sample catalogue', text: 'Every sample app, by control and UI5 release, each runnable in the playground.', url: 'https://abap2ui5.github.io/samples-site/', terms: 'samples examples demos catalogue catalog controls' },
  { title: 'Documentation', text: 'The manual: how it works, how to install it, how to build apps.', url: 'https://abap2ui5.github.io/docs/', terms: 'docs manual guide handbook' },
  { title: 'Insights', text: 'Short essays on the idea: how it works, where it belongs, running it.', url: 'https://abap2ui5.github.io/insights/', terms: 'essays articles blog background architecture' },
  { title: 'Why abap2UI5', text: 'For decision makers: what it builds, who runs it, what it costs.', url: 'https://abap2ui5.github.io/why/', terms: 'managers business case cost decision evaluate' },
  { title: 'Addons & Tools', text: 'Addons for your system, tools for your editor and pipeline, each with a page.', url: 'https://abap2ui5-addons.github.io/', terms: 'addons extensions plugins tools' },
  { title: 'Home', text: 'The entry page of abap2UI5.', url: 'https://abap2ui5.github.io/', terms: 'start home entry' },
  { title: 'Slack', text: 'The community channel #abap2ui5 on the abapGit Slack.', url: 'https://abapgit.slack.com/join/shared_invite/zt-46tqufaht-QlrxTzlDqlx85CWbeUnOqg#/shared-invite/email', terms: 'chat community help ask question' },
  { title: 'Releases', text: 'The releases of the framework, with their notes.', url: `${GITHUB}/abap2UI5/abap2UI5/releases`, terms: 'release changelog version download latest' },
  { title: 'Report an issue', text: 'Open an issue for the framework on GitHub.', url: `${GITHUB}/abap2UI5/abap2UI5/issues/new/choose`, terms: 'bug report issue problem error new' },
  { title: 'Discussions', text: 'Questions, ideas and show-and-tell, on GitHub.', url: `${GITHUB}/abap2UI5/abap2UI5/discussions`, terms: 'question idea forum ask discuss' },
];

export function destinationEntries() {
  return DESTINATIONS.map((d) => ({ area: 'tools', group: 'Go to', ...d }));
}

/** The sites that publish a search.json, and the area each becomes. */
export const SITES = [
  { area: 'insights', label: 'Insights', url: 'https://abap2ui5.github.io/insights/search.json' },
  { area: 'why', label: 'Why abap2UI5', url: 'https://abap2ui5.github.io/why/search.json' },
  { area: 'addons', label: 'Addons & Tools', url: 'https://abap2ui5-addons.github.io/search.json' },
];

const timeout = () => AbortSignal.timeout(20000);
const ghHeaders = (token = process.env.GITHUB_TOKEN) => ({
  Accept: 'application/vnd.github+json',
  'User-Agent': 'abap2UI5-docs-search-index',
  ...(token ? { Authorization: `Bearer ${token}` } : {}),
});

/** Is this an http(s) URL of the family or GitHub - the only places a feed
 *  may send a reader. A feed is another repository's file; a hit that leads
 *  off the project is not something it gets to publish into this box. */
const OWN = /^https:\/\/(abap2ui5\.github\.io|abap2ui5-addons\.github\.io|github\.com\/abap2UI5(-addons)?)\//i;

/** One site's search.json as entries of `area`. Only the fields the matcher
 *  reads, each checked for its type: a feed that changed shape costs its
 *  odd entries, not the index. */
export function siteEntries(feed, { area, label }) {
  const out = [];
  for (const e of Array.isArray(feed?.entries) ? feed.entries : []) {
    if (typeof e?.title !== 'string' || !e.title.trim() || typeof e.url !== 'string' || !OWN.test(e.url)) continue;
    const entry = { area, group: label, title: e.title.trim(), text: typeof e.text === 'string' ? e.text : '', url: e.url };
    if (Array.isArray(e.headings)) {
      const hs = e.headings.filter((h) => Array.isArray(h) && typeof h[0] === 'string' && typeof h[1] === 'string');
      if (hs.length) entry.headings = hs;
    }
    if (typeof e.terms === 'string' && e.terms) entry.terms = e.terms;
    if (typeof e.code === 'string' && e.code) entry.code = e.code;
    const links = linksOf(e.links);
    if (links.length) entry.links = links;
    out.push(entry);
  }
  return out;
}

function linksOf(links) {
  return (Array.isArray(links) ? links : [])
    .filter((l) => typeof l?.label === 'string' && typeof l.url === 'string' && OWN.test(l.url))
    .map((l) => ({ label: l.label, url: l.url }));
}

/** Every repository of the organisations, as entries. Archived ones are left
 *  out: a search that leads to a repository nobody maintains is a wrong
 *  answer that looks right. Forks of the organisation's own are kept - they
 *  are how an addon carries a library it adapted. */
export function repoEntries(repos) {
  return repos
    .filter((r) => r && !r.archived && !r.private && typeof r.full_name === 'string')
    .map((r) => {
      const [org, name] = r.full_name.split('/');
      return {
        area: 'repos',
        group: 'Repositories',
        title: name,
        text: r.description || '',
        /* the full name is what somebody pastes - abap2UI5-addons/popups */
        code: r.full_name.toLowerCase(),
        terms: [org, ...(r.topics || []), r.language || '', r.homepage ? 'site' : ''].join(' ').toLowerCase().trim(),
        url: r.html_url || `${GITHUB}/${r.full_name}`,
        links: [
          { label: 'README', url: `${GITHUB}/${r.full_name}#readme` },
          { label: 'Issues', url: `${GITHUB}/${r.full_name}/issues` },
          ...(r.homepage && OWN.test(r.homepage) ? [{ label: 'Site', url: r.homepage }] : []),
        ],
      };
    });
}

async function fetchRepos(org, fetchFn) {
  const all = [];
  for (let page = 1; page <= 5; page++) {
    const res = await fetchFn(`${API}/orgs/${org}/repos?type=public&per_page=100&page=${page}`, { headers: ghHeaders(), signal: timeout() });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const rows = await res.json();
    all.push(...rows);
    if (rows.length < 100) break;
  }
  return all;
}

/* ── THE CODE ─────────────────────────────────────────────────────────────── */

const FRAMEWORK = 'abap2UI5/abap2UI5';
const CLIENT_API = path.join(DOCS, 'public', 'api', 'client-api.json');
const API_PAGE = 'https://abap2ui5.github.io/docs/resources/api.html';

/** The first sentence of a doc block - what a result row has room for. */
const first = (lines) => {
  const s = (Array.isArray(lines) ? lines.join(' ') : String(lines || '')).replace(/\s+/g, ' ').trim();
  const m = s.match(/^.{20,}?[.!?](\s|$)/);
  return (m ? m[0] : s).trim();
};

/** The line a member is declared on in the interface source, 1-based, or 0. */
export function lineOf(source, name, kind = 'METHODS') {
  if (!source) return 0;
  const re = new RegExp(`^\\s*(${kind}|CLASS-${kind})\\s+${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'im');
  const lines = source.split('\n');
  const i = lines.findIndex((l) => re.test(l));
  return i < 0 ? 0 : i + 1;
}

/** The client API: one entry per method and per constant group, from the
 *  committed client-api.json, with the anchors resources/api.md really has. */
export function clientApiEntries(api, apiPageBody, source) {
  const anchors = new Map(headings(apiPageBody).map((h) => [h.text, h.anchor]));
  const file = api.source || `${GITHUB}/${FRAMEWORK}/blob/main/src/02/z2ui5_if_client.intf.abap`;
  const at = (name, kind) => {
    const n = lineOf(source, name, kind);
    return n ? `${file}#L${n}` : file;
  };
  const out = [];
  for (const m of api.methods || []) {
    const anchor = anchors.get(m.name);
    out.push({
      area: 'api',
      group: 'API',
      title: `client->${m.name}( )`,
      text: first(m.doc),
      code: m.name.toLowerCase(),
      terms: [m.group, ...(m.parameters || []).map((p) => p.name)].join(' ').toLowerCase(),
      url: anchor ? `${API_PAGE}#${anchor}` : API_PAGE,
      links: [{ label: 'Source', url: at(m.name, 'METHODS') }],
    });
  }
  for (const c of api.constants || []) {
    const anchor = anchors.get(c.name);
    out.push({
      area: 'api',
      group: 'API',
      title: `client->${c.name}`,
      text: first(c.doc) || `The ${c.name} constants of z2ui5_if_client.`,
      code: c.name.toLowerCase(),
      terms: (c.members || []).map((x) => x.name).join(' ').toLowerCase(),
      url: anchor ? `${API_PAGE}#${anchor}` : API_PAGE,
      links: [{ label: 'Source', url: at(c.name, 'CONSTANTS') }],
    });
  }
  return out;
}

/** Every class and interface of the framework outside src/99 - the frozen
 *  package, whose names this site does not teach (check:api-names) and a
 *  search box should not offer. From the tree of main: one request. */
export function sourceEntries(tree) {
  const out = [];
  for (const t of tree || []) {
    const m = /^src\/(?!99\/)(?:.*\/)?(z2ui5_\w+)\.(clas|intf)\.abap$/i.exec(t?.path || '');
    if (!m || t.type !== 'blob') continue;
    const [, name, kind] = m;
    out.push({
      area: 'api',
      group: 'Framework source',
      title: name.toLowerCase(),
      text: `${kind === 'intf' ? 'Interface' : 'Class'} of the framework - ${t.path.split('/').slice(1, -1).join('/') || 'src'}`,
      code: name.toLowerCase(),
      url: `${GITHUB}/${FRAMEWORK}/blob/main/${t.path}`,
    });
  }
  return out;
}

/**
 * The family's areas, each fetched on its own, each allowed to fail.
 * `{ entries, areas }` - `areas` in the same shape search-index.mjs reports.
 */
export async function familyEntries({ fetchFn = globalThis.fetch, log = () => {}, readFile = fs.readFileSync, interfaceFn = fetchInterface } = {}) {
  const entries = [];
  const areas = [];
  const add = (area, label, rows, from) => {
    entries.push(...rows);
    areas.push({ area, label, count: rows.length });
    log(`  ${label}: ${rows.length} entr${rows.length === 1 ? 'y' : 'ies'} (${from})`);
  };
  const tryArea = async (label, fn) => {
    try {
      await fn();
    } catch (err) {
      log(`  ${label}: not reachable (${err.message}) — not in this index`);
    }
  };

  add('tools', 'Go to', destinationEntries(), 'scripts/lib/search-family.mjs');

  await tryArea('Repositories', async () => {
    const repos = [];
    for (const org of ORGS) repos.push(...await fetchRepos(org, fetchFn));
    add('repos', 'Repositories', repoEntries(repos), `api.github.com, ${ORGS.join(' + ')}`);
  });

  for (const site of SITES) {
    await tryArea(site.label, async () => {
      const res = await fetchFn(site.url, { signal: timeout() });
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const rows = siteEntries(await res.json(), site);
      if (!rows.length) throw new Error('no entry in the feed');
      add(site.area, site.label, rows, site.url);
    });
  }

  await tryArea('API', async () => {
    const api = JSON.parse(readFile(CLIENT_API, 'utf8'));
    const page = readFile(path.join(DOCS, 'resources', 'api.md'), 'utf8');
    /* the line numbers are a nicety: without the source the links go to the
     * file, which is still the right file */
    let source = '';
    try { source = await interfaceFn(api.ref || 'main'); } catch (err) { log(`  API: interface source not reachable (${err.message}) — links without a line`); }
    add('api', 'API', clientApiEntries(api, page, source), 'docs/public/api/client-api.json');
  });

  await tryArea('Framework source', async () => {
    const res = await fetchFn(`${API}/repos/${FRAMEWORK}/git/trees/main?recursive=1`, { headers: ghHeaders(), signal: timeout() });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const rows = sourceEntries((await res.json()).tree);
    if (!rows.length) throw new Error('no class in the tree');
    add('api', 'Framework source', rows, `api.github.com, ${FRAMEWORK}@main`);
  });

  return { entries, areas };
}
