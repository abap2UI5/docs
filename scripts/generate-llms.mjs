#!/usr/bin/env node
/*
 * generate-llms — this documentation, in the form a machine can actually read.
 *
 * The case this exists for is the one nothing else covers. An agent with the
 * MCP server (abap2UI5/ai-mcp) has tools. An agent working inside a checkout
 * has AGENTS.md and the skills. An agent that is simply ASKED "build me an
 * abap2UI5 app", with nothing but a web search, has neither - it lands on 70
 * rendered HTML pages wrapped in site navigation, or it falls back on training
 * data, where abap2UI5 still looks like `z2ui5_cl_xml_view`. That last one is
 * the failure we keep paying for.
 *
 * `llms.txt` is the convention for exactly that reader, and it is a WEB
 * convention: the file is fetched from the site, not from a repository. One
 * has existed in the framework repository for a while, which means only
 * somebody already inside the repository could find it - and they had no need
 * for it. So it is published here, from the source it describes.
 *
 * Three artefacts, all served from the site root:
 *
 *   llms.txt         the map: every page with its title and one line of what
 *                    it covers, plus the repositories around it. One fetch to
 *                    know what exists.
 *   llms-full.txt    the whole documentation as one markdown document. One
 *                    fetch instead of seventy, with no HTML in the way.
 *   <page>.md        each page as its own raw markdown, next to the .html, so
 *                    a specific chapter can be read without the full text.
 *
 * Generated at BUILD time and gitignored rather than committed: unlike the
 * sample links, nothing here is a claim about another repository that could go
 * stale between builds - it is a projection of the pages next to it, so the
 * only correct copy is the one made from the pages as they are. `npm run
 * docs:build` runs it, which means the deploy workflow does too.
 *
 * The page list is the SIDEBAR, not a directory walk: an agent should meet the
 * documentation in the order a reader does, and a page that is in the tree but
 * in no sidebar is a page nobody navigates to. Those are reported, and still
 * published, because being hard to find is not a reason to hide it from the
 * one reader that would have found it.
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import config from '../docs/.vitepress/config.mjs';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const DOCS = path.join(ROOT, 'docs');
const PUBLIC = path.join(DOCS, 'public');
const SITE = 'https://abap2ui5.github.io/docs';

/* ------------------------------------------------------------- the pages */

/** Every sidebar entry with a local link, in sidebar order, first one wins:
 *  several groups deliberately point their own heading at their first page. */
function sidebarPages() {
  const seen = new Set();
  const out = [];
  const walk = (nodes, section) => {
    for (const node of nodes || []) {
      // a top-level group names the section everything under it belongs to
      const here = section ?? node.text;
      if (node.link?.startsWith('/') && !seen.has(node.link)) {
        seen.add(node.link);
        out.push({ section: here, text: node.text, link: node.link });
      }
      walk(node.items, here);
    }
  };
  walk(config.themeConfig.sidebar, null);
  return out;
}

function markdownFiles(dir, out = []) {
  for (const name of fs.readdirSync(dir)) {
    if (name === '.vitepress' || name === 'public' || name === 'node_modules') continue;
    const full = path.join(dir, name);
    if (fs.statSync(full).isDirectory()) markdownFiles(full, out);
    else if (full.endsWith('.md')) out.push(full);
  }
  return out;
}

const linkOf = (file) => `/${path.relative(DOCS, file).replace(/\\/g, '/').replace(/\.md$/, '')}`;
const fileOf = (link) => path.join(DOCS, `${link.slice(1)}.md`);

/* ------------------------------------------------------------ the content */

const stripFrontmatter = (text) => text.replace(/^---\r?\n[\s\S]*?\r?\n---\r?\n/, '');

/** The first real sentence of a page, for the one-line note in the index.
 *
 *  The WHOLE first paragraph is collected before cutting: the sources are hard
 *  wrapped, so stopping at the newline would end half the notes mid-sentence.
 *
 *  Link syntax and `*` emphasis are flattened, and backticks with them - but
 *  NOT the underscore. Almost every identifier this documentation is about
 *  carries one (`_bind`, `check_on_init`, `s_device`), and an index that
 *  advertises `client->bind( )` is worse than no index: it is a plausible,
 *  citable, wrong API name aimed at the one reader least able to notice. */
function summarise(body) {
  const lines = stripFrontmatter(body).split('\n');
  const para = [];
  let inFence = false;
  for (const line of lines) {
    if (line.startsWith('```')) { inFence = !inFence; continue; }
    if (inFence) continue;
    const t = line.trim();
    if (!para.length) {
      if (!t || t.startsWith('#') || t.startsWith(':::') || t.startsWith('|') || t.startsWith('<')) continue;
      para.push(t);
      continue;
    }
    if (!t) break; // the paragraph ended
    para.push(t);
  }
  const plain = para.join(' ')
    .replace(/\[([^\]]+)\]\([^)]*\)/g, '$1')
    .replace(/[*`]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
  if (!plain) return '';
  // cut at the end of the first sentence, but not so early that the note says
  // nothing; a period inside `sap.m.Table` or `1.71` is not a sentence end
  const stop = plain.search(/\.(?=\s|$)|:\s—|\s—\s/);
  const first = stop > 40 ? plain.slice(0, stop + 1) : plain;
  return first.length > 220 ? `${first.slice(0, 217)}...` : first;
}

const title = (body, fallback) =>
  (stripFrontmatter(body).match(/^#\s+(.+?)\s*$/m) || [, fallback])[1].trim();

/* -------------------------------------------------------------------- run */

const pages = sidebarPages();
const linked = new Set(pages.map((p) => p.link));

// pages that exist but sit in no sidebar - reported, and appended so the agent
// still gets them
const orphans = markdownFiles(DOCS)
  .map(linkOf)
  .filter((l) => !linked.has(l) && l !== '/index')
  .sort();

const read = new Map();
for (const link of [...linked, ...orphans]) {
  const file = fileOf(link);
  if (!fs.existsSync(file)) {
    throw new Error(`the sidebar links ${link}, which is not a page (${path.relative(ROOT, file)})`);
  }
  read.set(link, fs.readFileSync(file, 'utf8'));
}

/* --- llms.txt: the map ---------------------------------------------------- */

const bySection = new Map();
for (const p of pages) {
  if (!bySection.has(p.section)) bySection.set(p.section, []);
  bySection.get(p.section).push(p);
}

const entry = (p) => {
  const body = read.get(p.link);
  const note = summarise(body);
  return `- [${title(body, p.text)}](${SITE}${p.link}.md)${note ? `: ${note}` : ''}`;
};

const index = [
  '# abap2UI5',
  '',
  '> Build SAP UI5 applications purely in ABAP - no JavaScript, OData or RAP',
  '> required. One ABAP class per app: the framework renders the UI5 XML view,',
  '> binds ABAP data both ways and dispatches events over stateless HTTP',
  '> roundtrips. Runs from NetWeaver 7.02 to ABAP Cloud, on-premise and cloud.',
  '',
  'This is the documentation site, written for people and published here in',
  'markdown so a machine can read the same thing. Every link below is the raw',
  'source of a page; drop the `.md` for the rendered version.',
  '',
  '- [llms-full.txt](' + SITE + '/llms-full.txt): the whole documentation as one',
  '  document, if you would rather fetch it once than page by page',
  '',
  ...[...bySection].flatMap(([section, list]) => [
    `## ${section}`,
    '',
    ...list.map(entry),
    '',
  ]),
  ...(orphans.length
    ? ['## Other pages', '',
      ...orphans.map((link) => entry({ link, text: link.split('/').pop() })), '']
    : []),
  '## Beyond the documentation',
  '',
  '- [abap2UI5](https://github.com/abap2UI5/abap2UI5): the framework. `AGENTS.md`',
  '  is the briefing for changing it; `src/02/z2ui5_if_client.intf.abap` is the',
  '  complete API an app may call, with inline documentation',
  '- [samples](https://github.com/abap2UI5/samples): 152 complete apps, one class',
  '  each. [`SAMPLES.md`](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)',
  '  lists all of them with search terms - ask it "is there a sample for X?"',
  '  before writing one',
  '- [samples-controls](https://github.com/abap2UI5/samples-controls): ~400 ports',
  '  of the official UI5 demo kit, plus `CAPABILITIES.md` - what abap2UI5 can and',
  '  cannot express',
  '- [samples-stack](https://github.com/abap2UI5/samples-stack): abap2UI5 combined',
  '  with OData, RAP, WebSockets and the Fiori Launchpad',
  '- [app-template](https://github.com/abap2UI5/app-template): starter repository',
  '  with both gates and CI already wired up - begin an app here',
  '- [linter](https://github.com/abap2UI5/linter): checks an app class and the',
  '  view it builds without an SAP system (CLI, GitHub Action, VS Code)',
  '- [ai-mcp](https://github.com/abap2UI5/ai-mcp): MCP server - query the sample',
  '  catalogue, validate a view, deploy, build, boot the app headless and get a',
  '  screenshot back. No SAP system needed',
  '',
  '## What to know before writing ABAP',
  '',
  '- An app is ONE class implementing `z2ui5_if_app`. Everything enters `main`,',
  '  which dispatches on `client->check_on_init( )`, `client->check_on_event( )`',
  '  and `client->check_on_navigated( )`.',
  '- Build the view with `z2ui5_cl_ui5_view_builder` and its `ele`/`tag`/`a`/`end`',
  '  verbs. `z2ui5_cl_xml_view` is the FROZEN predecessor: it still works, it is',
  '  what most training data shows, and it is not what to write today.',
  '- Bind with `client->_bind( )`. It is bidirectional, and only the paths the',
  '  user edited travel back.',
  '- Every roundtrip is a fresh ABAP session. Nothing on the server survives',
  '  between two clicks unless it is in the app class, which is serialized.',
  '',
].join('\n');

/* --- llms-full.txt: everything ------------------------------------------- */

const chapter = (link, text) => {
  const body = stripFrontmatter(read.get(link)).trim();
  return `\n\n---\n\n<!-- ${SITE}${link} -->\n\n${body}`;
};

const full = [
  '# abap2UI5 — the complete documentation',
  '',
  `Every page of ${SITE}, in sidebar order, as one document.`,
  'Each chapter is preceded by an HTML comment with its canonical URL.',
  ...pages.map((p) => chapter(p.link, p.text)),
  ...orphans.map((link) => chapter(link, link)),
  '',
].join('\n');

/* --- per-page markdown next to the html ---------------------------------- */

let written = 0;
for (const [link, body] of read) {
  const out = path.join(PUBLIC, `${link.slice(1)}.md`);
  fs.mkdirSync(path.dirname(out), { recursive: true });
  fs.writeFileSync(out, `<!-- ${SITE}${link} -->\n\n${stripFrontmatter(body).trim()}\n`);
  written += 1;
}

fs.writeFileSync(path.join(PUBLIC, 'llms.txt'), `${index}\n`);
fs.writeFileSync(path.join(PUBLIC, 'llms-full.txt'), full);

const kb = (s) => `${Math.round(s / 1024)} kB`;
console.log(`llms.txt: ${pages.length} pages in ${bySection.size} sections (${kb(index.length)})`);
console.log(`llms-full.txt: ${kb(full.length)}`);
console.log(`${written} page(s) published as raw markdown under docs/public/`);
if (orphans.length) {
  console.log(`\n${orphans.length} page(s) are in no sidebar - published, but nothing navigates to them:`);
  for (const link of orphans) console.log(`  ${link}`);
}
