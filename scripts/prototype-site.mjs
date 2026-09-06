/*
 * THE WHOLE MANUAL, WITHOUT THE THEME - the prototype from
 * scripts/prototype-page.mjs, run over every page.
 *
 * This exists to answer one question with a build rather than an argument:
 * what does the manual look like, and what does it cost, if the pages are
 * generated the way the sample catalogue's are - a Node script that writes
 * HTML - instead of by a Vue application with a theme?
 *
 * It is a PROTOTYPE. It is not wired into any gate, nothing depends on it,
 * and it deliberately leaves things out (they are listed at the end of the
 * run, so the report is the program's rather than mine).
 *
 * The load-bearing finding, worth repeating here: VitePress's markdown
 * renderer works as a plain library. `createMarkdownRenderer` produces the
 * same HTML the site ships today - the ::: blocks, the header anchors, Shiki
 * in both themes, and internal links already rewritten to /docs/x.html - with
 * no Vue, no router and no theme. So this does not reimplement markdown; it
 * replaces the FRAME around it.
 *
 *   node scripts/prototype-site.mjs [out-dir]
 */
import fs from 'node:fs';
import path from 'node:path';
import { createMarkdownRenderer } from 'vitepress';
import config from '../docs/.vitepress/config.mjs';
import { trailFor } from '../docs/.vitepress/theme/crumbs.js';

const ROOT = process.cwd();
const DOCS = path.join(ROOT, 'docs');
const OUT = process.argv[2] || path.join(ROOT, '.prototype');
const BASE = '/docs/';
const started = Date.now();

const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

/* ---- which files are pages ------------------------------------------ */
const pages = [];
(function walk(dir) {
  for (const e of fs.readdirSync(path.join(DOCS, dir), { withFileTypes: true })) {
    const rel = dir ? `${dir}/${e.name}` : e.name;
    if (e.isDirectory()) {
      /* `.vitepress` is the build, `public` is the raw-markdown mirror that is
         copied verbatim - neither is a page. Excluding public here is the same
         rule `srcExclude` states for the VitePress build. */
      if (e.name === '.vitepress' || e.name === 'public') continue;
      walk(rel);
    } else if (e.name.endsWith('.md')) pages.push(rel);
  }
})('');
pages.sort();

/* ---- the frontmatter keys this needs, read line by line --------------
 * No YAML parser is installed and there is no network to fetch one; the two
 * keys read here are scalars on their own line, which a reader of four lines
 * can do. The home page's nested hero is NOT read - see the note at the end. */
const front = (src) => {
  const m = src.match(/^---\n([\s\S]*?)\n---\n/);
  if (!m) return {};
  const out = {};
  for (const line of m[1].split('\n')) {
    const kv = line.match(/^(\w+):\s*(.+?)\s*$/);
    if (kv) out[kv[1]] = kv[2].replace(/^["']|["']$/g, '');
  }
  return out;
};

/* ---- the frame, borrowed rather than copied --------------------------
 *
 * The bar and the two stylesheets belong to the catalogue. They are read out
 * of a playground checkout at build time and NOT committed here: a copy in
 * this repository would be stale on the first change to either, which is the
 * whole argument the palette and the search engine already make. The places
 * looked in are `check:design`'s (scripts/lib/design.mjs) so there is one
 * convention for "where is the playground".
 *
 * The bar comes from a BUILT sample page rather than from the generator that
 * writes it, because what ships is the thing to copy. */
const HOMES = ['PLAYGROUND_HOME', '.playground', '../playground'];
const playground = HOMES
  .map((d) => (d.endsWith('_HOME') ? process.env[d] : path.join(ROOT, d)))
  .find((at) => at && fs.existsSync(path.join(at, 'src', 'catalogue', 'catalogue.css')));
if (!playground) {
  console.error('No playground checkout found (PLAYGROUND_HOME, .playground, ../playground).');
  console.error('This prototype wears the catalogue\'s frame, so it needs the catalogue.');
  process.exit(1);
}

const built = path.join(playground, 'dist', 'samples');
const anySample = fs.existsSync(built)
  && fs.readdirSync(built, { withFileTypes: true }).find((e) => e.isDirectory() && e.name.startsWith('z2ui5_'));
if (!anySample) {
  console.error(`No built sample pages under ${built} — run \`npm run build\` in the playground first.`);
  process.exit(1);
}
const BAR = (() => {
  const src = fs.readFileSync(path.join(built, anySample.name, 'index.html'), 'utf8');
  const m = src.match(/<header class="bar">[\s\S]*?<\/header>/);
  if (!m) throw new Error('no bar in the built sample page');
  return m[0]
    .replace(/(?:href|src)="\.\.\/\.\.\//g, (t) => t.slice(0, -6) + 'https://abap2ui5.github.io/playground/')
    .replace(/ aria-current="page"/g, '')
    .replace('data-site="docs"', 'data-site="docs" aria-current="page"');
})();

const urlOf = (page) => BASE + page.replace(/\.md$/, '.html');
const routeOf = (page) => '/' + page.replace(/\.md$/, '').replace(/\/index$/, '');

function sidebarFor(route) {
  const same = (link) => link && link.replace(/\/$/, '') === route.replace(/\/$/, '');
  const holds = (i) => same(i.link) || (i.items || []).some(holds);
  const tree = (items, level) => items.map((i) => {
    const on = same(i.link) ? ' class="here" aria-current="page"' : '';
    const href = i.link ? `${BASE.slice(0, -1)}${i.link}${i.link.endsWith('/') ? 'index.html' : '.html'}` : null;
    const label = href ? `<a href="${esc(href)}"${on}>${esc(i.text)}</a>` : `<span>${esc(i.text)}</span>`;
    if (!i.items) return `<div class="side-item level-${level}">${label}</div>`;
    return `<details class="side-group level-${level}"${holds(i) ? ' open' : ''}>`
      + `<summary><span class="side-caret" aria-hidden="true"></span>${label}</summary>`
      + `<div class="side-items">${tree(i.items, level + 1)}</div></details>`;
  }).join('');
  return `<nav class="sidebar" aria-label="Documentation">${tree(config.themeConfig.sidebar, 0)}</nav>`;
}

function outlineFor(html) {
  const rows = [...html.matchAll(/<h2 id="([^"]+)"[^>]*>([\s\S]*?)<a class="header-anchor"/g)]
    .map((m) => ({ id: m[1], text: m[2].replace(/<[^>]*>/g, '').trim() }));
  if (rows.length < 2) return '';
  return `<aside class="outline" aria-label="On this page">
    <div class="outline-head">On this page</div>
    <nav>${rows.map((r) => `<a href="#${r.id}">${esc(r.text)}</a>`).join('')}</nav>
  </aside>`;
}

function crumbsFor(page) {
  return trailFor(config.themeConfig.sidebar, page)
    .map((c, i) => (i ? '<span class="sep" aria-hidden="true">›</span>' : '')
      + (c.link
        ? `<a href="${esc(BASE.slice(0, -1) + c.link + (c.link.endsWith('/') ? 'index.html' : '.html'))}">${esc(c.text)}</a>`
        : `<span>${esc(c.text)}</span>`))
    .join('');
}

const shell = ({ title, body, page, route }) => `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${esc(title)} | abap2UI5</title>
<link rel="stylesheet" href="${BASE}catalogue.css">
<link rel="stylesheet" href="${BASE}sample.css">
<link rel="stylesheet" href="${BASE}docs.css">
<script>try{var t=localStorage.getItem("abap2ui5-playground:theme");if(t==="dark"||t==="light")document.documentElement.dataset.theme=t}catch(e){}</script>
</head>
<body>
${BAR}
<main class="manual">
  ${sidebarFor(route)}
  <div class="doc-body">
    <p class="crumbs">${crumbsFor(page)}</p>
    <div class="vp-doc">${body}</div>
    <div class="doc-foot">
      <a class="edit" href="${esc((config.themeConfig.editLink?.pattern || '').replace(':path', page))}"
         target="_blank" rel="noopener">${esc(config.themeConfig.editLink?.text || 'Edit this page on GitHub')} ↗</a>
      <span class="updated">Last updated: ${new Date(fs.statSync(path.join(DOCS, page)).mtime).toISOString().slice(0, 10)}</span>
    </div>
  </div>
  ${outlineFor(body)}
</main>
<footer class="foot"><p>
  <a href="${BASE}resources/license.html">License</a> |
  <a href="${BASE}resources/contact.html">Contact</a> —
  Copyright © 2023-2026 abap2UI5
</p></footer>
</body>
</html>
`;

/* ---- run ------------------------------------------------------------- */
const md = await createMarkdownRenderer(DOCS, config.markdown || {}, BASE);
fs.rmSync(OUT, { recursive: true, force: true });

let written = 0, headings = 0, blocks = 0, skipped = [];
for (const page of pages) {
  const src = fs.readFileSync(path.join(DOCS, page), 'utf8');
  const fm = front(src);
  if (fm.layout === 'home') { skipped.push(page); continue; }

  let body = md.render(src.replace(/^---\n[\s\S]*?\n---\n/, '')).replace(/ v-pre=""/g, '');
  /* The renderer puts the base in front of a LINK but not in front of an
     asset: VitePress rewrites those in a Vite step this build does not have,
     so `/get_started/image-2.png` arrives without the `/docs`. One rewrite,
     and only for paths that are root-relative and not already based - which
     is what the theme was quietly doing for us. */
  body = body.replace(/(\b(?:src|href)=")\/(?!docs\/)([^"]*)"/g, `$1${BASE}$2"`);
  const title = fm.title || (src.match(/^#\s+(.+)$/m) || [, page])[1];
  const html = shell({ title, body, page, route: routeOf(page) });

  const to = path.join(OUT, 'docs', page.replace(/\.md$/, '.html'));
  fs.mkdirSync(path.dirname(to), { recursive: true });
  fs.writeFileSync(to, html);
  written++;
  headings += (body.match(/<h2 id=/g) || []).length;
  blocks += (body.match(/class="language-/g) || []).length;
}

/* ---- what the pages need beside them --------------------------------- */
const copyInto = (from, to) => {
  if (!fs.existsSync(from)) return 0;
  let n = 0;
  for (const e of fs.readdirSync(from, { withFileTypes: true })) {
    const a = path.join(from, e.name), b = path.join(to, e.name);
    if (e.isDirectory()) { fs.mkdirSync(b, { recursive: true }); n += copyInto(a, b); }
    else { fs.mkdirSync(to, { recursive: true }); fs.copyFileSync(a, b); n++; }
  }
  return n;
};
/* publicDir goes to the root of the site, which is where llms.txt points and
   where every <img src="/docs/get_started/image-2.png"> resolves. */
const assets = copyInto(path.join(DOCS, 'public'), path.join(OUT, 'docs'));
/* The catalogue's two stylesheets come from its build; only the manual's own
   layer lives in this repository. */
for (const f of ['catalogue.css', 'sample.css']) fs.copyFileSync(path.join(built, f), path.join(OUT, 'docs', f));
fs.copyFileSync(path.join(ROOT, 'scripts', 'prototype-css', 'docs.css'), path.join(OUT, 'docs', 'docs.css'));

const seconds = ((Date.now() - started) / 1000).toFixed(1);
console.log(`${written} pages, ${headings} sections, ${blocks} code blocks, ${assets} files beside them — ${seconds}s`);
console.log(`   ${OUT}/docs/`);
if (skipped.length) console.log(`\nNOT BUILT: ${skipped.join(', ')} — the home page is a hero and a grid of tiles,`);
if (skipped.length) console.log('   which is a design of its own and not what this prototype is asking about.');
console.log('\nAlso still missing, on purpose:');
console.log('   the search box (the module exists, it is not wired in)');
console.log('   code-group tabs (the renderer emits them, the tabs need a few lines of JS)');
console.log('   line numbers in listings (theme/code-lines.js is standalone and would drop straight in)');
console.log('   the position memory between the four sites (a plain inline script on catalogue pages)');
console.log('   prev/next under an article, and a dead-link check the build does today');
