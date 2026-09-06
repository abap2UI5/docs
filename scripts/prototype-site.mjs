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

/* ---- the frontmatter ------------------------------------------------
 * Not parsed here. `md.render(src, env)` fills `env.frontmatter` with the
 * whole block, nested keys and all - the same @mdit-vue plugin the shipped
 * site reads it with - and strips it from the HTML on the way. So the home
 * page's hero, its three tiles and their inline SVGs arrive as objects, which
 * is what makes the front door buildable here at all. */

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
    .replace(/ aria-current="page"/g, '');
})();

/* WHICH OF THE FOUR THE READER IS ON. The bar names Home, Documentation,
 * Samples and Playground, and marks one of them; on this deployment that is
 * Documentation for every chapter and Home for the front door, which is a
 * different page of the same site.
 *
 * Both marks are made by finding a string in somebody else's markup, so both
 * throw when it is not there rather than quietly marking nothing: a bar with
 * nothing in bold reads as a bug in whichever site you came from. */
const marked = (find) => {
  if (!BAR.includes(find)) throw new Error(`the borrowed bar has no ${find} to mark`);
  return BAR.replace(find, `${find} aria-current="page"`);
};
const BAR_DOCS = marked('data-site="docs"');
const BAR_HOME = marked('href="https://abap2ui5.github.io/docs/"');

const urlOf = (page) => BASE + page.replace(/\.md$/, '.html');

/** A link out of the FRONTMATTER, based.
 *
 * The renderer rewrites every link in the body and cannot see these: the hero's
 * three buttons and the three tiles are frontmatter, and `/get_started/about`
 * arrived in the page as `/get_started/about` - a path that is not on this
 * deployment at all. An absolute URL is left exactly as written; it is another
 * site, and one of them carries a `target` that keeps a router off it. */
const linkOf = (href) => (/^[a-z]+:|^\/\//i.test(href)
  ? href
  : BASE + String(href).replace(/^\//, '') + (href.endsWith('/') ? 'index.html' : '.html'));
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

const shell = ({ title, main, bar }) => `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${esc(title)} | abap2UI5</title>
<link rel="stylesheet" href="${BASE}catalogue.css">
<link rel="stylesheet" href="${BASE}sample.css">
<link rel="stylesheet" href="${BASE}docs.css">
<script type="module" src="${BASE}site.js"></script>
<script type="module" src="${BASE}search.mjs"></script>
<script>try{var t=localStorage.getItem("abap2ui5-playground:theme");if(t==="dark"||t==="light")document.documentElement.dataset.theme=t}catch(e){}</script>
</head>
<body>
${bar}
${main}
<footer class="foot"><p>
  <a href="${BASE}resources/license.html">License</a> |
  <a href="${BASE}resources/contact.html">Contact</a> —
  Copyright © 2023-2026 abap2UI5
</p></footer>
</body>
</html>
`;

/** A chapter: the menu beside it, the crumb line, the article, the outline. */
const chapter = ({ body, page, route }) => `<main class="manual">
  <input class="side-open" type="checkbox" id="side-open">
  ${sidebarFor(route)}
  <label class="side-scrim" for="side-open" aria-hidden="true"></label>
  <div class="doc-body">
    <label class="side-button" for="side-open" title="Chapters"><span>Chapters</span></label>
    <p class="crumbs">${crumbsFor(page)}</p>
    <div class="vp-doc">${body}</div>
    <div class="doc-foot">
      <a class="edit" href="${esc((config.themeConfig.editLink?.pattern || '').replace(':path', page))}"
         target="_blank" rel="noopener">${esc(config.themeConfig.editLink?.text || 'Edit this page on GitHub')} ↗</a>
      <span class="updated">Last updated: ${new Date(fs.statSync(path.join(DOCS, page)).mtime).toISOString().slice(0, 10)}</span>
    </div>
  </div>
  ${outlineFor(body)}
</main>`;

/* ---- the front door ---------------------------------------------------
 *
 * The one page of this site that is not a chapter: a greeting, the headline,
 * the tagline, three buttons and three tiles, and then the markdown under the
 * frontmatter as an ordinary article. Every value it is drawn with - 20/28 for
 * the greeting, 38/46 for the headline, 17/26 for the tagline, 13 on a 34px
 * button, a 15/22 tile title over 13/22 of detail - is the one the page
 * carries today; what changes is the container. It was 1152 wide starting at
 * 144, which was VitePress's number, and it is the catalogue's 1160-with-20
 * here, so the front door opens on the same vertical as every chapter behind
 * it and as every one of the 772 sample pages.
 *
 * A link that leaves this deployment keeps `target="_self"` from the
 * frontmatter, which is what holds a router off a neighbouring site; nothing
 * here reads it, but the attribute is checked by `check:cross-site` and is
 * part of what the page means. */
const tile = (f) => `<a class="tile" href="${esc(linkOf(f.link))}"${f.target ? ` target="${esc(f.target)}"` : ''}>
      <span class="tile-icon" aria-hidden="true">${f.icon || ''}</span>
      <span class="tile-title">${esc(f.title)}</span>
      <span class="tile-details">${esc(f.details)}</span>
    </a>`;

const home = ({ body, fm }) => {
  const h = fm.hero || {};
  const img = h.image || {};
  return `<main class="home">
  <section class="hero">
    <div class="hero-main">
      ${h.name ? `<p class="hero-name">${esc(h.name)}</p>` : ''}
      ${h.text ? `<h1 class="hero-text">${esc(h.text)}</h1>` : ''}
      ${h.tagline ? `<p class="hero-tagline">${esc(h.tagline)}</p>` : ''}
      <div class="hero-actions">${(h.actions || []).map((a) => `
        <a class="hero-action ${a.theme === 'brand' ? 'primary' : 'plain'}" href="${esc(linkOf(a.link))}"${a.target ? ` target="${esc(a.target)}"` : ''}>${esc(a.text)}</a>`).join('')}
      </div>
    </div>
    ${img.src ? `<div class="hero-image"><img src="${esc(BASE + String(img.src).replace(/^\//, ''))}"
         alt="${esc(img.alt || '')}" width="200"></div>` : ''}
  </section>
  <section class="tiles">${(fm.features || []).map(tile).join('')}
  </section>
  <div class="vp-doc">${body}</div>
</main>`;
};

/* ---- run ------------------------------------------------------------- */
const md = await createMarkdownRenderer(DOCS, config.markdown || {}, BASE);
fs.rmSync(OUT, { recursive: true, force: true });

let written = 0, headings = 0, blocks = 0;
for (const page of pages) {
  const src = fs.readFileSync(path.join(DOCS, page), 'utf8');
  const env = {};
  let body = md.render(src, env).replace(/ v-pre=""/g, '');
  const fm = env.frontmatter || {};
  /* The renderer puts the base in front of a LINK but not in front of an
     asset: VitePress rewrites those in a Vite step this build does not have,
     so `/get_started/image-2.png` arrives without the `/docs`. One rewrite,
     and only for paths that are root-relative and not already based - which
     is what the theme was quietly doing for us. */
  body = body.replace(/(\b(?:src|href)=")\/(?!docs\/)([^"]*)"/g, `$1${BASE}$2"`);
  /* And a page written by hand as `/docs/resources/addons` gets its `.html`.
     VitePress resolves that in the router, and GitHub Pages happens to resolve
     it too, by trying `<path>.html` - so it was never broken on the site and
     is broken everywhere else, which is the kind of link that goes wrong on
     the day the host changes. A file that exists is named. */
  body = body.replace(/href="(\/docs\/[^"#?]*)([^"]*)"/g, (all, at, rest) => {
    const last = at.split('/').pop();
    if (at.endsWith('/')) return `href="${at}index.html${rest}"`;
    return last.includes('.') ? all : `href="${at}.html${rest}"`;
  });
  const title = fm.title || (src.match(/^#\s+(.+)$/m) || [, page])[1];
  const route = routeOf(page);
  const isHome = fm.layout === 'home';
  const html = shell({
    title,
    bar: isHome ? BAR_HOME : BAR_DOCS,
    main: isHome ? home({ body, fm }) : chapter({ body, page, route }),
  });

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
/* The catalogue's two stylesheets and its search box come from its build; only
   the manual's own layer and its own entry module live in this repository.
   search.mjs is the SAME FILE the 772 sample pages load - the box in this bar
   is not a second implementation of that one, it is that one, mounting into
   the `[data-search]` slot the borrowed bar already carries and reading the
   index this repository publishes. */
for (const f of ['catalogue.css', 'sample.css', 'search.mjs']) fs.copyFileSync(path.join(built, f), path.join(OUT, 'docs', f));
/* One adaptation to the borrowed stylesheet, and it is about DEPTH, not taste.
 * catalogue.css names the type as `../fonts/inter-…woff2`, which is right where
 * it lives: one directory down, in `dist/samples/`, beside `dist/fonts/`. Here
 * it sits at the root of this deployment, so `../fonts/` would leave the site
 * altogether - `/fonts/` on the shared origin is nobody's. The same two files
 * are already in this repository under docs/public/fonts and land beside it,
 * so the `..` goes and nothing else changes. */
{
  const at = path.join(OUT, 'docs', 'catalogue.css');
  const css = fs.readFileSync(at, 'utf8');
  if (!css.includes('../fonts/')) throw new Error('catalogue.css no longer names ../fonts/ - check what the type is now');
  fs.writeFileSync(at, css.replace(/\.\.\/fonts\//g, 'fonts/'));
}
fs.copyFileSync(path.join(ROOT, 'scripts', 'prototype-css', 'docs.css'), path.join(OUT, 'docs', 'docs.css'));
fs.copyFileSync(path.join(ROOT, 'scripts', 'prototype-js', 'site.js'), path.join(OUT, 'docs', 'site.js'));
/* The behaviour a page has beyond its markup was already framework-free in
   the theme - the Run button, the line numbers and their addresses, the link
   to a selection, the position memory between the four sites. `index.js` was
   the only Vue in front of them; these are the modules themselves. */
const THEME = path.join(DOCS, '.vitepress', 'theme');
const modules = ['playground.js', 'code-lines.js', 'link-to-selection.js', 'text-fragment.js', 'site-memory.js'];
for (const f of modules) fs.copyFileSync(path.join(THEME, f), path.join(OUT, 'docs', f));

const seconds = ((Date.now() - started) / 1000).toFixed(1);
console.log(`${written} pages, ${headings} sections, ${blocks} code blocks, ${assets} files beside them — ${seconds}s`);
console.log(`   ${OUT}/docs/`);
console.log('\nStill missing, on purpose:');
console.log('   code-group tabs (the renderer emits them, the tabs need a few lines of JS)');
console.log('   prev/next under an article, and a dead-link check the build does today');
