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
 *   node scripts/build-site.mjs [out-dir]
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
/* An explicit PLAYGROUND_URL wins over any checkout: it is how the fetched
   path is exercised without waiting for CI to be the first run of it. */
const playground = process.env.PLAYGROUND_URL ? null : HOMES
  .map((d) => (d.endsWith('_HOME') ? process.env[d] : path.join(ROOT, d)))
  .find((at) => at && fs.existsSync(path.join(at, 'dist', 'samples', 'catalogue.css')));

const PUBLISHED = (process.env.PLAYGROUND_URL || 'https://abap2ui5.github.io/playground').replace(/\/$/, '');

/** The four files of the frame, from a built checkout or from the site.
 *
 * A checkout is the fast path and the one to use while working: `PLAYGROUND_HOME`,
 * `.playground`, `../playground`, the same three `check:design` looks in - and
 * it has to be BUILT, because two of the four (`sample.css`, `search.mjs`) are
 * build outputs and the bar is lifted from a page the build writes.
 *
 * Without one, they come off the published site. That is not a fallback so
 * much as the arrangement CI uses: cloning and building the playground to
 * publish a page of this manual would be an hour of UI5 for four files, and
 * what those four files are is exactly what is deployed. `check:design`
 * already reaches for the published copy the same way when there is no
 * checkout, for the same reason.
 *
 * The bar comes from a per-sample page rather than the catalogue's index,
 * because those are written by the build with `../../` in front of every
 * shared asset - one rewrite turns all of them absolute. Which sample does not
 * matter and is not hard-coded: the first one the sitemap names.
 */
const fetchText = async (url) => {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${res.statusText} for ${url}`);
  return res.text();
};

const frame = await (async () => {
  if (playground) {
    const built = path.join(playground, 'dist', 'samples');
    const sample = fs.readdirSync(built, { withFileTypes: true })
      .find((e) => e.isDirectory() && e.name.startsWith('z2ui5_'));
    if (!sample) throw new Error(`no built sample pages under ${built} — run \`npm run build\` in the playground`);
    const file = (n) => fs.readFileSync(path.join(built, n), 'utf8');
    return {
      from: built,
      page: fs.readFileSync(path.join(built, sample.name, 'index.html'), 'utf8'),
      files: { 'catalogue.css': file('catalogue.css'), 'sample.css': file('sample.css'), 'search.mjs': file('search.mjs') },
    };
  }
  const sitemap = await fetchText(`${PUBLISHED}/sitemap.xml`);
  /* The PATH out of the sitemap, not the url in it: the sitemap names the
     published origin, and this may be pointed at a local copy of the same
     tree (PLAYGROUND_URL) to exercise exactly this path. */
  const one = sitemap.match(/<loc>[^<]*(\/samples\/z2ui5_[a-z0-9_]+\/)<\/loc>/);
  if (!one) throw new Error(`no per-sample page in ${PUBLISHED}/sitemap.xml to take the bar from`);
  const [page, ...files] = await Promise.all([
    fetchText(PUBLISHED + one[1]),
    fetchText(`${PUBLISHED}/samples/catalogue.css`),
    fetchText(`${PUBLISHED}/samples/sample.css`),
    fetchText(`${PUBLISHED}/samples/search.mjs`),
  ]);
  return {
    from: PUBLISHED,
    page,
    files: { 'catalogue.css': files[0], 'sample.css': files[1], 'search.mjs': files[2] },
  };
})();

const BAR = (() => {
  const m = frame.page.match(/<header class="bar">[\s\S]*?<\/header>/);
  if (!m) throw new Error(`no bar in the sample page from ${frame.from}`);
  return m[0]
    .replace(/(?:href|src)="\.\.\/\.\.\//g, (t) => t.slice(0, -6) + `${PUBLISHED}/`)
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

/* ---- the head ---------------------------------------------------------
 *
 * The whole of it, per page, because a link to a chapter that previews as the
 * site's front page is a link nobody clicks. What is here is what the theme
 * and `transformPageData` used to put there between them: the title, the
 * description, the canonical url, and the six og/twitter values that decide
 * what LinkedIn, Slack and WhatsApp draw. Those four take ABSOLUTE urls - a
 * relative one is silently dropped and the preview falls back to a grey card -
 * which is what SITE_URL is for. */
const SITE_URL = 'https://abap2ui5.github.io/docs';
const OG_IMAGE = `${SITE_URL}/og-image.png`;
const SITE_DESC = 'Build UI5 Apps Purely in ABAP';

const meta = ({ page, title, description }) => {
  const url = `${SITE_URL}/${page}`.replace(/index\.md$/, '').replace(/\.md$/, '.html');
  return [
    ['link', { rel: 'canonical', href: url }],
    ['meta', { name: 'description', content: description }],
    ['meta', { property: 'og:type', content: 'website' }],
    ['meta', { property: 'og:site_name', content: 'abap2UI5' }],
    ['meta', { property: 'og:url', content: url }],
    ['meta', { property: 'og:title', content: title }],
    ['meta', { property: 'og:description', content: description }],
    ['meta', { property: 'og:image', content: OG_IMAGE }],
    ['meta', { property: 'og:image:type', content: 'image/png' }],
    ['meta', { property: 'og:image:width', content: '1200' }],
    ['meta', { property: 'og:image:height', content: '630' }],
    ['meta', { property: 'og:image:alt', content: 'abap2UI5 — Build UI5 Apps Purely in ABAP' }],
    ['meta', { name: 'twitter:card', content: 'summary_large_image' }],
    ['meta', { name: 'twitter:image', content: OG_IMAGE }],
    ['meta', { name: 'twitter:title', content: title }],
    ['meta', { name: 'twitter:description', content: description }],
  ].map(([tag, attrs]) => `<${tag} ${Object.entries(attrs)
    .map(([k, v]) => `${k}="${esc(v)}"`).join(' ')}>`).join('\n');
};

const shell = ({ title, main, bar, head = '' }) => `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${esc(title)}</title>
<link rel="shortcut icon" href="${BASE}favicon.ico">
<link rel="apple-touch-icon" sizes="180x180" href="${BASE}favicon.ico">
<link rel="preload" href="${BASE}fonts/inter-roman-latin.woff2" as="font" type="font/woff2" crossorigin>
<link rel="stylesheet" href="${BASE}catalogue.css">
<link rel="stylesheet" href="${BASE}sample.css">
<link rel="stylesheet" href="${BASE}docs.css">
<script type="module" src="${BASE}site.js"></script>
<script type="module" src="${BASE}search.mjs"></script>
<script>try{var t=localStorage.getItem("abap2ui5-playground:theme");if(t==="dark"||t==="light")document.documentElement.dataset.theme=t}catch(e){}</script>
${head}
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

/* ---- what comes before and after ---------------------------------------
 *
 * The sidebar read as one list, in the order it is drawn, with the entries
 * that only open a section (no link of their own) skipped. A reader working
 * through the manual front to back has this and the menu; a reader who arrived
 * from a search has only this. */
const ORDER = (function flatten(items, out = []) {
  for (const i of items) {
    if (i.link) out.push({ text: i.text, link: i.link });
    if (i.items) flatten(i.items, out);
  }
  return out;
})(config.themeConfig.sidebar)
  /* A section and its first page name the same link - "Cookbook" and "View"
     both point at /cookbook/view/definition - and prev/next that walks the
     same page twice reads as a broken button. First mention wins, which is the
     section, because that is the name the crumb line uses too. */
  .filter((row, at, all) => all.findIndex((o) => o.link === row.link) === at);

const linkTo = (link) => BASE.slice(0, -1) + link + (link.endsWith('/') ? 'index.html' : '.html');

function prevNextFor(route) {
  const at = ORDER.findIndex((r) => r.link.replace(/\/$/, '') === route.replace(/\/$/, ''));
  if (at < 0) return '';
  const side = (row, which, arrow) => (row
    ? `<a class="pn pn-${which}" href="${esc(linkTo(row.link))}">
         <span class="pn-what">${which === 'prev' ? 'Previous' : 'Next'}</span>
         <span class="pn-title">${arrow === 'l' ? '← ' : ''}${esc(row.text)}${arrow === 'r' ? ' →' : ''}</span>
       </a>`
    : '<span></span>');
  return `<nav class="prev-next" aria-label="Previous and next page">
    ${side(ORDER[at - 1], 'prev', 'l')}${side(ORDER[at + 1], 'next', 'r')}
  </nav>`;
}

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
    ${prevNextFor(route)}
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
  const name = fm.title || (src.match(/^#\s+(.+)$/m) || [, page])[1];
  const route = routeOf(page);
  const isHome = fm.layout === 'home';
  /* The theme's own title template, and its own rule for the preview: a
     chapter previews as itself, the front door as the project. */
  const title = `${name} | abap2UI5`;
  const html = shell({
    title,
    head: meta({
      page,
      title: isHome ? 'abap2UI5 — Build UI5 Apps Purely in ABAP' : title,
      description: fm.description || SITE_DESC,
    }),
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

/* ---- the page for a url that is not a page -----------------------------
 *
 * GitHub Pages answers anything it cannot find under this deployment with the
 * 404.html at the root of the artefact, which is this. It carries the bar, so
 * a reader who mistyped a chapter is one click from the four sections rather
 * than on a white page with a sentence on it. */
fs.writeFileSync(path.join(OUT, 'docs', '404.html'), shell({
  title: 'Not found | abap2UI5',
  head: meta({ page: '404.md', title: 'Not found | abap2UI5', description: SITE_DESC }),
  bar: BAR_DOCS,
  main: `<main class="manual">
  <input class="side-open" type="checkbox" id="side-open">
  ${sidebarFor('/404')}
  <label class="side-scrim" for="side-open" aria-hidden="true"></label>
  <div class="doc-body">
    <label class="side-button" for="side-open" title="Chapters"><span>Chapters</span></label>
    <p class="crumbs"><a href="${BASE}get_started/about.html">Documentation</a></p>
    <div class="vp-doc">
      <h1>This page is not here</h1>
      <p>The address does not name a page of this manual. It may have been
         renamed, or the link that brought you here may be old.</p>
      <p>The menu beside this lists every chapter, the box in the bar searches
         the manual and all ~770 samples at once, and
         <a href="${BASE}get_started/about.html">In a Nutshell</a> is where the
         manual starts.</p>
    </div>
  </div>
</main>`,
}));

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
for (const [name, text] of Object.entries(frame.files)) fs.writeFileSync(path.join(OUT, 'docs', name), text);
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
fs.copyFileSync(path.join(ROOT, 'scripts', 'site-css', 'docs.css'), path.join(OUT, 'docs', 'docs.css'));
fs.copyFileSync(path.join(ROOT, 'scripts', 'site-js', 'site.js'), path.join(OUT, 'docs', 'site.js'));
/* The behaviour a page has beyond its markup was already framework-free in
   the theme - the Run button, the line numbers and their addresses, the link
   to a selection, the position memory between the four sites. `index.js` was
   the only Vue in front of them; these are the modules themselves. */
const THEME = path.join(DOCS, '.vitepress', 'theme');
const modules = ['playground.js', 'code-lines.js', 'link-to-selection.js', 'text-fragment.js', 'site-memory.js'];
for (const f of modules) fs.copyFileSync(path.join(THEME, f), path.join(OUT, 'docs', f));

/* ---- every internal link, before anything is published -----------------
 *
 * The build this replaced had one (VitePress refuses to finish on a dead
 * link), and a manual whose cross-references rot is a manual nobody trusts
 * twice. Root-relative only: an external url is somebody else's uptime, and a
 * relative one does not occur in what this writes.
 *
 * `/docs/x` with no extension counts as `/docs/x.html`, which is what GitHub
 * Pages resolves it to - the pages themselves are rewritten to say so, and
 * this is the net under that. */
const dead = [];
let checked = 0;
(function sweep(dir) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const at = path.join(dir, e.name);
    if (e.isDirectory()) { sweep(at); continue; }
    if (!e.name.endsWith('.html')) continue;
    const html = fs.readFileSync(at, 'utf8');
    for (const m of html.matchAll(/(?:href|src)="(\/[^"]*)"/g)) {
      const to = m[1].split('#')[0].split('?')[0];
      if (!to) continue;
      checked++;
      const target = path.join(OUT, to);
      if (fs.existsSync(target) || fs.existsSync(path.join(target, 'index.html'))
        || fs.existsSync(`${target}.html`)) continue;
      dead.push(`${at.slice(OUT.length)} -> ${m[1]}`);
    }
  }
})(OUT);
if (dead.length) {
  console.error(`\n${dead.length} dead link(s) - nothing is published with these:`);
  for (const d of [...new Set(dead)].slice(0, 25)) console.error(`   ${d}`);
  process.exit(1);
}

const seconds = ((Date.now() - started) / 1000).toFixed(1);
console.log(`${written} pages, ${headings} sections, ${blocks} code blocks, ${assets} files beside them,`);
console.log(`   ${checked} internal links, none of them dead — ${seconds}s`);
console.log(`   ${OUT}/docs/`);
console.log(`   frame from ${frame.from}`);
