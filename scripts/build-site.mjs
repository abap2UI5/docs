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
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import { createMarkdownRenderer } from 'vitepress';
import { build as bundle, transform } from 'esbuild';
import config from '../docs/.vitepress/config.mjs';
import { trailFor } from '../docs/.vitepress/theme/crumbs.js';
import { describe } from './lib/pages.mjs';
import { declaredRelease } from './lib/release.mjs';

const ROOT = process.cwd();
const DOCS = path.join(ROOT, 'docs');
const OUT = process.argv[2] || path.join(ROOT, '.prototype');
const BASE = '/docs/';
const started = Date.now();

/* Three characters were not enough: this is used inside ATTRIBUTES in nine
 * places - a canonical url, a description, an alt text, the menu's data-key -
 * and a double quote in any of them ends the attribute and starts writing
 * markup. No page carries one today, which is exactly the kind of "today" that
 * changes the day somebody writes `description: The "client" object`. In text
 * a `&quot;` is drawn as a quote, so nothing looks different for it. */
const esc = (s) => String(s)
  .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
  .replace(/"/g, '&quot;').replace(/'/g, '&#39;');

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
      highlighter: file('abap-highlight.mjs'),
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
    fetchText(`${PUBLISHED}/samples/abap-highlight.mjs`),
  ]);
  return {
    from: PUBLISHED,
    page,
    files: { 'catalogue.css': files[0], 'sample.css': files[1], 'search.mjs': files[2] },
    highlighter: files[3],
  };
})();

/* The front of THIS section. The wordmark and the Home item both point at it,
 * and the marker below finds the Home item by it. */
const HOME = 'https://abap2ui5.github.io/docs/';

/* EVERY EDIT BELOW IS TO THE NAV, AND CUTS IT OUT TO SAY SO.
 *
 * The bar names the catalogue twice: the wordmark on the left goes there as
 * well, from the sample page this is borrowed from. A plain `replace` takes
 * the first of the two, which is the wordmark - so `data-site="samples"`
 * landed on the wordmark and the Samples ITEM, the one a reader presses,
 * carried nothing for site.js to lift. It opened the front of the catalogue
 * however deep the reader had been, which is the memory not working at all
 * for the half of the bar most people use. */
const NAV = /<nav class="bar-nav">[\s\S]*?<\/nav>/;
const inNav = (bar, edit) => {
  const nav = bar.match(NAV);
  if (!nav) throw new Error(`no <nav class="bar-nav"> in the bar from ${frame.from}`);
  return bar.replace(NAV, edit(nav[0]));
};
/* ...and once means once. A marker that matches twice edits the wrong item and
 * one that matches nothing edits none, and both are silent. */
const once = (nav, find, add) => {
  const n = nav.split(find).length - 1;
  if (n !== 1) throw new Error(`${find} occurs ${n} times in the borrowed bar's nav, expected exactly one`);
  return nav.replace(find, find + add);
};

const BAR = (() => {
  const m = frame.page.match(/<header class="bar">[\s\S]*?<\/header>/);
  if (!m) throw new Error(`no bar in the sample page from ${frame.from}`);
  const brand = /(<a class="brand" href=")[^"]*"/;
  let bar = m[0]
    .replace(/(?:href|src)="\.\.\/\.\.\//g, (t) => t.slice(0, -6) + `${PUBLISHED}/`)
    .replace(/ aria-current="page"/g, '');
  /* The wordmark leads to the front of the section the reader is in - the
     catalogue, on the page this came from, and from here that walked out of
     the manual. Here the front of the section is the manual's own. */
  if (!brand.test(bar)) throw new Error(`no wordmark in the bar from ${frame.from}`);
  bar = bar.replace(brand, `$1${HOME}"`);
  /* THE SAMPLES ITEM HAS TO SAY WHICH SECTION IT RESTORES.
   *
   * The bar is lifted from a per-sample page, where Samples is the section the
   * reader is already IN - so it carries no `data-site`, because there is
   * nothing to come back to. Here it is one of the three places you leave for,
   * and without the attribute site.js has nothing to lift: pressing Samples
   * opened the front of the catalogue however deep the reader had been. The
   * `data-scope` is what the stored value is checked against, exactly as the
   * Documentation item over there declares its own. */
  return inNav(bar, (nav) => once(nav, `href="${PUBLISHED}/samples/"`,
    ` data-site="samples" data-scope="${PUBLISHED}/samples/"`));
})();

/* WHICH OF THE FOUR THE READER IS ON. The bar names Home, Documentation,
 * Samples and Playground, and marks one of them; on this deployment that is
 * Documentation for every chapter and Home for the front door, which is a
 * different page of the same site.
 *
 * Both marks are made by finding a string in somebody else's markup, so both
 * throw when it is not there rather than quietly marking nothing: a bar with
 * nothing in bold reads as a bug in whichever site you came from. */
/* THE BAR IS MARKUP AND BEHAVIOUR, and only the markup was being borrowed.
 *
 * Two of the bar's parts need a script: the light/dark switch in the menu
 * behind the last button, and the menu itself, which is a <details> that has
 * to close when the reader clicks anywhere else or presses Escape. A sample
 * page carries both in one inline script at the end of its body - so the
 * switch WORKED over there and did nothing at all here, on a bar that draws
 * the same button in the same place. A reader pressing it saw a page that
 * stayed light.
 *
 * It comes from the same page the bar comes from, for the same reason: one
 * implementation of a thing two documents show. Not the script beside it,
 * which writes down where the reader is - that one says `last-samples`, and
 * saying it here would tell the Samples item that the manual is a sample
 * page. site.js writes this site's own.
 *
 * Identified by what it DOES rather than by its position, and the match may
 * not cross a script boundary - `[\s\S]*?` would happily start at the
 * theme-restoring script in the head and swallow everything down to here. */
const MENU_SCRIPT = (() => {
  const one = /<script>(?:(?!<\/script>)[\s\S])*getElementById\("extra"\)(?:(?!<\/script>)[\s\S])*<\/script>/;
  const m = frame.page.match(one);
  if (!m) throw new Error(`no menu script in the sample page from ${frame.from} - the bar's switch would be dead`);
  if (!m[0].includes('getElementById("theme")')) throw new Error("the borrowed menu script no longer wires the theme switch");
  return m[0];
})();

/* ---- THE RELEASE, IN THE BAR ------------------------------------------
 *
 * The number used to stand at the top of the menu behind the bar's last
 * button - `VERSION` in SiteMenu.vue - and that file is the theme, which has
 * not rendered this site since the switch. So the manual stopped saying
 * anywhere in its frame which release it describes, while three gates went on
 * holding that number against the newest tag of abap2UI5/abap2UI5. A checked
 * fact nobody can see.
 *
 * It is read from the one place `check:version` and `check:examples` already
 * read it from, so there is still one number; and it links to the release
 * notes, because "which version is this" and "what changed in it" are the same
 * question two seconds apart.
 *
 * Written into the borrowed bar rather than into a page, so it is on all 166
 * of them and on the 404 - and, like everything else done to that bar, by
 * finding a string in somebody else's markup and failing loudly when it is not
 * there. */
const RELEASE = declaredRelease(ROOT);
const withRelease = (bar) => {
  const at = '<div class="socials">';
  if (!bar.includes(at)) throw new Error("the borrowed bar has no socials group to put the release before");
  /* INSIDE the socials group, not before it. The search slot is
     `position: absolute` - the box is centred in the bar rather than laid out
     in it - so anything placed in the flow after the nav lands underneath it:
     measured at x=606 with the search box at 603, the same 220px of bar. The
     socials are the flow's right-hand end, which is where this belongs
     anyway. */
  return bar.replace(at, `${at}\n    <a class="release" href="${BASE}resources/changelog.html"
       title="What changed in this release">Version ${esc(RELEASE)}</a>`);
};

const marked = (find) => inNav(BAR, (nav) => once(nav, find, ' aria-current="page"'));
const BAR_DOCS = withRelease(marked('data-site="docs"'));
const BAR_HOME = withRelease(marked(`href="${HOME}"`));

/* THE OTHER HALF OF THE PAIR IS THE BORROWED MARKUP'S, so it is checked rather
 * than assumed. `data-site` above says which page to come back to; `data-back`,
 * which the catalogue writes on every bar item that leads to a place, says to
 * come back to the same POSITION in it. Losing it upstream would take the
 * scroll memory and leave everything else working - the quietest half of a
 * failure this file has already shipped once. */
{
  const item = BAR.match(/<a [^>]*data-site="samples"[^>]*>/);
  if (!item) throw new Error('the Samples item did not come out of the bar surgery');
  if (!item[0].includes('data-back')) throw new Error(`the borrowed bar's Samples item has no data-back: ${item[0]}`);
}

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
    /* ONE ROW IS MARKED, and it is the deepest one that names this page. A
       section often points at its own first page - Model and Binding are the
       same link - and marking both painted two rows in the accent, which is
       one more than the colour means. */
    const on = same(i.link) && !(i.items || []).some(holds) ? ' class="here" aria-current="page"' : '';
    const href = i.link ? `${BASE.slice(0, -1)}${i.link}${i.link.endsWith('/') ? 'index.html' : '.html'}` : null;
    const label = href ? `<a href="${esc(href)}"${on}>${esc(i.text)}</a>` : `<span>${esc(i.text)}</span>`;
    if (!i.items) return `<div class="side-item level-${level}">${label}</div>`;
    return `<details class="side-group level-${level}" data-key="${esc(i.link || i.text)}"${holds(i) ? ' open' : ''}>`
      + `<summary><span class="side-caret" aria-hidden="true"></span>${label}</summary>`
      + `<div class="side-items">${tree(i.items, level + 1)}</div></details>`;
  }).join('');
  return `<nav class="sidebar" aria-label="Documentation">${tree(config.themeConfig.sidebar, 0)}</nav>`;
}

/* WHICH HEADINGS THE OUTLINE LISTS is the theme's setting, not a number here:
 * `themeConfig.outline` is [2, 6] on this site, and a page whose sections are
 * h3 under one h2 - which several chapters of the cookbook are - had an
 * outline of one row and therefore none at all. A sample page's outline is one
 * level deep because its headings are; the manual's is not.
 *
 * The rows are flat in the markup, as the catalogue's are, and the depth is a
 * class the stylesheet indents. */
const LEVELS = (() => {
  const o = config.themeConfig.outline;
  if (o === 'deep') return [2, 6];
  if (typeof o === 'number') return [o, o];
  if (Array.isArray(o)) return [o[0], o[1]];
  return [2, 2];
})();

function outlineFor(html) {
  const rows = [...html.matchAll(/<h([2-6]) id="([^"]+)"[^>]*>([\s\S]*?)<a class="header-anchor"/g)]
    .map((m) => ({ level: Number(m[1]), id: m[2], text: m[3].replace(/<[^>]*>/g, '').trim() }))
    .filter((r) => r.level >= LEVELS[0] && r.level <= LEVELS[1]);
  if (rows.length < 2) return '';
  /* The indent is relative to the SHALLOWEST heading on this page, not to h2.
     Several chapters put their sections in h3 under a single h2, and measured
     against h2 every row of those outlines started 13px in - an indent under
     nothing, which reads as a mistake rather than as a level. Where nothing is
     nested, the column is flat and identical to a sample page's. */
  const top = Math.min(...rows.map((r) => r.level));
  return `<aside class="outline" aria-label="On this page">
    <div class="outline-head">On this page</div>
    <nav>${rows.map((r) => `<a href="#${r.id}"${r.level > top ? ` class="lvl-${r.level - top + 2}"` : ''}>${esc(r.text)}</a>`).join('')}</nav>
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

/* The published markdown of a page, when there is one. `generate-llms.mjs`
 * writes one per SIDEBAR page - the front door and the 404 are not in the
 * sidebar and have none - and a head that names a file nobody wrote is a
 * 404 advertised in every preview and every crawler's queue. Asked of the
 * filesystem rather than assumed. */
const markdownTwin = (page) => {
  const at = page.replace(/\.md$/, '.md');
  return fs.existsSync(path.join(DOCS, 'public', at)) ? `${SITE_URL}/${at}` : null;
};

const canonical = (page) => `${SITE_URL}/${page}`.replace(/index\.md$/, '').replace(/\.md$/, '.html');

const meta = ({ page, title, description, kind = 'article' }) => {
  const url = canonical(page);
  return [
    ['link', { rel: 'canonical', href: url }],
    /* THE SAME PAGE AS MARKDOWN. `generate-llms.mjs` publishes one per page
       under docs/public and llms.txt tells a reader to "drop the .md for the
       rendered version" - but nothing on the page itself said the twin
       existed, so it could only be found by reading the index first. This is
       the standard way to say it, and it is one line. */
    ...(markdownTwin(page) ? [['link', { rel: 'alternate', type: 'text/markdown', href: markdownTwin(page) }]] : []),
    ['meta', { name: 'description', content: description }],
    /* A chapter is an `article` and the front door is the `website`. Every page
       said `website`, which is what a preview card uses to decide whether it is
       looking at a site or at something published ON one - and it is the same
       distinction the structured data below draws between the TechArticle and
       the WebSite it is part of. */
    ['meta', { property: 'og:type', content: kind }],
    ['meta', { property: 'og:site_name', content: 'abap2UI5' }],
    ['meta', { property: 'og:locale', content: 'en_US' }],
    ['meta', { property: 'og:url', content: url }],
    /* Without the site's name on the end: `og:site_name` already carries it,
       and a card that reads "Frontend · Event | abap2UI5 — abap2UI5" says it
       twice. The <title> keeps the suffix, because a browser tab has no other
       way to say which site it is. */
    ['meta', { property: 'og:title', content: title.replace(/ \| abap2UI5$/, '') }],
    ['meta', { property: 'og:description', content: description }],
    ['meta', { property: 'og:image', content: OG_IMAGE }],
    ['meta', { property: 'og:image:type', content: 'image/png' }],
    ['meta', { property: 'og:image:width', content: '1200' }],
    ['meta', { property: 'og:image:height', content: '630' }],
    ['meta', { property: 'og:image:alt', content: 'abap2UI5 — Build UI5 Apps Purely in ABAP' }],
    ['meta', { name: 'twitter:card', content: 'summary_large_image' }],
    ['meta', { name: 'twitter:image', content: OG_IMAGE }],
    ['meta', { name: 'twitter:title', content: title.replace(/ \| abap2UI5$/, '') }],
    ['meta', { name: 'twitter:description', content: description }],
  ].map(([tag, attrs]) => `<${tag} ${Object.entries(attrs)
    .map(([k, v]) => `${k}="${esc(v)}"`).join(' ')}>`).join('\n')
  + '\n' + linkedData({ page, title, description, url });
};

/* ---- what a machine reads instead of the page ------------------------
 *
 * The per-sample pages already carry this - a `SoftwareSourceCode` each - and
 * the manual carried none, so a search engine had a title, a description and
 * nothing that says what KIND of thing the page is or where it sits. Two
 * objects, both of them things this build already knows:
 *
 *   TechArticle     the page: what it is called, what it is about, where it
 *                   lives, and who publishes it.
 *   BreadcrumbList  the trail printed above the title - Documentation ›
 *                   Cookbook › Model. A result that shows the path a page sits
 *                   on tells a reader more than a url with three slashes in it.
 *
 * `</script>` cannot be written by any of this: JSON.stringify escapes the
 * quotes, and `<` is escaped as `\u003c` afterwards, which is the one
 * character that could end the block early. */
const linkedData = ({ page, title, description, url }) => {
  const trail = trailFor(config.themeConfig.sidebar, page);
  const json = JSON.stringify([
    {
      '@context': 'https://schema.org',
      '@type': 'TechArticle',
      headline: title.replace(/ \| abap2UI5$/, ''),
      description,
      url,
      inLanguage: 'en',
      isPartOf: { '@type': 'WebSite', name: 'abap2UI5', url: `${SITE_URL}/` },
      publisher: { '@type': 'Organization', name: 'abap2UI5', url: 'https://github.com/abap2UI5' },
    },
    {
      '@context': 'https://schema.org',
      '@type': 'BreadcrumbList',
      itemListElement: [
        ...trail.map((c, i) => ({
          '@type': 'ListItem', position: i + 1, name: c.text,
          /* Named exactly as the crumb line names it, `.html` and all - the
             same rule `crumbsFor` uses - so the url in the structured data is
             the url a reader would land on, and the one the page declares as
             canonical. */
          ...(c.link ? { item: SITE_URL + c.link + (c.link.endsWith('/') ? 'index.html' : '.html') } : {}),
        })),
        { '@type': 'ListItem', position: trail.length + 1, name: title.replace(/ \| abap2UI5$/, ''), item: url },
      ],
    },
  ]);
  return `<script type="application/ld+json">${json.replace(/</g, '\\u003c')}</script>`;
};

const shell = ({ title, main, bar, head = '' }) => `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<!-- 7.02, 1.71, 1.143.0: iOS reads a run of digits and dots as a telephone
     number and draws it as a link that dials. This manual is full of release
     numbers and of none that anybody can call. -->
<meta name="format-detection" content="telephone=no">
<title>${esc(title)}</title>
<script>try{var t=localStorage.getItem("abap2ui5-playground:theme");if(t==="dark"||t==="light")document.documentElement.dataset.theme=t}catch(e){}</script>
<link rel="icon" href="${BASE}favicon.ico" sizes="16x16 32x32 48x48">
<link rel="icon" type="image/png" href="${BASE}favicon.png" sizes="64x64">
<link rel="apple-touch-icon" sizes="180x180" href="${BASE}apple-touch-icon.png">
<meta name="theme-color" content="#f4f5f7" media="(prefers-color-scheme: light)">
<meta name="theme-color" content="#1e2024" media="(prefers-color-scheme: dark)">
<link rel="preload" href="${BASE}fonts/inter-roman-latin.woff2" as="font" type="font/woff2" crossorigin>
<link rel="stylesheet" href="${BASE}site.css">
<script type="module" src="${BASE}site.js"></script>
<script type="module" src="${BASE}search.mjs"></script>
<!-- TWO BUTTONS THAT ARE ONLY BUTTONS WITH JAVASCRIPT. The Run bar under a
     runnable example and the copy button on a listing are written into the
     page by the build, and both do their work in the browser - so with
     scripting off they are controls that look pressable and answer nothing.
     Everything else on this site works without it: the pages are static, the
     menu is a checkbox, the listings are already coloured. -->
<noscript><style>.vp-doc .a2ui5-play-run, .vp-doc div[class*="language-"] .copy { display: none; }</style></noscript>
${head}
</head>
<body>
<a class="skip" href="#main-content">Skip to content</a>
${bar}
${main}
<footer class="foot"><p>
  <a href="${BASE}resources/license.html">License</a> |
  <a href="${BASE}resources/contact.html">Contact</a> —
  Copyright © 2023-${new Date().getFullYear()} abap2UI5
</p></footer>
${MENU_SCRIPT}
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
/* ---- WHEN A PAGE LAST CHANGED --------------------------------------
 *
 * The date of the commit that last touched it, not the file's mtime. Git does
 * not store mtimes, so a fresh clone gives every file the time of the clone -
 * which is what the foot of every page was printing. On a machine that has
 * had the repository a while it says the day it was cloned; in CI, where the
 * checkout is minutes old, all 166 pages would have said the day of the
 * deploy. A "last updated" that means "last deployed" is worse than none.
 *
 * One `git log` for the whole tree rather than one per file, and a checkout
 * too shallow to know - CI clones a pull request at depth 1 - falls back to
 * today, which is the only thing it can honestly say then. The sitemap's
 * `lastmod` is the same answer from the same place. */
const lastTouched = (() => {
  const when = new Map();
  try {
    const log = execFileSync('git', ['log', '--pretty=format:%cs', '--name-only', '--', 'docs'],
      { cwd: ROOT, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 });
    let date = '';
    for (const line of log.split('\n')) {
      if (/^\d{4}-\d{2}-\d{2}$/.test(line)) { date = line; continue; }
      const f = line.trim();
      if (f.startsWith('docs/') && f.endsWith('.md') && !when.has(f)) when.set(f, date);
    }
  } catch { /* no git, or a checkout with no history in it */ }
  return (page) => when.get(`docs/${page}`) || new Date().toISOString().slice(0, 10);
})();

const chapter = ({ body, page, route }) => `<main class="manual">
  <input class="side-open" type="checkbox" id="side-open">
  ${sidebarFor(route)}
  <label class="side-scrim" for="side-open" aria-hidden="true"></label>
  <div class="doc-body" id="main-content" tabindex="-1">
    <label class="side-button" for="side-open" title="Chapters" aria-label="Chapters"><span>Chapters</span></label>
    <nav class="crumbs" aria-label="Breadcrumb">${crumbsFor(page)}</nav>
    <div class="vp-doc">${body}</div>
    <div class="doc-foot">
      <a class="edit" href="${esc((config.themeConfig.editLink?.pattern || '').replace(':path', page))}"
         target="_blank" rel="noopener">${esc(config.themeConfig.editLink?.text || 'Edit this page on GitHub')} ↗</a>
      <span class="updated">Last updated: <time datetime="${lastTouched(page)}">${lastTouched(page)}</time></span>
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
  return `<main class="home" id="main-content" tabindex="-1">
  <section class="hero">
    <div class="hero-main">
      ${h.name ? `<p class="hero-name">${esc(h.name)}</p>` : ''}
      ${h.text ? `<h1 class="hero-text">${esc(h.text)}</h1>` : ''}
      ${h.tagline ? `<p class="hero-tagline">${esc(h.tagline)}</p>` : ''}
      <div class="hero-actions">${(h.actions || []).map((a) => `
        <a class="hero-action ${a.theme === 'brand' ? 'primary' : 'plain'}" href="${esc(linkOf(a.link))}"${a.target ? ` target="${esc(a.target)}"` : ''}>${esc(a.text)}</a>`).join('')}
      </div>
    </div>
    ${img.src ? (() => {
      /* The one image on this site that is NOT lazy - it is the first thing on
         the front door - and the only one `sized` never sees, because the hero
         is this template rather than markdown. It still needs its height, or
         the tagline under it jumps when the file lands. */
      const at = BASE + String(img.src).replace(/^\//, '');
      const box = measure(path.join(DOCS, 'public', at.slice(BASE.length)));
      const h = box && box.w ? ` height="${Math.round(box.h * 200 / box.w)}"` : '';
      return `<div class="hero-image"><img src="${esc(at)}"
         alt="${esc(img.alt || '')}" width="200"${h} decoding="async"></div>`;
    })() : ''}
  </section>
  <section class="tiles">${(fm.features || []).map(tile).join('')}
  </section>
  <div class="vp-doc">${body}</div>
</main>`;
};

/* ---- the ABAP, tokenised by the file the catalogue tokenises with -------
 *
 * Not "the same colours" - the same PROGRAM. `abap-highlight.mjs` is what
 * writes the red and the green into all 772 per-sample pages, and it is
 * published beside them for this; the manual runs it over its own listings and
 * gets the same answer token for token, which "the same palette applied to
 * somebody else's tokenising" does not: Shiki's ABAP grammar draws the lines
 * elsewhere, and `DATA name TYPE` came out with the name inside the keyword.
 *
 * It replaces the CONTENT of each line and nothing else. The `<span
 * class="line">` wrappers stay, because code-lines.js numbers them and every
 * line of every listing on this site has an address (#B2L42); so does the card,
 * the language label and the Run button under it.
 *
 * The text has to be handed over as it was written, which means undoing the
 * escaping the renderer did - and `&amp;` last, or `&amp;lt;` becomes a tag. */
/* ONE PASS, LEFT TO RIGHT, and it has to be. Five `replace` calls in a row
 * were wrong twice over: they knew five named entities and Shiki writes
 * NUMERIC ones - `&#x3C;` for `<` and `&#x26;` for `&` - so an XML view inside
 * an ABAP string template arrived as the literal text `&#x3C;mvc:View`, the
 * highlighter escaped its `&` again, and 28 pages printed `&#x3C;mvc:View`
 * where the reader should have seen `<mvc:View`. And a chain of replaces
 * re-reads what the one before it produced: `&amp;lt;` - which is how an ABAP
 * source containing the four characters `&lt;` reaches here - would decode to
 * `&lt;` and then to `<`, which is a different program.
 *
 * A single regex with an alternation never re-scans its own output, so each
 * entity is decoded exactly once and the order stops mattering. An entity this
 * does not know is left exactly as it was rather than guessed at. */
const CHARS = { lt: '<', gt: '>', quot: '"', apos: "'", amp: '&', nbsp: '\u00a0' };
const unescape = (s) => s.replace(/&(#x[0-9a-fA-F]+|#\d+|[a-zA-Z][a-zA-Z0-9]*);/g, (all, what) => {
  if (what[0] !== '#') return Object.hasOwn(CHARS, what) ? CHARS[what] : all;
  const code = what[1] === 'x' || what[1] === 'X' ? parseInt(what.slice(2), 16) : Number(what.slice(1));
  return Number.isFinite(code) && code > 0 && code <= 0x10ffff ? String.fromCodePoint(code) : all;
});

/* CODE IS NOT PROSE, and a browser that offers to translate the page will
 * happily rewrite it: Chrome translates the text inside a <pre> like any
 * other, so a reader who has translation on gets `CLASS` as `KLASSE` and
 * `client->view_display( )` with its identifier rewritten - source that no
 * longer compiles, in a manual whose whole point is source that does. The
 * attribute is the standard way to say so, and it costs nothing.
 * `.line` rather than the block, because the language label above it is prose
 * and may be translated. */
const notProse = (html) => html.replace(/<code>/g, '<code translate="no">');

/* ...AND A BLOCK YOU CAN TAB TO SAYS WHAT IT IS. The renderer gives every
 * listing `tabindex="0"` so that a reader on a keyboard can scroll one that is
 * wider than the column - and a focusable region with no accessible name is
 * announced as nothing at all. The language is already written above the block
 * for everybody else; this is the same word, for a reader who cannot see it. */
let seenBlocks = 0;
const named = (html) => html.replace(
  /<div class="language-([\w-]*)([^"]*)">([\s\S]*?)<pre ([^>]*?)tabindex="0"/g,
  (all, lang, rest, between, attrs) => {
    /* Numbered, because the name is what a reader hears in a list of the
       page's regions and "abap code" ten times over is a list of ten things
       that cannot be told apart. The number is the one this site already
       addresses a listing by: `#B2L42` is line 42 of the second listing. */
    const n = ++seenBlocks;
    const what = lang ? `Listing ${n}, ${esc(lang)}` : `Listing ${n}`;
    return `<div class="language-${lang}${rest}">${between}<pre ${attrs}tabindex="0" role="region" aria-label="${what}"`;
  },
);

const abapify = (html) => html.replace(
  /(<div class="language-abap[^"]*">[\s\S]*?<code>)([\s\S]*?)(<\/code>)/g,
  (all, head, code, tail) => {
    const lines = [...code.matchAll(/<span class="line">([\s\S]*?)<\/span>\s*(?=<span class="line">|$)/g)]
      .map((m) => unescape(m[1].replace(/<[^>]*>/g, '')));
    if (!lines.length) return all;
    const source = lines.join('\n');
    /* AND NOTHING MAY STILL BE AN ESCAPE. This is the check that would have
       caught `&#x3C;mvc:View`, and the round trip below would not: Shiki
       writes NUMERIC entities, the decoder above knew only named ones, so
       `<` arrived as the four characters `&#x` `3` `C` `;`, was escaped a
       second time on the way out, and came back through the same blind
       decoder unchanged - agreeing with itself, wrongly, on 28 pages. An
       entity that survives decoding is a decoder that does not know it. ABAP
       has no reason to contain one literally; if a listing ever does, this
       stops the build rather than printing it as markup. */
    const left = source.match(/&(?:#x?[0-9a-fA-F]+|lt|gt|amp|quot|apos);/);
    if (left) throw new Error(`an escape survived decoding in a listing: ${left[0]} in ${JSON.stringify(source.slice(Math.max(0, left.index - 30), left.index + 40))}`);
    const coloured = highlightAbapLines(source);
    /* THE HIGHLIGHTER MAY COLOUR, IT MAY NOT REWRITE. Reading the text back out
       of what it returned has to give exactly the text it was given: that is
       the contract, and this is what holds a highlighter that drops, reorders
       or re-escapes a character. */
    const back = coloured.map((line) => unescape(line.replace(/<[^>]*>/g, ''))).join('\n');
    if (back !== source) {
      const at = [...source].findIndex((c, i) => c !== back[i]);
      throw new Error(`a listing came back changed at character ${at}:\n`
        + `   source: ${JSON.stringify(source.slice(Math.max(0, at - 20), at + 40))}\n`
        + `   after:  ${JSON.stringify(back.slice(Math.max(0, at - 20), at + 40))}`);
    }
    return head + coloured
      .map((line) => `<span class="line">${line}</span>`).join('\n') + tail;
  },
);

/* ---- and every other language, in the colours a sample page prints ABAP in -
 *
 * Shiki hands every token an inline `--shiki-light` / `--shiki-dark` pair from
 * the github themes, which is a scheme of its own: functions purple, types
 * blue, seven colours in all. A per-sample page over in the catalogue prints
 * the SAME ABAP in four - keywords red, literals green, numbers purple,
 * comments grey and italic, everything else the body colour - written into the
 * markup at build time by tools/abap-highlight.mjs and coloured by
 * `catalogue.css`, which this site already loads.
 *
 * So the pair is swapped for the class the catalogue styles. The colours then
 * come from ONE place for both documents, light and dark, and this file states
 * no hex at all.
 *
 * Three of the seven are decided by the token TEXT rather than by its colour,
 * because github gives `constant` to three things the catalogue tells apart:
 * `42` is a number, `|` and `{` in a string template are part of the string,
 * and `string` after TYPE is an ordinary word. Verified against the
 * catalogue's own highlighter, run over the same lines.
 *
 * An unknown colour STOPS THE BUILD. A Shiki release that adds one would
 * otherwise silently print that token in the body colour, and nobody would
 * look at a listing again to notice. */
const INK = {
  '#D73A49': () => 'code-key',      // keywords
  '#032F62': () => 'code-string',   // string literals
  '#6A737D': () => 'code-comment',  // comments
  '#24292E': () => '',              // the body colour: no class at all
  '#6F42C1': () => '',              // a class or method NAME is plain over there
  '#22863A': () => '',              // an XML tag name, likewise
  '#E36209': () => '',              // a parameter, likewise
  '#005CC5': (text) => {
    const t = text.trim();
    if (!t) return '';
    if (/^[0-9]+(\.[0-9]+)?$/.test(t)) return 'code-number';
    if (/^[|{}]+$/.test(t)) return 'code-string';   // a string template's own marks
    return '';
  },
};

/* ---- HOW BIG AN IMAGE IS, BEFORE IT ARRIVES ---------------------------
 *
 * A markdown image is `<img src="…">` and nothing else, so the browser
 * discovers its shape only when the file lands: the article is laid out
 * without it, the paragraph below sits where the picture will be, and it is
 * pushed down when the bytes arrive. On the quickstart that is four
 * screenshots pushing the text the reader is on, twice.
 *
 * The build has the file, so it can say. `width`/`height` are the intrinsic
 * pixels; what they are FOR here is the ratio - `.vp-doc img` caps the width
 * at the column and `height: auto` scales the rest, and a browser given both
 * attributes reserves the right box before the image exists.
 *
 * An image with a width already on it (`{width=64}` in the markdown) keeps it
 * and is given the height that width implies, so it reserves its box too. A
 * percentage width, or a file this cannot measure - an external one, above
 * all - is left exactly as it was. */
const measure = (file) => {
  let b;
  try { b = fs.readFileSync(file); } catch { return null; }
  if (b.length > 24 && b.readUInt32BE(0) === 0x89504e47 && b.toString('latin1', 12, 16) === 'IHDR')
    return { w: b.readUInt32BE(16), h: b.readUInt32BE(20) };
  if (b.length > 4 && b[0] === 0xff && b[1] === 0xd8) {
    for (let i = 2; i + 9 < b.length;) {
      if (b[i] !== 0xff) { i++; continue; }
      const marker = b[i + 1];
      if (marker >= 0xc0 && marker <= 0xcf && ![0xc4, 0xc8, 0xcc].includes(marker))
        return { h: b.readUInt16BE(i + 5), w: b.readUInt16BE(i + 7) };
      i += 2 + b.readUInt16BE(i + 2);
    }
    return null;
  }
  const svg = b.toString('utf8', 0, 2000);
  const box = svg.match(/viewBox="[\d.+-]+\s+[\d.+-]+\s+([\d.]+)\s+([\d.]+)"/);
  if (box) return { w: Math.round(+box[1]), h: Math.round(+box[2]) };
  return null;
};

/* ...and one that is not on the screen yet is not fetched yet. A chapter with
 * four screenshots fetched all four before the reader had scrolled past the
 * first paragraph. `loading="lazy"` from the SECOND image down - the first may
 * well be in the first screen, and lazy-loading something already in view only
 * delays it - and `decoding="async"` on all of them, so decoding a large PNG
 * never blocks the paint of the text around it. */
let seenImages = 0;
const sized = (html) => html.replace(/<img ([^>]*?)src="([^"]+)"([^>]*)>/g, (tag, before, src, after) => {
  /* The loading hints go on every image, including one hosted somewhere else -
     those are the slowest of all, and the ones a reader is likeliest to be
     waiting on for nothing. */
  const hinted = /\b(?:loading|decoding)=/.test(tag);     // a page that said it itself keeps what it said
  const later = hinted ? '' : (seenImages++ ? ' loading="lazy" decoding="async"' : ' decoding="async"');
  const size = src.startsWith(BASE) ? measure(path.join(DOCS, 'public', src.slice(BASE.length))) : null;
  if (/\bheight=/.test(tag) || !size || !size.w || !size.h) return tag.replace(/\s*\/?>$/, `${later}>`);
  const declared = tag.match(/\bwidth="(\d+)"/);
  if (declared) return tag.replace(/>$/, ` height="${Math.round(size.h * +declared[1] / size.w)}"${later}>`);
  if (/\bwidth=/.test(tag)) return tag.replace(/>$/, `${later}>`);   // a percentage, or something else
  return `<img ${before}src="${src}" width="${size.w}" height="${size.h}"${later}${after}>`;
});

const recolour = (html) => html.replace(
  /<span style="--shiki-light:(#[0-9A-F]{6});--shiki-dark:#[0-9A-F]{6}">([^<]*)<\/span>/g,
  (all, light, text) => {
    const of = INK[light];
    if (!of) throw new Error(`the highlighter used ${light}, which no catalogue colour is named for`);
    const cls = of(text);
    return cls ? `<span class="${cls}">${text}</span>` : text;
  },
);

/* ---- run ------------------------------------------------------------- */
const md = await createMarkdownRenderer(DOCS, config.markdown || {}, BASE);
fs.rmSync(OUT, { recursive: true, force: true });

/* The borrowed highlighter, made importable. It is a module, not data, and the
 * only way to run somebody else's module is to have it on disk - so it is
 * written OUTSIDE `docs/`, one level up from the site: this build uses it, and
 * nothing on the site loads it. */
fs.mkdirSync(OUT, { recursive: true });
const highlighterAt = path.join(OUT, 'abap-highlight.mjs');
fs.writeFileSync(highlighterAt, frame.highlighter);
const { highlightAbapLines } = await import(pathToFileURL(highlighterAt).href);
if (typeof highlightAbapLines !== 'function') {
  throw new Error(`abap-highlight.mjs from ${frame.from} no longer exports highlightAbapLines( )`);
}

/* ---- TWO PAGES CALLED THE SAME THING ----------------------------------
 *
 * `Frontend` is a chapter of Cookbook › Event & Navigation and also one of
 * Advanced › Extensibility, and both said `Frontend | abap2UI5` and nothing
 * else. In a browser's history, in a list of open tabs and in a search result
 * the two were the same page, and the one the reader wanted was a coin toss.
 *
 * The crumb line above the title has always told them apart; this puts the
 * same word in the title, and ONLY for a name that actually repeats - a title
 * carrying its section when nothing collides is noise in every tab. */
const nameOf = (page) => {
  const src = fs.readFileSync(path.join(DOCS, page), 'utf8');
  const fm = src.match(/^---\r?\n([\s\S]*?)\r?\n---/);
  const declared = fm && fm[1].match(/^title:\s*(.+)$/m);
  if (declared) return declared[1].trim().replace(/^["']|["']$/g, '');
  return (src.match(/^#\s+(.+)$/m) || [, page])[1].trim();
};
/* Read once. `nameOf` opens the file, and this used to call it twice per page
   inside the same expression - 332 reads to count 166 names. */
const names = new Map(pages.map((page) => [page, nameOf(page)]));
const shared = (() => {
  const seen = new Map();
  for (const name of names.values()) seen.set(name, (seen.get(name) || 0) + 1);
  return new Set([...seen].filter(([, n]) => n > 1).map(([name]) => name));
})();

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
  seenImages = 0;
  seenBlocks = 0;
  body = named(notProse(sized(recolour(abapify(body)))));
  /* A link that opens a new tab hands that tab a `window.opener` pointing at
     this one unless it says otherwise. Every current browser implies
     `noopener` for `target="_blank"` and has since 2020 - this is for the ones
     that do not, and it costs nothing. `noreferrer` is deliberately NOT added:
     these links go to the project's own repositories, and the referrer is how
     they can tell the manual sent you. */
  body = body.replace(/<a ([^>]*\btarget="_blank"[^>]*)>/g, (tag, attrs) =>
    /\brel="/.test(attrs) ? tag.replace(/\brel="(?![^"]*noopener)/, 'rel="noopener ') : `<a ${attrs} rel="noopener">`);
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
     chapter previews as itself, the front door as the project. A name two
     chapters share carries the section that tells them apart. */
  const under = shared.has(name) ? trailFor(config.themeConfig.sidebar, page).at(-1)?.text : null;
  /* The front door's own title is the project and its claim, not "Home |
     abap2UI5" - which is what a tab among twenty says, and what a search
     result offers to be clicked. It is what the preview card has always
     said; now the tab says it too. */
  const title = isHome ? 'abap2UI5 — Build UI5 Apps Purely in ABAP'
    : `${name}${under && under !== name ? ` · ${under}` : ''} | abap2UI5`;
  const html = shell({
    title,
    head: meta({
      page,
      title,
      kind: isHome ? 'website' : 'article',
      /* 151 of the 166 pages declare no description of their own, and every
         one of them was being given the project's slogan. That is the same
         sentence under 151 different results in a search engine, and the same
         card in Slack and LinkedIn whichever chapter was shared - a preview
         that says nothing about the page it previews.
         The page's own opening sentence is a better description than a
         constant, and the site already knows how to take one: `describe` is
         what writes the one-line note beside every entry of llms.txt and the
         line under every hit in the search box - the page's declared
         description when it has one, its opening sentence when it has not.
         Same sentence in all three places, one implementation. The slogan stays for the front door, whose subject
         really is the project, and for a page that opens with something a
         sentence cannot be taken from. */
      description: isHome ? (fm.description || SITE_DESC) : (describe(src) || SITE_DESC),
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
/* Every page as [path, name], for the suggestions at the foot of the 404 - the
   one place that wants the whole list inside one page. `<` is escaped, so no
   chapter title can end the script block early. */
const nearby = JSON.stringify(pages.map((f) => [f.replace(/(?:\/index)?\.md$/, ''), names.get(f)]))
  .replace(/</g, '\\u003c');

fs.writeFileSync(path.join(OUT, 'docs', '404.html'), shell({
  title: 'Not found | abap2UI5',
  /* Served with a 404 status, which is what a crawler goes by - and said in
     the head as well, for the case where it is not (a preview, a mirror, a
     proxy that rewrites the status). */
  head: `<meta name="robots" content="noindex">\n`
    + meta({ page: '404.md', title: 'Not found | abap2UI5', description: SITE_DESC, kind: 'website' }),
  bar: BAR_DOCS,
  main: `<main class="manual">
  <input class="side-open" type="checkbox" id="side-open">
  ${sidebarFor('/404')}
  <label class="side-scrim" for="side-open" aria-hidden="true"></label>
  <div class="doc-body" id="main-content" tabindex="-1">
    <label class="side-button" for="side-open" title="Chapters" aria-label="Chapters"><span>Chapters</span></label>
    <nav class="crumbs" aria-label="Breadcrumb"><a href="${BASE}get_started/about.html">Documentation</a></nav>
    <div class="vp-doc">
      <h1>This page is not here</h1>
      <p>The address does not name a page of this manual. It may have been
         renamed, or the link that brought you here may be old.</p>
      <p>Chapters lists every page of the manual, the box in the bar searches
         it and all ~770 samples at once, and
         <a href="${BASE}get_started/about.html">In a Nutshell</a> is where the
         manual starts.</p>
      <div id="near" hidden><h2>Did you mean</h2><ul></ul></div>
    </div>
  </div>
</main>
<script>
/* WHAT THE ADDRESS ALMOST SAID. Chapters get renamed - two dozen urls that
   once worked are 404s now - and every link to one of them out in a blog post,
   an issue or a Slack message lands here. A page that only says "not here" and
   points at the front of the manual makes the reader search for something they
   had already named.
   The whole index is IN this page: 166 titles and paths, about 12 kB, so
   nothing is fetched to answer and it works with the network already gone.
   Matching is on the words of the path, which is what an old url still carries
   - /technical/how_it_all_works still says "how it all works" - scored by how
   many of them a page's own path and title contain.
   (No backticks and no backslashes anywhere in here: this whole block is
   inside a template literal, which eats both - a backtick ends the string
   mid-sentence, and a backslash is stripped before the browser ever sees it,
   which turned /^\// into /^// and made the rest of the line a comment. Both
   happened. The character classes below say the same thing without one.) */
(function () {
  var pages = ${nearby};
  var words = decodeURIComponent(location.pathname)
    .replace(/[.]html?$/, "").split(/[^a-zA-Z0-9]+/).filter(function (w) { return w.length > 2; })
    .map(function (w) { return w.toLowerCase(); });
  if (!words.length) return;
  /* A WHOLE WORD IS WORTH MORE THAN A SUBSTRING, or "all" in an old address
     matches "installation" and the suggestions are noise. Both are counted,
     the whole word at three times the weight, and a substring only from four
     letters up. */
  var scored = pages.map(function (p) {
    var tokens = (p[0] + " " + p[1]).toLowerCase().split(/[^a-z0-9]+/);
    var hay = tokens.join(" ");
    var n = 0;
    for (var i = 0; i < words.length; i++) {
      if (tokens.indexOf(words[i]) >= 0) n += 3;
      else if (words[i].length > 3 && hay.indexOf(words[i]) >= 0) n += 1;
    }
    return [n, p];
  }).filter(function (s) { return s[0] >= 3; })
    .sort(function (a, b) { return b[0] - a[0] || a[1][1].length - b[1][1].length; })
    .slice(0, 5);
  if (!scored.length) return;
  var box = document.getElementById("near");
  var ul = box.querySelector("ul");
  scored.forEach(function (s) {
    var li = document.createElement("li");
    var a = document.createElement("a");
    a.href = "${BASE}" + s[1][0].replace(/^[/]/, "") + ".html";
    a.textContent = s[1][1];
    li.appendChild(a);
    ul.appendChild(li);
  });
  box.hidden = false;
})();
</script>`,
}));

/* ---- WHAT IS HERE, FOR A CRAWLER --------------------------------------
 *
 * 166 pages and nothing saying so. The playground publishes a sitemap beside
 * its 772; this site published none, so a crawler had to find every page by
 * following links from whichever one it landed on, and a page reachable only
 * through the menu (which is a link in every page, so most are fine) or only
 * through prev/next is a page it is entitled to give up on.
 *
 * `lastmod` is the date of the commit that last touched the page, not the
 * date of the build: a deploy that changed one chapter should not tell a
 * crawler that all 166 changed. One `git log` for the whole tree rather than
 * one per file, and a checkout too shallow to know - CI clones a pull request
 * at depth 1 - falls back to the build's date, which is the only thing it can
 * honestly say then.
 *
 * robots.txt is deliberately NOT written, for the reason the playground gives
 * for not writing one either: a crawler reads /robots.txt at the ORIGIN root,
 * and abap2ui5.github.io/ belongs to another repository. This file is
 * discovered by being submitted, or through the links. */


fs.writeFileSync(path.join(OUT, 'docs', 'sitemap.xml'),
  `<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n`
  + pages.map((page) => `<url><loc>${esc(canonical(page))}</loc><lastmod>${lastTouched(page)}</lastmod></url>`).join('\n')
  + `\n</urlset>\n`);

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
for (const [name, text] of Object.entries(frame.files))
  if (!name.endsWith('.css')) fs.writeFileSync(path.join(OUT, 'docs', name), text);

/* ---- ONE STYLESHEET ---------------------------------------------------
 *
 * Three files were linked in the head - the catalogue's, the sample page's,
 * and the manual's own layer on top - in that order, because that order is
 * the cascade. They are joined here in the same order and minified, which is
 * not a nicety on these particular files: they are written in the house style,
 * with the reasoning for every rule beside it, and comments are most of them.
 * 72 kB becomes 30, and over the wire - GitHub Pages gzips - 21.6 kB becomes
 * 6.5. That is on the critical path of every single page, because a
 * stylesheet blocks the first paint by design.
 *
 * What is lost is reading the borrowed stylesheets at their published url,
 * and they are not this site's to explain: the playground publishes its own
 * readable copies beside the pages they belong to, which is where the
 * reasoning is maintained. This repository keeps its own layer in
 * scripts/site-css/docs.css, in full.
 *
 * ONE ADAPTATION TO THE BORROWED FILE, and it is about DEPTH, not taste.
 * catalogue.css names the type as `../fonts/inter-…woff2`, which is right
 * where it lives: one directory down, in `dist/samples/`, beside
 * `dist/fonts/`. Here it sits at the root of this deployment, so `../fonts/`
 * would leave the site altogether - `/fonts/` on the shared origin is
 * nobody's. The same two files are in this repository under
 * docs/public/fonts and land beside it, so the `..` goes and nothing else
 * changes. */
{
  const catalogue = frame.files['catalogue.css'];
  if (!catalogue.includes('../fonts/')) throw new Error('catalogue.css no longer names ../fonts/ - check what the type is now');
  const joined = [
    catalogue.replace(/\.\.\/fonts\//g, 'fonts/'),
    frame.files['sample.css'],
    fs.readFileSync(path.join(ROOT, 'scripts', 'site-css', 'docs.css'), 'utf8'),
  ].join('\n');
  const { code, warnings } = await transform(joined, { loader: 'css', minify: true });
  for (const w of warnings) console.warn(`site.css: ${w.text}`);
  fs.writeFileSync(path.join(OUT, 'docs', 'site.css'), code);
}
/* THE BEHAVIOUR A PAGE HAS BEYOND ITS MARKUP, as one file.
 *
 * It was already framework-free in the theme - the Run button, the line
 * numbers and their addresses, the link to a selection, the position memory
 * between the four sites. `index.js` was the only Vue in front of them, and
 * the switch to this build simply copied the five modules out beside site.js.
 *
 * Copied, they cost six requests in a three-deep waterfall for 49 kB: the
 * browser cannot know that site.js imports four files until site.js has
 * arrived and been parsed, nor that one of those imports a fifth. Two round
 * trips before anything is wired, on a connection where a round trip is the
 * expensive part. Bundled and minified it is one request of about 20 kB.
 *
 * The SOURCE stays five readable modules with their reasoning in them - this
 * is a build step, not a rewrite - and the theme still imports the same files,
 * so VitePress's build goes on being the second opinion it is.
 *
 * The modules live in the theme and site.js in scripts/, so `./code-lines.js`
 * does not resolve from the importer's own directory; the copy that used to
 * put them side by side is what made it work at runtime. The resolver below
 * is that copy, done at build time and only for a name the theme actually
 * has. */
const THEME = path.join(DOCS, '.vitepress', 'theme');
await bundle({
  entryPoints: [path.join(ROOT, 'scripts', 'site-js', 'site.js')],
  outfile: path.join(OUT, 'docs', 'site.js'),
  bundle: true,
  format: 'esm',
  target: 'es2022',
  minify: true,
  legalComments: 'none',
  logLevel: 'warning',
  plugins: [{
    name: 'the theme’s modules',
    setup(build) {
      build.onResolve({ filter: /^\.\/[\w-]+\.js$/ }, ({ path: name, importer }) => {
        const beside = path.join(path.dirname(importer), name);
        if (fs.existsSync(beside)) return null;
        const inTheme = path.join(THEME, name.slice(2));
        if (fs.existsSync(inTheme)) return { path: inTheme };
        throw new Error(`${importer} imports ${name}, which is neither beside it nor in the theme`);
      });
    },
  }],
});

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
let checked = 0, anchors = 0;
/* Every id each built page carries, so that the fragment half of a link can be
 * checked as well as the path half. A `#section` that names a heading which
 * has been renamed is a link that RESOLVES - the page opens - and then does
 * nothing: the reader lands at the top and has to find the section by eye, and
 * nothing anywhere says the link is stale. It is the failure a manual's
 * cross-references decay into, and it was the half this sweep did not look
 * at. */
const idsOf = new Map();
const ids = (file) => {
  if (!idsOf.has(file)) {
    const html = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : '';
    idsOf.set(file, new Set([...html.matchAll(/\bid="([^"]+)"/g)].map((m) => m[1])));
  }
  return idsOf.get(file);
};
const resolve = (to) => {
  const target = path.join(OUT, to);
  if (fs.existsSync(target) && fs.statSync(target).isFile()) return target;
  if (fs.existsSync(path.join(target, 'index.html'))) return path.join(target, 'index.html');
  if (fs.existsSync(`${target}.html`)) return `${target}.html`;
  return null;
};
(function sweep(dir) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const at = path.join(dir, e.name);
    if (e.isDirectory()) { sweep(at); continue; }
    if (!e.name.endsWith('.html')) continue;
    const html = fs.readFileSync(at, 'utf8');
    /* A link INSIDE the page - `href="#a-section"` - is checked against this
       page's own ids. It is where a stale anchor is likeliest: a heading is
       renamed and the sentence pointing at it three screens up is not. */
    for (const m of html.matchAll(/href="#([^"]+)"/g)) {
      const fragment = decodeURIComponent(m[1]);
      checked++; anchors++;
      if (fragment === 'top' || fragment.startsWith(':~:')) continue;
      if (!ids(at).has(fragment)) dead.push(`${at.slice(OUT.length)} -> #${m[1]}  (no section by that name on this page)`);
    }
    /* An absolute url into THIS deployment is an internal link written the
       long way - the head is full of them, because a canonical and an
       `og:` value have to be absolute - and the sweep only ever read the
       root-relative ones. The markdown twin above was advertised on two
       pages that have none, and nothing said so. */
    for (const m of html.matchAll(/(?:href|src)="(?:https:\/\/abap2ui5\.github\.io)?(\/[^"]*)"/g)) {
      const [pathPart, fragment] = m[1].split('?')[0].split('#');
      /* ...but only what is INSIDE this deployment. The bar leads to three
         neighbours on the same origin - /playground/, /playground/samples/,
         /linter/ - and those are somebody else's artefact: this build has no
         file to check them against, which is what `check:cross-site` and the
         published site are for. */
      if (!pathPart || !pathPart.startsWith(BASE.slice(0, -1))) continue;
      checked++;
      const target = resolve(pathPart);
      if (!target) { dead.push(`${at.slice(OUT.length)} -> ${m[1]}`); continue; }
      if (!fragment || !target.endsWith('.html')) continue;
      anchors++;
      /* `#top` is the browser's own, and a text fragment (`#:~:text=…`) names
         words rather than an element. */
      if (fragment === 'top' || fragment.startsWith(':~:')) continue;
      if (!ids(target).has(decodeURIComponent(fragment)))
        dead.push(`${at.slice(OUT.length)} -> ${m[1]}  (the page is there, that section is not)`);
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
console.log(`   ${checked} internal links (${anchors} of them naming a section), none of them dead — ${seconds}s`);
console.log(`   ${OUT}/docs/`);
console.log(`   frame from ${frame.from}`);
