#!/usr/bin/env node
// Can a reader actually get from this site to the ones next to it?
//
// The three sites share an origin, and a same-origin link that looks like a
// page is taken over by VitePress's router — which then cannot find a page of
// THIS site behind /playground/ and renders the 404 instead. Every link out of
// the manual and into a neighbouring deployment was broken that way at once:
// both bar items, the Linter rules row in the menu, and the Run bar's link
// into the playground. The whole reasoning is at the top of
// scripts/lib/cross-site.mjs.
//
// The fix is one attribute — `target="_self"`, which the router honours as an
// opt-out and which is also the one-tab behaviour the four bars promise — and
// an attribute is exactly the kind of thing that gets left off the next link
// somebody adds. So it is decided here, against the BUILT site: what ships is
// what a reader clicks.
//
// Usage: node scripts/check-cross-site.mjs [--list]
//   Needs docs/.vitepress/dist, so run it after `npm run docs:build` - see the
//   note on DIST below for why it is that build and not the published one.
//   --list prints every cross-site link it found, by destination.

import { readFileSync, readdirSync, statSync, existsSync } from 'node:fs';
import { join, dirname, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { crossSiteLinks, SITE, unreachable } from './lib/cross-site.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
/* WHICH BUILD THIS JUDGES, and why it is still VitePress's.
 *
 * What this gate is about is one specific failure: VitePress's ROUTER taking
 * over a same-origin link that looks like a page of this site. The published
 * site is written by scripts/build-site.mjs now and has no router - every link
 * on it is an ordinary navigation - so the failure cannot happen there, and
 * pointing this at that build would only have it object to the bar, which is
 * the catalogue's markup and correct as it stands.
 *
 * Both builds render the same markdown, so judging VitePress's output still
 * judges every link WE write, which is what the gate is for. It stops being
 * meaningful on the day the VitePress build goes; that is the day to decide
 * what replaces it, not before. `SITE_OUT` points it at another build. */
const DIST = process.env.SITE_OUT
  ? join(ROOT, process.env.SITE_OUT)
  : join(ROOT, 'docs/.vitepress/dist');
const LIST = process.argv.includes('--list');

if (!existsSync(DIST)) {
  console.error(`check-cross-site: no built site at ${relative(ROOT, DIST)}.`);
  console.error('This gate reads what ships, not the sources. Run `npm run docs:build` first.');
  process.exit(1);
}

const walk = (dir) =>
  readdirSync(dir).flatMap((e) => {
    const p = join(dir, e);
    return statSync(p).isDirectory() ? walk(p) : [p];
  });

const pages = walk(DIST).filter((f) => f.endsWith('.html')).sort();
const broken = [];
const destinations = new Map();

for (const file of pages) {
  const page = relative(DIST, file);
  /* The page's own address, because a relative href on it resolves against
   * THAT and not against the site root. */
  const from = SITE.origin + SITE.base + page;
  for (const link of crossSiteLinks(readFileSync(file, 'utf8'), from)) {
    destinations.set(link.url, (destinations.get(link.url) ?? 0) + 1);
    if (!link.exempt) broken.push({ page, ...link });
  }
}

/* Both floors. A gate that walked nothing and a gate that found nothing wrong
 * print the same line, and this site has been burned by that once already —
 * check:examples ran no rules at all for years and reported `0 issue(s)`. The
 * bar is on every page of the site, so "no cross-site link anywhere" is not a
 * clean site, it is a broken walk. */
if (pages.length === 0) {
  console.error(`check-cross-site: ${relative(ROOT, DIST)} holds no .html at all — did the build fail?`);
  process.exit(1);
}
if (destinations.size === 0) {
  console.error(`check-cross-site: walked ${pages.length} page(s) and found no link to a neighbouring site.`);
  console.error('The bar carries three of them on every page, so this is the walk failing, not the');
  console.error('site being clean — has the bar moved out of the HTML, or the origin changed?');
  process.exit(1);
}

const total = [...destinations.values()].reduce((a, b) => a + b, 0);
console.log(
  `check-cross-site: ${total} link(s) out of ${pages.length} page(s) into `
  + `${destinations.size} neighbouring destination(s) on ${SITE.origin}`,
);

if (LIST) {
  for (const [url, count] of [...destinations].sort()) console.log(`  ${String(count).padStart(5)}  ${url}`);
}

/* WHERE THEY GO, not only whether they work. The line this gate ends on has
 * always said "All of them lead somewhere" and never checked it: three of the
 * destinations - /samples/, /samples-controls/, /samples-stack/ - had been
 * retired and replaced by the one catalogue under /playground/samples/, and
 * thirteen links across seven pages went on naming them. An internal link that
 * dies fails the build; a link to the site next door just quietly stops being
 * true. scripts/lib/cross-site.mjs holds the list. */
const gone = [];
for (const [url, count] of destinations) {
  const why = unreachable(new URL(url).pathname);
  if (why) gone.push({ url, count, why });
}
if (gone.length) {
  console.error(`\n${gone.length} cross-site destination(s) are not deployments of this origin:\n`);
  for (const one of gone.sort((a, b) => b.count - a.count)) {
    console.error(`  - ${one.url}`);
    console.error(`      ${one.count} link(s) — ${one.why}`);
  }
  console.error('\nIf one of these IS a deployment now, name it in NEIGHBOURS in');
  console.error('scripts/lib/cross-site.mjs - one line, and a decision somebody makes rather');
  console.error('than a URL nobody rereads.');
  process.exit(1);
}

if (broken.length) {
  const byHref = new Map();
  for (const one of broken) {
    if (!byHref.has(one.href)) byHref.set(one.href, []);
    byHref.get(one.href).push(one.page);
  }
  console.error(`\n${broken.length} link(s) into a neighbouring site carry no \`target\`:\n`);
  for (const [href, where] of byHref) {
    console.error(`  - ${href}`);
    console.error(`      on ${where.length} page(s), e.g. ${where[0]}`);
  }
  console.error('\nA same-origin link that looks like a page is taken over by VitePress\'s router,');
  console.error('which has no page of THIS site to render at that address and shows the 404 —');
  console.error('at the neighbour\'s URL, so it reads as the other site being broken.');
  console.error('\nAdd the attribute where the link is written:');
  console.error('\n  <a href="https://abap2ui5.github.io/playground/" target="_self">Playground</a>');
  console.error('\n`_self` because these sites are one site and open in one tab; any `target` opts');
  console.error('the link out of the router. scripts/lib/cross-site.mjs says why in full.');
  process.exit(1);
}

console.log('every link into a neighbouring deployment opts out of the router. All of them lead somewhere.');
