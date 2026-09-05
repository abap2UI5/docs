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
//   Needs docs/.vitepress/dist, so run it after `npm run docs:build`.
//   --list prints every cross-site link it found, by destination.

import { readFileSync, readdirSync, statSync, existsSync } from 'node:fs';
import { join, dirname, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
import { crossSiteLinks, SITE } from './lib/cross-site.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const DIST = join(ROOT, 'docs/.vitepress/dist');
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
