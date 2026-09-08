#!/usr/bin/env node
/* check:images - every image under docs/public, against three things a page
 * can afford and one it cannot.
 *
 * A chapter's screenshots were PNGs of 200 to 335 kB each - 2.9 MB of them
 * across the manual, on pages whose text is 20 kB. The same captures as WebP
 * are a fifth of that with no visible difference, so a screenshot is WebP
 * here, and this is what says so the next time somebody drags a PNG in. The
 * exceptions are the files the logo page hands OUT - the mark, the wordmark
 * on white, the code cover, the mascots, the preview card - which are PNG
 * because that is what a reader downloads them as, and the icons, which the
 * browser wants as PNG. Those are listed, and anything not listed is a
 * screenshot.
 *
 * Then a budget per file, in the playground's spirit (tools/check-size.mjs
 * over there): the measured sizes with a little room above them, so a bigger
 * capture is a question and not a surprise.
 *
 * And the one thing a page cannot afford at all: an image the build cannot
 * measure. build-site.mjs writes width and height onto every <img> so the box
 * is reserved before the file lands; a file it cannot read the size of gets
 * no box and moves the page when it arrives. Same reader as the build
 * (scripts/lib/images.mjs), so a format the build cannot size fails here
 * rather than shifting the page there.
 *
 *   npm run check:images
 */
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { measureImage } from './lib/images.mjs';

const ROOT = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const PUBLIC = path.join(ROOT, 'docs', 'public');

/* The files the site hands out as PNG, and the icons. A path under public/,
   forward slashes. */
const DELIVERABLE = [
  /^favicon\.(?:png|ico)$/, /^apple-touch-icon\.png$/, /^og-image\.png$/,
  /^logo\.png$/, /^logo_white_back\.png$/, /^code_cover\.png$/, /^mascots\//,
];
/* Content is a screenshot or the hero mark: WebP, and never more than this.
   The largest today is 62 kB. A deliverable is a PNG somebody downloads, and
   the mark at 1064px is 197 kB. */
const LIMIT = { content: 120 * 1024, deliverable: 220 * 1024 };
const IMAGE = /\.(?:png|jpe?g|gif|webp|avif|svg|ico)$/i;

const walk = (dir) => fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
  const at = path.join(dir, e.name);
  return e.isDirectory() ? walk(at) : IMAGE.test(e.name) ? [at] : [];
});

const kB = (n) => `${(n / 1024).toFixed(0)} kB`;
const problems = [];
let content = 0, deliverables = 0, largest = { size: 0, rel: '' };
const files = walk(PUBLIC);
for (const file of files) {
  const rel = path.relative(PUBLIC, file).split(path.sep).join('/');
  const size = fs.statSync(file).size;
  const deliverable = DELIVERABLE.some((re) => re.test(rel));
  deliverable ? deliverables++ : content++;
  if (size > largest.size) largest = { size, rel };
  if (!deliverable && !/\.(?:webp|avif|svg)$/i.test(rel)) {
    problems.push(`${rel}: a screenshot as ${path.extname(rel).slice(1).toUpperCase()} - convert it to WebP (sharp, cwebp) and point the page at the .webp, or list it in check-images.mjs if it is a file the site hands out`);
  }
  const limit = deliverable ? LIMIT.deliverable : LIMIT.content;
  if (size > limit) problems.push(`${rel}: ${kB(size)} is over the ${kB(limit)} a ${deliverable ? 'deliverable' : 'screenshot'} may weigh`);
  if (!/\.ico$/i.test(rel)) {
    const box = measureImage(file);
    if (!box || !box.w || !box.h) problems.push(`${rel}: the build cannot measure it, so no page can reserve its box before it arrives`);
  }
}

/* The floor every walking gate carries: a gate that found nothing reports the
   same shape as a gate that found nothing wrong. */
if (files.length === 0) {
  console.error(`check-images: no image under ${path.relative(ROOT, PUBLIC)} at all - the walk or the extensions are wrong`);
  process.exit(1);
}

console.log(`check-images: ${files.length} image(s) under docs/public - ${content} screenshot(s) and marks, ${deliverables} deliverable(s); the largest is ${largest.rel} at ${kB(largest.size)}`);
if (problems.length) {
  console.error(`             ${problems.length} problem(s):`);
  for (const p of problems) console.error(`               ${p}`);
  process.exit(1);
}
console.log('             every screenshot is WebP, nothing is over its budget, and the build can measure all of them.');
