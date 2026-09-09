/*
 * The one image reader (scripts/lib/images.mjs), against the files the site
 * actually carries and a header built by hand.
 *
 * build-site.mjs writes width and height onto every <img> so the page reserves
 * the box before the file lands, and check:images refuses an image the build
 * could not size. Both read through this, so a format it cannot read fails a
 * gate rather than shifting a page - which is what the WebP captures did on
 * the day they replaced the PNGs, before the reader knew the RIFF container.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { writeFileSync, mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { measureImage } from '../scripts/lib/images.mjs';

const PUBLIC = join(dirname(fileURLToPath(import.meta.url)), '..', 'docs', 'public');

test('a lossy WebP - the screenshots - is read from its VP8 frame header', () => {
  assert.deepEqual(measureImage(join(PUBLIC, 'get_started/image.webp')), { w: 1600, h: 765 });
  assert.deepEqual(measureImage(join(PUBLIC, 'configuration/debug.webp')), { w: 1600, h: 843 });
});

test('a lossless WebP - the hero mark - is read from its VP8L bits', () => {
  assert.deepEqual(measureImage(join(PUBLIC, 'logo-hero.webp')), { w: 400, h: 392 });
});

test('an extended WebP is read from its VP8X canvas', () => {
  /* Built by hand: RIFF, WEBP, a VP8X chunk of ten bytes - flags, reserved,
     then width-1 and height-1 as 24-bit little-endian. 1600 by 900. */
  const b = Buffer.alloc(32);
  b.write('RIFF', 0, 'latin1'); b.writeUInt32LE(24, 4); b.write('WEBP', 8, 'latin1');
  b.write('VP8X', 12, 'latin1'); b.writeUInt32LE(10, 16);
  b.writeUIntLE(1599, 24, 3); b.writeUIntLE(899, 27, 3);
  const file = join(mkdtempSync(join(tmpdir(), 'webp-')), 'x.webp');
  writeFileSync(file, b);
  assert.deepEqual(measureImage(file), { w: 1600, h: 900 });
});

test('the PNGs and the JPEG the site hands out are read as before', () => {
  assert.deepEqual(measureImage(join(PUBLIC, 'logo.png')), { w: 1064, h: 1042 });
  assert.deepEqual(measureImage(join(PUBLIC, 'og-image.png')), { w: 1200, h: 630 });
  assert.deepEqual(measureImage(join(PUBLIC, 'mascots/sloth.jpg')), { w: 400, h: 400 });
});

test('what it cannot read is null, not a guess', () => {
  const file = join(mkdtempSync(join(tmpdir(), 'notimg-')), 'x.bin');
  writeFileSync(file, Buffer.from('nothing like an image, and long enough to be looked at twice'));
  assert.equal(measureImage(file), null);
  assert.equal(measureImage(join(PUBLIC, 'does-not-exist.webp')), null);
});
