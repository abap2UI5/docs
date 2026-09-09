/* What an image IS, for the two places that need to know: the build, which
 * writes width and height onto every <img> so the page reserves the box before
 * the file arrives, and check:images, which refuses an image the build could
 * not measure - because that one moves the page when it lands.
 *
 * No library. The four formats the site carries put their size in the first
 * few dozen bytes, and reading those bytes is thirty lines against a dependency
 * that would have to be installed to build the manual. */
import fs from 'node:fs';

/** `{ w, h }` of a PNG, JPEG, WebP or SVG file, or null for anything else. */
export function measureImage(file) {
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
  /* WebP is a RIFF container, and the size sits in whichever chunk comes
     first: VP8X (extended - alpha, metadata - with a 24-bit canvas width-1 and
     height-1), VP8L (lossless: 14 bits each, packed behind a signature byte),
     or VP8 (lossy: the 14-bit width and height of the frame header). */
  if (b.length > 30 && b.toString('latin1', 0, 4) === 'RIFF' && b.toString('latin1', 8, 12) === 'WEBP') {
    const chunk = b.toString('latin1', 12, 16);
    if (chunk === 'VP8X') return { w: 1 + b.readUIntLE(24, 3), h: 1 + b.readUIntLE(27, 3) };
    if (chunk === 'VP8L' && b[20] === 0x2f) {
      const bits = b.readUInt32LE(21);
      return { w: 1 + (bits & 0x3fff), h: 1 + ((bits >>> 14) & 0x3fff) };
    }
    if (chunk === 'VP8 ' && b[23] === 0x9d && b[24] === 0x01 && b[25] === 0x2a)
      return { w: b.readUInt16LE(26) & 0x3fff, h: b.readUInt16LE(28) & 0x3fff };
    return null;
  }
  const svg = b.toString('utf8', 0, 2000);
  const box = svg.match(/viewBox="[\d.+-]+\s+[\d.+-]+\s+([\d.]+)\s+([\d.]+)"/);
  if (box) return { w: Math.round(+box[1]), h: Math.round(+box[2]) };
  return null;
}
