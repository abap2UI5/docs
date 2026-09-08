/*
 * The policy every page is published under (scripts/lib/csp.mjs), and the
 * build's use of it.
 *
 * Three deployments share one origin, and one of them runs whatever ABAP a
 * shared link carries. A page of the manual runs nothing of the kind and says
 * so with a Content-Security-Policy - a <meta>, because GitHub Pages sets no
 * headers of ours. What has to hold: the two inline scripts the build writes
 * are allowed by HASH and nothing inline is allowed otherwise, the hash is the
 * one CSP spells, a data block is not a script, and the build refuses a page
 * carrying an inline script the policy would kill rather than publishing a
 * page whose switch is silently dead.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { contentSecurityPolicy, hashOf, inlineScriptsIn, NEIGHBOUR } from '../scripts/lib/csp.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

test('an inline script is named the way CSP names it: sha256, base64, quoted', () => {
  assert.equal(hashOf('alert(1)'), "'sha256-bhHHL3z2vDgxUt0W3dWQOrprscmda2Y5pLsLg4GF+pI='");
  assert.notEqual(hashOf('alert(1)'), hashOf('alert(1) '), 'a byte is a different script');
});

test('only the hashed scripts are allowed inline, and nothing else is', () => {
  const policy = contentSecurityPolicy(['a', 'b']);
  const script = policy.split('; ').find((d) => d.startsWith('script-src '));
  assert.ok(script.includes(hashOf('a')) && script.includes(hashOf('b')));
  assert.ok(!script.includes('unsafe-inline'), 'the whole point');
  assert.ok(!script.includes('unsafe-eval'));
  assert.ok(script.includes("'self'") && script.includes(NEIGHBOUR), 'this origin, and the playground for the Run panel');
});

test('the rest of the policy: no objects, no foreign base, styles inline for the highlighter', () => {
  const policy = contentSecurityPolicy([]);
  for (const d of ["default-src 'self'", "object-src 'none'", "base-uri 'self'", "form-action 'self'",
    "style-src 'self' 'unsafe-inline'", "img-src 'self' data:", "font-src 'self'",
    `frame-src 'self' ${NEIGHBOUR}`, `connect-src 'self' ${NEIGHBOUR}`]) {
    assert.ok(policy.includes(d), d);
  }
  assert.ok(!policy.includes('frame-ancestors'), 'a <meta> policy cannot carry it, and a browser would warn');
});

test('the inline scripts of a page are the ones the browser would run', () => {
  const page = '<script src="x.js"></script><script type="application/ld+json">{"a":1}</script>'
    + '<script>one</script><script type="module">two</script>';
  assert.deepEqual(inlineScriptsIn(page), ['one', 'two'], 'a src is not inline and a data block is not run');
});

/* ---- the build, as written ---------------------------------------------- */
const BUILD = readFileSync(join(ROOT, 'scripts/build-site.mjs'), 'utf8');

test('the build hashes the very strings it writes, and writes the policy before them', () => {
  assert.match(BUILD, /const INLINE = \[THEME_SCRIPT, MENU_SCRIPT_BODY\];/);
  assert.match(BUILD, /const allowed = \[\.\.\.INLINE, \.\.\.inline\];\s*const csp = contentSecurityPolicy\(allowed\);/,
    'the two scripts every page carries, plus what this page brings - and the policy is made of exactly those');
  assert.match(BUILD, /<script>\${THEME_SCRIPT}<\/script>/, 'the theme line is written from the same constant that is hashed');
  const meta = BUILD.indexOf('<meta http-equiv="Content-Security-Policy" content="${csp}">');
  const theme = BUILD.indexOf('<script>${THEME_SCRIPT}</script>');
  assert.ok(meta > 0 && theme > meta, 'the policy has to be parsed before the first script it governs');
});

test('a page carrying an inline script the policy would kill is refused, not published', () => {
  assert.match(BUILD, /const shell = \(\{ title, main, bar, head = '', inline = \[\] \}\) => \{/);
  assert.match(BUILD, /return announced\(`<!doctype html>/);
  assert.match(BUILD, /for \(const script of inlineScriptsIn\(page\)\) \{\s*if \(!allowed\.includes\(script\)\) throw new Error/);
  /* The one page that brings a script of its own says so from the same
     string it writes, so the hash and the script cannot drift apart. */
  assert.match(BUILD, /<script>\$\{NOT_FOUND_SCRIPT\}<\/script>/);
  assert.match(BUILD, /inline: \[NOT_FOUND_SCRIPT\],/);
});
