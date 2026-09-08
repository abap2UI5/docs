/*
 * What every page loses on its way out, and what a row of the menu is.
 *
 * The sources explain themselves in comments, and in a page a comment is
 * bytes: 418 kB across the site, a tenth of every page's compressed weight,
 * read by nobody. stripComments (scripts/lib/html.mjs) takes them off the
 * finished page - outside <script>, because a script's text is what its hash
 * in the policy is taken from, and a "<!--" in JavaScript is JavaScript.
 *
 * The rest is the build read as text: the sidebar's row is the link and not a
 * box around it, the hero mark asks to be fetched first, and the search box
 * rides in site.js rather than as a second module on every page.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { stripComments } from '../scripts/lib/html.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

test('a comment outside a script goes, the doctype and the markup stay', () => {
  assert.equal(stripComments('<!doctype html><!-- one --><p>a<!--\n two\n--></p>'), '<!doctype html><p>a</p>');
});

test('a script leaves byte for byte, whatever it contains', () => {
  const script = '<script>var a = "<!-- not a comment -->"; /* <!-- --> */</script>';
  assert.equal(stripComments(`<!-- x -->${script}<!-- y -->`), script);
  const ld = '<script type="application/ld+json">{"a":"<!-- -->"}</script>';
  assert.equal(stripComments(ld), ld);
});

test('an escaped comment in a listing is text and is left alone', () => {
  const listing = '<pre><code>&lt;!-- a comment in an XML view --&gt;</code></pre>';
  assert.equal(stripComments(listing), listing);
});

/* ---- the build, as written ---------------------------------------------- */
const BUILD = readFileSync(join(ROOT, 'scripts/build-site.mjs'), 'utf8');
const CSS = readFileSync(join(ROOT, 'scripts/site-css/docs.css'), 'utf8');

test('the page is stripped before its scripts are read back for the policy', () => {
  assert.match(BUILD, /const page = stripComments\(raw\);\s*for \(const script of inlineScriptsIn\(page\)\)/);
});

test('a row of the menu is the link, and the stylesheet addresses it as one', () => {
  assert.match(BUILD, /<a class="side-item level-\$\{level\}" href=/);
  assert.match(BUILD, /<span class="side-item level-\$\{level\}">/);
  assert.doesNotMatch(BUILD, /<div class="side-item\b(?!s)/, 'the box around the link is what cost half a page');
  assert.doesNotMatch(CSS, /\.side-item > (?:a|span)\b/, 'a rule that still reaches into the box matches nothing now');
  assert.match(CSS, /\.sidebar a\.side-item,\s*\.sidebar span\.side-item,\s*\.sidebar summary \{/);
});

test('the hero mark is fetched first, and the search box rides in site.js', () => {
  assert.match(BUILD, /decoding="async" fetchpriority="high"><\/div>`/);
  assert.doesNotMatch(BUILD, /<script type="module" src="\$\{BASE\}search\.mjs">/, 'one module script per page, not two');
  assert.match(BUILD, /fs\.writeFileSync\(path\.join\(scratch, 'search\.mjs'\), frame\.files\['search\.mjs'\]\);/);
  assert.match(BUILD, /entryPoints: \[path\.join\(scratch, 'entry\.mjs'\)\]/);
});
