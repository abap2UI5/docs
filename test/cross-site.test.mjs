/*
 * Which links out of this site the gate must object to.
 *
 * scripts/check-cross-site.mjs walks the built HTML and demands a `target` on
 * every link that leaves this deployment for a neighbouring one on the same
 * origin — because without it VitePress's router takes the link over, finds no
 * page of this site behind /playground/ and renders the 404 there. The rule
 * has to be exactly as narrow as the router's own: too wide and it demands an
 * attribute on links that work perfectly (another host, a JSON file); too
 * narrow and the bar breaks again in silence.
 *
 * That boundary is what is pinned here, plus the one link the gate CANNOT see:
 * the Run bar's "Switch to Playground with this code" is created in a browser,
 * from a URL the playground's loader returns, and appears in no built page.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { leavesTheSite, crossSiteLinks } from '../scripts/lib/cross-site.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

test('a link into a neighbouring deployment is the case the router gets wrong', () => {
  for (const href of [
    'https://abap2ui5.github.io/playground/',
    'https://abap2ui5.github.io/playground/samples/',
    'https://abap2ui5.github.io/playground/samples/?q=table&lib=sap.m',
    'https://abap2ui5.github.io/playground/#src=CLASS%20zcl',
    'https://abap2ui5.github.io/linter/',
    'https://abap2ui5.github.io/samples/',
    /* Written root-relative rather than absolute: the same link, and the
     * router does not care which form it was written in. */
    '/playground/',
    '/linter/rules.html',
  ]) {
    assert.ok(leavesTheSite(href), `${href} leaves the site and needs a target`);
  }
});

test('a page of this site is the router\'s own job', () => {
  for (const href of [
    '/docs/',
    '/docs/resources/changelog.html',
    'https://abap2ui5.github.io/docs/get_started/quickstart.html',
    '#a-heading-on-this-page',
    'cookbook/view/definition.html',
  ]) {
    assert.equal(leavesTheSite(href), null, `${href} is a page of this site`);
  }
});

test('a relative link is resolved against the page it is written on', () => {
  /* The cookbook links its neighbours as `./../model/x.html`. Against the site
   * ROOT that reads as a link out of the manual, and a gate that resolved it
   * that way objected to a dozen perfectly ordinary in-site links. */
  const page = 'https://abap2ui5.github.io/docs/cookbook/device_capabilities/info.html';
  assert.equal(leavesTheSite('./../model/device_model.html', page), null);
  assert.equal(leavesTheSite('./../../configuration/ui5_versions.html', page), null);
  /* Far enough up and it really does leave: two more levels and this is
   * /playground/, not a page of the manual. */
  assert.ok(leavesTheSite('./../../../playground/', page));
});

test('another host is never touched by the router, whatever it points at', () => {
  for (const href of [
    'https://github.com/abap2UI5/abap2UI5',
    'https://www.linkedin.com/company/abap2ui5/',
    'http://localhost:5173/playground/',
    'mailto:someone@example.org',
    'javascript:void 0',
  ]) {
    assert.equal(leavesTheSite(href), null, `${href} is not this origin`);
  }
});

test('a file the router would not treat as a page needs no attribute', () => {
  /* The extension list is VitePress's, copied. Demanding the attribute here
   * would be a rule that cannot be justified from what the router does. */
  for (const href of [
    'https://abap2ui5.github.io/playground/samples/catalogue.json',
    'https://abap2ui5.github.io/linter/rules.txt',
    'https://abap2ui5.github.io/playground/embed/abap2ui5-embed.js',
  ]) {
    assert.equal(leavesTheSite(href), null, `${href} is not a page`);
  }
  assert.ok(leavesTheSite('https://abap2ui5.github.io/playground/index.html'), '.html is a page');
});

test('the scan reads the attributes off the tag, quotes and all', () => {
  const html = `
    <a href="https://abap2ui5.github.io/playground/">Playground</a>
    <a href="https://abap2ui5.github.io/linter/" target="_self">Linter rules</a>
    <a href="https://abap2ui5.github.io/playground/samples/" title="a > b in a title">Samples</a>
    <a href="/docs/resources/api.html">API</a>
    <a href="https://github.com/abap2UI5/docs" target="_blank" rel="noopener">docs</a>
  `;
  const found = crossSiteLinks(html);
  assert.equal(found.length, 3, 'three links leave the site; the docs page and github.com do not');
  assert.deepEqual(found.map((f) => f.exempt), [false, true, false]);
});

test('the Run bar\'s link into the playground carries the attribute too', () => {
  /* The gate cannot reach this one: it is built in the browser, out of a URL
   * the playground's loader hands back, so it is in no built page. It is the
   * same link with the same fault - it was the fourth way out of the manual
   * that led to this site's 404 - so it is checked here, as the text that
   * creates it. */
  const source = readFileSync(join(ROOT, 'docs/.vitepress/theme/playground.js'), 'utf8');
  const bar = source.slice(source.indexOf("open.className = 'a2ui5-play-open'"));
  assert.ok(bar, "the Run bar still builds a link with the class 'a2ui5-play-open'");
  const untilAppended = bar.slice(0, bar.indexOf('bar.append('));
  assert.match(
    untilAppended,
    /open\.target\s*=\s*'_self'/,
    'the link into the playground must opt out of the router, or it lands on this site\'s 404',
  );
});
