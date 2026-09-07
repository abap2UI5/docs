/*
 * The sentence a page is described BY — `summarise( )` in scripts/lib/pages.mjs.
 *
 * It is the meta description of 150 of the 166 pages (the rest declare one in
 * their frontmatter), the note beside every entry in llms.txt, and the line
 * under a search hit. Nobody writes it, so nobody reads it before it ships:
 * the failures are all quiet ones, and they were all real. It used to stop at
 * the FIRST full stop, which on thirty-four pages was a premise rather than a
 * description — "abap2UI5 has no e-mail control of its own" — and it cut at a
 * dash wherever a dash came first, which is right for an opening sentence and
 * wrong four words into a bracket.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';

import { summarise, describe } from '../scripts/lib/pages.mjs';

test('a page says what it declares, whatever its first paragraph is', () => {
  const body = '---\ndescription: The one sentence this page chose\n---\n\n# Title\n\nSomething else entirely.';
  assert.equal(describe(body), 'The one sentence this page chose');
});

test('one sentence that already says something is left alone', () => {
  const one = 'The abap2UI5 HTTP Connector calls abap2UI5 apps remotely over HTTP between two ABAP systems, '
    + 'which is what makes one system a frontend for another.';
  assert.ok(one.length >= 110, 'the fixture is long enough to stand on its own');
  assert.equal(summarise(`# T\n\n${one} It works similarly to the RFC Connector.`), one);
});

test('a first sentence too short to describe anything takes the next ones', () => {
  const out = summarise('# T\n\nAll examples in these docs work without EML. But on a recent ABAP release you can '
    + 'also use this feature in your own apps. A third sentence nobody needs.');
  assert.equal(out, 'All examples in these docs work without EML. But on a recent ABAP release you can also use '
    + 'this feature in your own apps.');
});

test('a paragraph with nothing more in it stays as short as it is', () => {
  assert.equal(summarise('# T\n\nBarcode scanning is common in enterprise apps.'),
    'Barcode scanning is common in enterprise apps.');
});

test('a dash ends a first sentence and never extends one', () => {
  // The dash forms exist because a page can open with a sentence that never
  // reaches a full stop - so a dash is where such a page is cut...
  const opens = 'On SAP HANA you can match strings tolerantly — typos, missing letters, transposed '
    + 'characters — with the CONTAINS function in fuzzy mode, and no exact match at all';
  assert.equal(summarise(`# T\n\n${opens}`), 'On SAP HANA you can match strings tolerantly');

  // ...and never where one is extended, which stops mid-clause: the sentence
  // that follows is taken whole or not at all.
  const extended = summarise('# T\n\nAn AI coding agent asked to write an abap2UI5 app can write ABAP. '
    + 'What it cannot do is find out whether the app works — that has always needed a system.');
  assert.match(extended, /needed a system\.$/);
});

test('a cut that leaves a bracket open is not a cut', () => {
  const out = summarise('# T\n\nPull abap2UI5 with abapGit. (New to abapGit? Install it first — see abapGit; it is the one-time tool.) Then pull a release rather than main.');
  assert.ok(!out.endsWith('Install it first'), 'the aside is not left half-open');
  assert.equal((out.match(/\(/g) || []).length, (out.match(/\)/g) || []).length);
});

test('a version number is not the end of a sentence', () => {
  const one = 'abap2UI5 runs on 7.02 and later, needs nothing installed on the frontend at all, and asks '
    + 'the system for no service of its own.';
  assert.equal(summarise(`# T\n\n${one} Which is the point of it.`), one);
});

test('a paragraph without a full stop in it is still cut to a length', () => {
  const long = `# T\n\n${'word '.repeat(80)}`;
  const out = summarise(long);
  assert.ok(out.length <= 220, `${out.length} characters`);
  assert.match(out, /\.\.\.$/);
});

test('code fences, headings and tables are not the description', () => {
  const body = '# T\n\n```abap\nDATA(lo_x) = NEW cl_y( ).\n```\n\n| a | b |\n\nThe real opening sentence of the page, which is what a reader wants.';
  assert.equal(summarise(body), 'The real opening sentence of the page, which is what a reader wants.');
});
