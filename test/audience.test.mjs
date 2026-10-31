/*
 * The audience rule: the words of the site for decision makers do not
 * appear in the prose of a developer page. What is a hit, what is not (a
 * fence, a link target, a comment), and that the plural is a hit too.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { findBusinessCase, PHRASES, ALLOWED } from '../scripts/lib/audience.mjs';

const hits = (md) => findBusinessCase(md).map((h) => `${h.line}:${h.phrase}`);

test('a phrase of the other site in prose is a hit, whole words, any case, plural included', () => {
  assert.deepEqual(hits('# T\n\nThere is no license fee and no per-user price.\n\nDecision makers read the ROI.\n'), [
    '3:license fee', '3:per-user price', '5:Decision makers', '5:ROI',
  ]);
  assert.deepEqual(hits('# T\n\nThe SLAs, the subscriptions, the total cost: less than vendor lock-in.\n'), [
    '3:SLAs', '3:subscriptions', '3:total cost', '3:vendor lock',
  ]);
});

test('a word that only contains one is not a hit', () => {
  assert.deepEqual(hits('# T\n\nA translation and an island; the display is slanted.\n'), []);
});

test('code, link targets and comments are not prose', () => {
  const md = '# T\n\n```abap\n" the ROI of a subscription\n```\n\nSee [the case](https://example.com/business-case) and `per_user`.\n\n<!-- SLA -->\n';
  assert.deepEqual(hits(md), []);
});

test('the list and the allowed pages are the ones AGENTS.md names', () => {
  assert.equal(PHRASES.length, 13);
  assert.deepEqual([...ALLOWED].sort(), ['get_started/about.md', 'resources/license.md', 'resources/sponsor.md', 'resources/support.md']);
});
