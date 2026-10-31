/*
 * audience — is this page still written for a developer?
 *
 * The manual is for the person who builds with abap2UI5. The case for the
 * person who approves it - what it costs, how it compares, who runs it - is
 * a site of its own (https://abap2ui5.github.io/why/), and AGENTS.md says to
 * link it and never argue it here a second time. The pages did argue it,
 * for years and in passing: a sentence about licensing in a cookbook
 * chapter, a paragraph on the business case in a configuration page. Each
 * one was reasonable where it stood and together they were a second,
 * unmaintained copy of the other site.
 *
 * What is decidable about that is the vocabulary. A developer page has no
 * reason to say "license fee", "per-user" or "decision maker"; the handful
 * of phrases below are the words of the other conversation, and a page
 * that uses one has crossed over. Four pages may: the license, support and
 * sponsoring pages, whose subject the words are, and the "In a Nutshell"
 * page, which carries the one sentence that hands the reader over.
 *
 * The prose is scripts/lib/prose.mjs's, so a phrase in a code fence, a
 * link target or a comment is not a hit. Whole words, any case, a plural
 * allowed - "subscriptions", "SLAs", "decision makers".
 *
 * Pinned by test/audience.test.mjs.
 */
import { proseLines } from './prose.mjs';

/** The phrases, as written on the other site. */
export const PHRASES = [
  'license fee',
  'per-user fee',
  'per-user price',
  'per user fee',
  'per user price',
  'subscription',
  'ROI',
  'total cost',
  'vendor lock',
  'SLA',
  'decision maker',
  'IT management',
  'business case',
];

/** The pages that may use them, relative to docs/. */
export const ALLOWED = new Set([
  'resources/license.md',
  'resources/support.md',
  'resources/sponsor.md',
  'get_started/about.md',
]);

const escape = (s) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
const PATTERN = new RegExp(`\\b(?:${PHRASES.map(escape).join('|')})s?\\b`, 'gi');

/** Every hit in the prose of `markdown`: `[{ line, col, phrase }]`, 1-based. */
export function findBusinessCase(markdown) {
  const out = [];
  proseLines(markdown).forEach((line, i) => {
    for (const m of line.matchAll(PATTERN)) out.push({ line: i + 1, col: m.index + 1, phrase: m[0] });
  });
  return out;
}
