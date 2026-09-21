/* Is this a WORD - as opposed to which spelling of it.
 *
 * test/spelling.test.mjs answers the second question: it names British forms
 * from a list and rewrites them. What it cannot do is notice a word that is
 * neither spelling of anything. "thant", "havte", "fullfll" and
 * "requoirememnts" all went in through the browser editor, through CI and out
 * to the published site in two days, and one of them stood in the first
 * sentence of a page - which is also what the search index, llms.txt and the
 * card subtitle take their description from, so it read wrong in four places.
 *
 * So: every prose word is looked up in a dictionary (dictionary-en, the
 * Hunspell en_US), and what a dictionary cannot know about this project is
 * committed beside it in vocabulary.txt.
 *
 * The prose comes from scripts/lib/prose.mjs, which already blanks fenced
 * code, inline code, link targets, HTML, URLs, the generated blocks and the
 * configuration half of the frontmatter - so an ABAP keyword, a CSS property
 * or a path is never a word here.
 */

import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import nspell from 'nspell';
import en from 'dictionary-en';
import { proseLines } from './prose.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
export const WORDS_FILE = join(HERE, 'vocabulary.txt');

/** The project's own words, one per line; `#` starts a comment. */
export function projectWords(file = WORDS_FILE) {
  return readFileSync(file, 'utf8')
    .split('\n')
    .map((l) => l.replace(/#.*$/, '').trim())
    .filter(Boolean);
}

// A token is a word and everything that may sit INSIDE one. The Unicode
// classes matter: [A-Za-z] would start a new token at the "ller" of "Müller"
// and report it.
const TOKEN = /[\p{L}][\p{L}\p{N}_'’-]*/gu;

// Fragments a hyphen leaves behind that are not words on their own.
const AFFIX = new Set([
  'un', 're', 'pre', 'non', 'co', 'de', 'mid', 'sub', 'cross', 'multi', 'semi',
  'anti', 'self', 'off', 'on', 'in', 'out', 'over', 'under', 'post', 'pro',
  'well', 'half', 'side', 'by', 'to', 'up', 'so', 'no', 'per', 'ever', 'like',
  'free', 'wide', 'less', 'ish', 'est',
]);

/**
 * Every word of `markdown` that no dictionary and no list of ours knows, as
 * {line, word}. Ordered by where it stands.
 */
export function findUnknown(markdown, { words = projectWords() } = {}) {
  const spell = nspell(en);
  const known = new Set(words.map((w) => w.toLowerCase()));
  const inDictionary = (w) => spell.correct(w) || spell.correct(w.toLowerCase());
  const ok = (w) => known.has(w.toLowerCase()) || inDictionary(w);

  const found = [];
  proseLines(markdown).forEach((line, i) => {
    // `**f**rontend` - emphasis INSIDE a word. Markdown joins it back up; a
    // tokenizer that does not would report "rontend".
    for (const m of line.replace(/\*+/g, '').matchAll(TOKEN)) {
      const token = m[0].replace(/[-'’]+$/, '');
      // An identifier, not a word: it carries a digit or an underscore
      // (`z2ui5_cl_app`, `abap2UI5`), is an acronym (`ICF`, `APIs`), or is
      // camelCase (`liveChange`). None of them is English to be spelled.
      if (/\p{N}/u.test(token) || token.includes('_')) continue;
      if (/^\p{Lu}{2,}s?$/u.test(token)) continue;
      if (/\p{Ll}\p{Lu}/u.test(token)) continue;
      if (ok(token)) continue;

      // A hyphenated compound the dictionary does not carry whole is judged
      // part by part: "design-time" is two words it knows.
      for (const part of token.split('-')) {
        const word = part.replace(/[’']s$/, '');
        if (word.length < 3 || AFFIX.has(word.toLowerCase()) || ok(word)) continue;
        found.push({ line: i + 1, word });
      }
    }
  });
  return found;
}
