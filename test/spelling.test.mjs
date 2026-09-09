/*
 * The house spelling, held.
 *
 * The manual is written in American English - behavior, color, license,
 * catalog, optimize - because that is how the reader's own system spells
 * these words: SAP writes "authorization", "Customizing" and "behavior
 * definition", and a manual that spelled its prose the other way would teach
 * a reader to search for words their system does not use. The two were mixed
 * for years - 15 behaviours against 39 behaviors, in the same chapter
 * sometimes - which is what makes a rule worth a test: a decision that lives
 * only in a document wanders back the next time somebody pastes a paragraph.
 *
 * Checked in PROSE only (scripts/lib/prose.mjs): fenced code, inline code,
 * link targets, tags, comments, URLs and the frontmatter's settings are
 * blanked out first, so a CSS `colour` or an ABAP `INITIALISE` never trips
 * it - and neither does a path that has to stay what it is. A page that
 * fails here is rewritten by `npm run fix:spelling`, which changes exactly
 * the words this test names and nothing else.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { americanize, findBritish, manualPages, proseLines } from '../scripts/lib/prose.mjs';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const PAGES = manualPages(join(ROOT, 'docs'));

test('the walk found the manual', () => {
  assert.ok(PAGES.length > 100, `${PAGES.length} pages`);
});

test('every page spells its prose the way the house does', () => {
  const wrong = [];
  for (const page of PAGES) {
    for (const hit of findBritish(readFileSync(page, 'utf8'))) {
      wrong.push(`${page.slice(ROOT.length + 1)}:${hit.line}: "${hit.word}" - ${hit.american}`);
    }
  }
  assert.deepEqual(wrong, [], 'npm run fix:spelling rewrites these');
});

test('the reader blanks what is not prose, and keeps the offsets', () => {
  const md = '---\ntitle: Colour\ndescription: a colour\nlayout: colour\n---\n# Colour in `colour` and [colour](/colour)\n```css\ncolor: red;\n```\n<span class="colour">colour</span> https://x/colour <!-- colour -->\n<!--\ncolour\n--> colour';
  const lines = proseLines(md);
  const source = md.split('\n');
  assert.equal(lines.length, source.length);
  lines.forEach((line, i) => assert.equal(line.length, source[i].length, `line ${i + 1} keeps its length`));
  assert.equal(lines[1].trim(), 'Colour', 'the title is prose');
  assert.equal(lines[2].trim(), 'a colour', 'and the description');
  assert.equal(lines[3].trim(), '', 'a setting is not');
  assert.equal(lines[5].trim(), '# Colour in          and [colour', 'inline code and the link target go, the link text stays');
  assert.equal(lines[7].trim(), '', 'fenced code is not prose');
  assert.equal(lines[9].trim(), 'colour', 'tags, URLs and comments go, the text between tags stays');
  assert.equal(lines[10].trim(), '');
  assert.equal(lines[11].trim(), '', 'inside a comment that spans lines');
  assert.equal(lines[12].trim(), 'colour', 'and after it closes');
});

test('the British forms, and the American ones they become', () => {
  const pairs = {
    behaviour: 'behavior', behavioural: 'behavioral', colour: 'color', Colours: 'Colors', coloured: 'colored',
    favourite: 'favorite', neighbouring: 'neighboring', honoured: 'honored', labour: 'labor', recolour: 'recolor',
    organise: 'organize', Organised: 'Organized', OPTIMISE: 'OPTIMIZE', customising: 'customizing',
    initialise: 'initialize', synchronisation: 'synchronization', recognisable: 'recognizable',
    analyse: 'analyze', analysing: 'analyzing',
    catalogue: 'catalog', Catalogues: 'Catalogs', cataloguing: 'cataloging', dialogue: 'dialog',
    licence: 'license', Licenced: 'Licensed', practise: 'practice', practising: 'practicing',
    centre: 'center', centred: 'centered', Centring: 'Centering', metre: 'meter',
    labelled: 'labeled', travelling: 'traveling', modelling: 'modeling', cancelled: 'canceled',
    grey: 'gray', artefacts: 'artifacts', whilst: 'while', programme: 'program', enquiry: 'inquiry',
    judgement: 'judgment', acknowledgement: 'acknowledgment', enrolment: 'enrollment', fulfil: 'fulfill',
    'per cent': 'percent', 'co-operate': 'cooperate', focussed: 'focused',
  };
  for (const [british, american] of Object.entries(pairs)) {
    assert.equal(findBritish(british).length, 1, british);
    assert.equal(americanize(british), american, british);
  }
  assert.equal(americanize('the behaviour of a colour'), 'the behavior of a color');
});

test('what is -ise, -our or -re in American English too is left alone', () => {
  const fine = 'enterprise otherwise premise promise promised raise raising noise surprise surprised arising'
    + ' comprise advertise exercise precise franchise cruise disguise sunrise treatise expertise improvisation'
    + ' advisable disable crises rise wise advise devise revise supervise compromise despise improvise merchandise'
    + ' excise concise demise paradise analyses metallic totally initially equally cancellation controlled installed'
    + ' rigorous humorous honorary contour detour flour your hour four tour course resource journal glamour'
    + ' Customizing authorization organization behavior behavioral color center analyze catalog license';
  assert.deepEqual(findBritish(fine), []);
});

test('a correction never reaches code, a link target or a setting', () => {
  const md = '---\ntitle: Colour\nlayout: colour\n---\nA colour in `colour`, [colour](/colour) and\n\n```abap\n  colour = 1.\n```\n<i class="colour">colour</i>';
  assert.equal(americanize(md), '---\ntitle: Color\nlayout: colour\n---\nA color in `colour`, [color](/colour) and\n\n```abap\n  colour = 1.\n```\n<i class="colour">color</i>');
  assert.equal(americanize(americanize(md)), americanize(md), 'and it settles after one pass');
  const generated = 'colour\n<!-- api:start (generated by scripts/generate-api-reference.mjs — do not edit) -->\ncolour\n<!-- api:end -->\ncolour';
  assert.equal(americanize(generated), generated.replace(/^colour/, 'color').replace(/colour$/, 'color'),
    'a generated block is its generator\'s to spell');
});
