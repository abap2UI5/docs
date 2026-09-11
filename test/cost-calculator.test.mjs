/*
 * The cost calculator (resources/cost_calculator): a sheet of sliders whose
 * every line comes to zero, because that is what abap2UI5 costs.
 *
 * Two things are worth holding. The arithmetic - theme/cost-model.js, the
 * half with no DOM in it - so that the joke stays true in code: every line
 * is zero and so is the sum, for any setting of any slider. And the page
 * against the script: the readouts and the amounts are written into the
 * markup, so what the button reveals is what the page already says, and a
 * slider whose written readout is not what the script would write for its
 * starting position would flicker to the truth on load.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { amount, line, listed, reading, stopsOf, total } from '../docs/.vitepress/theme/cost-model.js';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const PAGE = readFileSync(join(ROOT, 'docs', 'resources', 'cost_calculator.md'), 'utf8');
const HOME = readFileSync(join(ROOT, 'docs', 'index.md'), 'utf8');

test('a reading is the stop at the position, with its unit', () => {
  assert.equal(reading(stopsOf('1|5|10'), 0, 'user|users'), '1 user');
  assert.equal(reading(stopsOf('1|5|10000'), 2, 'user|users'), '10,000 users');
  assert.equal(reading(stopsOf('1|5|10000'), '2', 'user|users'), '10,000 users', 'a slider\'s value is a string');
  assert.equal(reading(stopsOf('Community|Premium'), 1), 'Premium');
  assert.equal(reading(stopsOf('1|5'), 9, 'year|years'), '5 years', 'past the end is the end');
  assert.equal(reading(stopsOf('1|5'), -1, 'year|years'), '1 year');
  assert.equal(reading(stopsOf('1|5'), 'x'), '1');
  assert.equal(reading(stopsOf(''), 3), '');
});

test('choices read out as a list', () => {
  assert.equal(listed([], 'no system at all'), 'no system at all');
  assert.equal(listed(['S/4HANA on-premise']), 'S/4HANA on-premise');
  assert.equal(listed(['NetWeaver 7.50', 'BTP ABAP Environment']), 'NetWeaver 7.50 and BTP ABAP Environment');
  assert.equal(listed(['A', 'B', 'C']), 'A, B and C');
});

test('an amount is the currency\'s own zero', () => {
  assert.equal(amount(0, 'EUR'), '€0');
  assert.equal(amount(0, 'USD'), '$0');
  assert.equal(amount(0, 'JPY'), '¥0');
  /* A code and its number are joined with a no-break space, so they never
     part at a line's end. */
  assert.equal(amount(0, 'CHF'), 'CHF\u00a00');
  assert.equal(amount(0, 'XYZ'), 'XYZ\u00a00', 'a code the runtime does not know still reads');
});

test('every line is zero, and so is the sum, whatever the sliders say', () => {
  assert.equal(line('250,000 users'), 0);
  assert.equal(line(), 0);
  assert.equal(total({}), 0);
  assert.equal(total({ users: 250000, systems: 50, support: 'Enterprise Plus', term: 10 }), 0);
  for (let i = 0; i < 100; i++) {
    assert.equal(total({ a: Math.random() * 1e9, b: -1, c: Infinity, d: 'many', e: NaN }), 0);
  }
});

test('the page and the script agree on what every slider says at load', () => {
  const sliders = [...PAGE.matchAll(
    /<input id="([^"]+)" type="range" min="(\d+)" max="(\d+)" value="(\d+)" data-stops="([^"]+)"(?: data-unit="([^"]*)")?>/g,
  )];
  assert.ok(sliders.length >= 6, `${sliders.length} sliders`);
  const ids = new Set();
  for (const [, id, min, max, value, stops, unit] of sliders) {
    ids.add(id);
    const all = stopsOf(stops);
    assert.equal(Number(min), 0, id);
    assert.equal(Number(max), all.length - 1, `${id}: max is the last stop`);
    assert.ok(Number(value) <= Number(max), `${id}: starts on a stop`);
    const said = reading(all, Number(value), unit || '');
    const out = PAGE.match(new RegExp(`<output for="${id}">([^<]*)</output>`));
    assert.ok(out, `${id} has a readout`);
    assert.equal(out[1], said, `${id}: the readout at load is what the script would write`);
    for (const echo of PAGE.matchAll(new RegExp(`<span data-echo="${id}">([^<]*)</span>`, 'g'))) {
      assert.equal(echo[1], said, `${id}: an echo at load is what the script would write`);
    }
  }
  /* A choice made with radio buttons or checkboxes: what is ticked at load,
     read out as a list, is what the sheet echoes - and the group's name is an
     echo target like a slider's id. */
  const groups = new Map();
  for (const [, kind, name, choice, on] of PAGE.matchAll(/<input type="(radio|checkbox)" name="([^"]+)" value="[^"]+" data-choice="([^"]+)"( checked)?>/g)) {
    if (!groups.has(name)) groups.set(name, { kind, chosen: [] });
    assert.equal(groups.get(name).kind, kind, `${name} is one kind of choice`);
    if (on) groups.get(name).chosen.push(choice);
  }
  assert.ok(groups.size >= 2, `${groups.size} groups`);
  for (const [name, { kind, chosen }] of groups) {
    ids.add(name);
    if (kind === 'radio') assert.equal(chosen.length, 1, `${name}: exactly one choice is made at load`);
    const none = PAGE.match(new RegExp(`aria-labelledby="${name}-label" data-none="([^"]*)"`))?.[1];
    for (const echo of PAGE.matchAll(new RegExp(`<span data-echo="${name}">([^<]*)</span>`, 'g'))) {
      assert.equal(echo[1], listed(chosen, none), `${name}: the echo at load is what is ticked at load`);
    }
  }
  for (const [, id] of PAGE.matchAll(/data-echo="([^"]+)"/g)) assert.ok(ids.has(id), `an echo of ${id} has a slider or a choice`);
  for (const [, id] of PAGE.matchAll(/<label for="([^"]+)"/g)) {
    assert.match(PAGE, new RegExp(`<(?:input|select) id="${id}"`), `the label ${id} names a control`);
  }
});

test('every amount on the page is the chosen currency\'s zero', () => {
  const amounts = [...PAGE.matchAll(/<(?:span|output) [^>]*data-(?:amount|total)[^>]*>([^<]*)</g)].map((m) => m[1]);
  assert.ok(amounts.length >= 10, `${amounts.length} amounts`);
  const currency = PAGE.match(/<option value="([A-Z]{3})" selected>/)[1];
  for (const a of amounts) assert.equal(a, amount(0, currency));
});

test('the sheet waits for the button', () => {
  assert.match(PAGE, /<button type="button" class="cost-calculate" data-calculate="Calculate" data-again="[^"]+">Calculate<\/button>/);
  assert.match(PAGE, /<div class="cost-result" data-result hidden>/, 'the sheet is hidden until Calculate is pressed');
  assert.ok(PAGE.indexOf('<div class="cost-inputs">') < PAGE.indexOf('data-calculate='), 'the inputs come first');
  assert.ok(PAGE.indexOf('data-calculate=') < PAGE.indexOf('data-result'), 'then the button, then the sheet');
  /* The words under the calculator wait with it: the formula and the one line
     that is not zero are markdown inside a wrapper that carries the same mark,
     with blank lines around the markdown so the renderer still renders it. */
  const after = PAGE.indexOf('<div class="cost-after" data-result hidden>\n\n');
  assert.ok(after > PAGE.lastIndexOf('<div class="cost-result"'), 'the words under the sheet wait for the button too');
  assert.ok(PAGE.trimEnd().endsWith('\n\n</div>'), 'and the wrapper closes after the last of them');
  assert.ok(PAGE.indexOf('\n## Free to use is not the same as free to make', after) > after, 'the section is markdown inside it, heading and all');
});

/* What the sheet carries that the inputs deliberately do not: the links to the
   issue tracker and to Slack. They sat in the support row, which is a way out
   of the page in the middle of filling it in, and they belong on the Support
   line of the answer instead.

   There was a list under the total here too - what the zeros get you - on the
   grounds that ten identical characters are a thin reward for nine sliders.
   It was taken out again: the sentences under the sheet say the same thing in
   the page's own voice, and the list said it twice. Its rules went out of both
   stylesheets with it. */
test('the links wait for the sheet, and lead out of it only there', () => {
  const inputs = PAGE.slice(PAGE.indexOf('<div class="cost-inputs">'), PAGE.indexOf('data-calculate='));
  assert.doesNotMatch(inputs, /<a /, 'nothing leads out of the page while it is being filled in');
  const sheet = PAGE.slice(PAGE.indexOf('<div class="cost-result"'), PAGE.indexOf('<div class="cost-after"'));
  assert.match(sheet, /Support<small>[^<]*<span data-echo="cost-support">/, 'the sheet has a support line');
  assert.match(sheet, /github\.com\/abap2UI5\/abap2UI5\/issues/, 'and the issue tracker is on it');
  assert.match(sheet, /communityinviter\.com/, 'and Slack');
  assert.doesNotMatch(sheet, /cost-perks|<\/ul>/, 'and nothing is left of the list that used to close the sheet');
});

/* The page answers in zeros, so the section that says somebody still paid for
   them is a panel rather than a paragraph after the sheet - and what it asks
   for is mostly not money: four ways, three of which cost an evening. The
   ways are pinned because the ONE that takes money is the one a page like
   this drifts towards over time, and the other three are the point. */
test('the front door\'s cost card leads here, and the page ends on the four ways', () => {
  assert.match(HOME, /\]\(\/resources\/cost_calculator\)/, 'the cost card links the calculator');
  const last = PAGE.slice(PAGE.lastIndexOf('\n## '));
  assert.ok(PAGE.indexOf('<div class="cost-give">') < PAGE.lastIndexOf('\n## '), 'the section stands in a panel of its own');
  assert.match(last, /\]\(\/resources\/sponsor\)/, 'the last section is the one that asks');
  assert.match(last, /open-source/i);
  const ways = (last.match(/^- \*\*/gm) || []);
  assert.equal(ways.length, 9, 'nine ways, each led by what it is and each one thing to do');
  for (const way of last.split('\n').filter((l) => l.startsWith('- **'))) {
    assert.match(way, /\]\(/, `${way.slice(0, 28)}… says where to do it`);
  }
  /* Every way says where to do it, and the four places are not
     interchangeable: two to answer somebody in, two to say it out loud. And
     every way carries a link, because a way without one is a wish. */
  assert.match(last, /communityinviter\.com/, 'Slack');
  assert.match(last, /github\.com\/abap2UI5\/abap2UI5\/issues/, 'the issue tracker');
  assert.match(last, /linkedin\.com\/company\/abap2ui5/, 'the project page a post can mention');
  assert.match(last, /community\.sap\.com/, 'the SAP Community');
  assert.match(last, /#abap2UI5/, 'and the tag that collects those posts');
  assert.match(last, /\]\(\/resources\/references\)/, 'where what is written about it lands');
  assert.match(last, /\]\(\/resources\/who_uses\)/, 'and the list a company can put itself on');
  /* An intro that counts the ways goes stale the moment one is added or
     taken out - which is exactly how it went stale. The count was then taken
     out of the intro altogether, which is the other way of never being
     wrong; so the number is held to the list only while the intro states
     one. All but one cost an evening; that one is the money. */
  const said = last.match(/- (\w+) that cost an evening, and one that costs money/);
  const words = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven',
                 'eight', 'nine', 'ten', 'eleven', 'twelve'];
  if (said) {
    assert.equal(said[1], words[ways.length - 1], `${ways.length} ways, so the intro says ${words[ways.length - 1]} plus the one that costs money`);
  }
});

/* The stylesheet is written twice - scripts/site-css/docs.css for the site the
   build renders, docs/.vitepress/theme/style.css for VitePress's second
   opinion - and the calculator's rules are the same text in both, from the
   grid a row is down to the two lines it becomes on a phone. One edited
   without the other is a page that is right in one build and wrong in the
   other. */
test('the two stylesheets carry one calculator, its phone rules included', () => {
  const block = (file) => {
    const css = readFileSync(join(ROOT, file), 'utf8');
    const from = css.indexOf('.cost { margin: 20px 0; }');
    const to = css.indexOf('.cost-total { grid-template-columns: 1fr; } }', from);
    assert.ok(from > 0 && to > from, `${file} carries the calculator`);
    return css.slice(from, to);
  };
  const site = block('scripts/site-css/docs.css');
  assert.equal(block('docs/.vitepress/theme/style.css'), site, 'the theme carries the same calculator');
  /* On a phone a row is the label with its readout, then the slider across the
     whole width; a choice's boxes go under their label; the button is the
     width of the row and the height of a finger. */
  const phone = site.slice(site.indexOf('@media (max-width: 620px) {'));
  assert.match(phone, /\.cost-row output \{ grid-column: 2; grid-row: 1; \}/);
  assert.match(phone, /\.cost-row input\[type="range"\] \{ grid-column: 1 \/ -1; grid-row: 2; \}/);
  assert.match(phone, /\.cost-row \.cost-choices \{ grid-column: 1 \/ -1; \}/);
  assert.match(phone, /\.cost-calculate \{ width: 100%; line-height: 44px; \}/);
  /* Under a thumb the boxes are the size of a target, and the currency list is
     the one size iOS does not zoom the page in on. */
  const thumb = site.slice(site.indexOf('@media (pointer: coarse) {'));
  assert.match(thumb, /input\[type="checkbox"\] \{ width: 18px; height: 18px; \}/);
  assert.match(thumb, /\.cost-row select \{ font-size: 16px; \}/);
});
