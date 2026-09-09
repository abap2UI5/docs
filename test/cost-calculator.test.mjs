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
});

test('the front door\'s cost card leads here, and the page ends at the sponsors', () => {
  assert.match(HOME, /\]\(\/resources\/cost_calculator\)/, 'the cost card links the calculator');
  const last = PAGE.slice(PAGE.lastIndexOf('\n## '));
  assert.match(last, /\]\(\/resources\/sponsor\)/, 'the last section is the one that asks');
  assert.match(last, /open-source/i);
});
