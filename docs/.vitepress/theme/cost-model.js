/*
 * The cost calculator's arithmetic, with no DOM in it.
 *
 * The page (docs/resources/cost_calculator.md) carries the sliders and the
 * sheet as plain markup: every slider names its stops in `data-stops`, so
 * the page is the one place the numbers live and this module reads them
 * rather than knowing them. What is here is the reading of a position -
 * "10,000 users", "S/4HANA Private Cloud" - what a line of the sheet comes
 * to, the sum, and an amount written in a currency.
 *
 * The total is zero. Not rounded to zero, not zero for the default settings:
 * zero. abap2UI5 is MIT licensed and there is no edition, tier, seat or
 * per-system fee behind it, so every line on the sheet is the same number
 * whatever the sliders say, and the sum of them is that number too. The
 * functions exist so that a test can say so in code, and so the page can be
 * honest about being a calculator: it takes every input, and it returns the
 * answer. Pinned by test/cost-calculator.test.mjs.
 */

/** The stops a slider steps through, as written on it: `1|5|10`, or
 *  `Community|Standard|Premium`. */
export const stopsOf = (attr) => String(attr || '').split('|').map((s) => s.trim()).filter(Boolean);

/** What a slider at `position` reads: a number with its unit ("250 users",
 *  "1 system" - `unit` is `one|many`), or the stop's own words. A position
 *  off either end reads as the end. */
export function reading(stops, position, unit = '') {
  if (!stops.length) return '';
  const at = Math.min(Math.max(Math.trunc(Number(position)) || 0, 0), stops.length - 1);
  const stop = stops[at];
  const n = Number(stop.replace(/,/g, ''));
  if (!Number.isFinite(n)) return stop;
  const [one, many = one] = unit.split('|');
  const word = n === 1 ? one : many;
  return word ? `${n.toLocaleString('en-US')} ${word}` : n.toLocaleString('en-US');
}

/** What one line of the sheet comes to, for any setting of its slider. */
export const line = () => 0;

/** The sum of the sheet: every line, added up, for the settings given. */
export const total = (settings = {}) => Object.values(settings).reduce((sum, value) => sum + line(value), 0);

/** `value` in `currency`, the way English writes it: `amount(0, 'EUR')` is
 *  "€0". A runtime without the currency data gets the number and the code. */
export function amount(value, currency) {
  try {
    return new Intl.NumberFormat('en', { style: 'currency', currency, maximumFractionDigits: 0 }).format(value);
  } catch {
    return `${value} ${currency}`;
  }
}
