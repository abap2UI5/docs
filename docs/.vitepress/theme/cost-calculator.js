/*
 * The cost calculator, once it is in a browser.
 *
 * The page carries everything: sliders with their stops, a readout beside
 * each, a sheet whose lines echo the readings, and the total. All of it is
 * written into the HTML, so a page without this script is a page with the
 * same answer on it - the sliders just do not move the words beside them.
 * What this adds is the readout following its slider, the echoes following
 * the readout, and every amount rewritten in the currency chosen.
 *
 * One delegated listener each for `input` and `change`, on the document,
 * like the Run button and the copy link: the manual's own build loads this
 * once per page and VitePress once per application, and neither has to know
 * whether the page in front of it is the calculator. `cost-model.js` is the
 * half with no DOM in it.
 */
import { amount, line, reading, stopsOf, total } from './cost-model.js';

const CALCULATOR = '.cost[data-cost]';

/** Every readout, echo and amount on the calculator brought in line with
 *  its sliders and its currency. Nothing to do on a page without one. */
export function syncCostCalculator(root = document) {
  const calc = root.querySelector(CALCULATOR);
  if (!calc) return;
  const settings = {};
  for (const slider of calc.querySelectorAll('input[type="range"][data-stops]')) {
    const said = reading(stopsOf(slider.dataset.stops), slider.value, slider.dataset.unit || '');
    settings[slider.id] = said;
    for (const out of calc.querySelectorAll(`output[for="${slider.id}"], [data-echo="${slider.id}"]`)) {
      out.textContent = said;
    }
  }
  const currency = calc.querySelector('select[data-currency]')?.value || 'EUR';
  for (const cell of calc.querySelectorAll('[data-amount]')) cell.textContent = amount(line(settings), currency);
  for (const cell of calc.querySelectorAll('[data-total]')) cell.textContent = amount(total(settings), currency);
}

export function setUpCostCalculator() {
  const follow = (e) => { if (e.target.closest?.(CALCULATOR)) syncCostCalculator(); };
  document.addEventListener('input', follow);
  document.addEventListener('change', follow);
  /* A browser puts a page's controls back where they were when it restores
     the page - back, forward, reload - while the words beside them are the
     page's own again. */
  addEventListener('pageshow', () => syncCostCalculator());
  syncCostCalculator();
}
