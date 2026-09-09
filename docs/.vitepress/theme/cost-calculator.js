/*
 * The cost calculator, once it is in a browser.
 *
 * The page carries everything: sliders with their stops, a readout beside
 * each, choices made with checkboxes and radio buttons, a button, and - hidden
 * until the button is pressed - a sheet whose lines echo the readings, the
 * total, and the words under it.
 * All of it is written into the HTML with its answer already on it. What
 * this adds is the readout following its slider, the echoes following the
 * readout, every amount rewritten in the currency chosen, and the button.
 *
 * One delegated listener each for `input`, `change` and `click`, on the document,
 * like the Run button and the copy link: the manual's own build loads this
 * once per page and VitePress once per application, and neither has to know
 * whether the page in front of it is the calculator. `cost-model.js` is the
 * half with no DOM in it.
 */
import { amount, line, listed, reading, stopsOf, total } from './cost-model.js';

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
  /* A choice made with radio buttons or checkboxes: the group's name is the
     setting, and what the sheet echoes is the ticked ones' words as a list -
     or the group's own words for nothing ticked. */
  const groups = new Map();
  for (const box of calc.querySelectorAll('input[type="radio"][data-choice], input[type="checkbox"][data-choice]')) {
    if (!groups.has(box.name)) groups.set(box.name, { none: box.closest('[data-none]')?.dataset.none, chosen: [] });
    if (box.checked) groups.get(box.name).chosen.push(box.dataset.choice);
  }
  for (const [name, { none, chosen }] of groups) {
    const said = listed(chosen, none);
    settings[name] = said;
    for (const out of calc.querySelectorAll(`[data-echo="${name}"]`)) out.textContent = said;
  }
  const currency = calc.querySelector('select[data-currency]')?.value || 'EUR';
  for (const cell of calc.querySelectorAll('[data-amount]')) cell.textContent = amount(line(settings), currency);
  for (const cell of calc.querySelectorAll('[data-total]')) cell.textContent = amount(total(settings), currency);
}

/* The button. The sheet is not on the page until it is asked for: pressing
 * Calculate greys the inputs, thinks for a moment, and hands out the sheet -
 * and then offers a new round, which puts the inputs back and takes the
 * sheet away until the next press. THE ANSWER IS THE SAME EVERY TIME, which
 * is the joke; the ceremony is what makes it land. */
function calculate(button) {
  const calc = button.closest(CALCULATOR);
  const inputs = calc?.querySelector('.cost-inputs');
  /* What the button hands out: the sheet inside the calculator, and the words
     under it - the formula, and the one line that is not zero - which are
     markdown outside it and carry the same mark. */
  const results = calc ? [...calc.ownerDocument.querySelectorAll('[data-result]')] : [];
  if (!inputs || !results.length) return;
  const controls = inputs.querySelectorAll('input, select');
  if (calc.dataset.state === 'calculated') {
    for (const result of results) result.hidden = true;
    delete inputs.dataset.locked;
    for (const control of controls) control.disabled = false;
    delete calc.dataset.state;
    button.textContent = button.dataset.calculate;
    return;
  }
  syncCostCalculator(calc.ownerDocument);
  for (const control of controls) control.disabled = true;
  inputs.dataset.locked = '';
  button.disabled = true;
  button.textContent = 'Calculating\u2026';
  setTimeout(() => {
    for (const result of results) result.hidden = false;
    calc.dataset.state = 'calculated';
    button.disabled = false;
    button.textContent = button.dataset.again;
    results[0].scrollIntoView?.({ block: 'nearest' });
  }, 700);
}

export function setUpCostCalculator() {
  const follow = (e) => { if (e.target.closest?.(CALCULATOR)) syncCostCalculator(); };
  document.addEventListener('input', follow);
  document.addEventListener('change', follow);
  document.addEventListener('click', (e) => {
    const button = e.target.closest?.('button[data-calculate]');
    if (button && !button.disabled) calculate(button);
  });
  /* A browser puts a page's controls back where they were when it restores
     the page - back, forward, reload - while the words beside them are the
     page's own again. */
  addEventListener('pageshow', () => syncCostCalculator());
  syncCostCalculator();
}
