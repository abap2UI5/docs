/*
 * An address for every line of every listing on this site.
 *
 * The sample catalogue got this first: a class printed on a page, with a
 * number beside every line, and #L42 / #L42-L58 addressing a line or a passage
 * of it. "Look at line 40 to 55" is most of what one person tells another
 * about code, and until it existed here it could only be said about the copy
 * on GitHub — or, now, about a sample and not about the manual.
 *
 * WHAT IS DIFFERENT HERE, and why the fragment does not read `#L42`. A sample's
 * page prints ONE class, so a bare line number is unambiguous on it. A page of
 * this manual is a dozen listings interleaved with prose, and `#L42` would
 * name a line in each of them. So the block is part of the address:
 * `#B2L42` is line 42 of the second listing on the page, `#B2L42-L58` a
 * passage of it. The block index is its position in the page and nothing else
 * — no title to keep in step, and no link that rots when a paragraph moves.
 *
 * WHAT IT DOES NOT COST: the class still copies as code. The numbers are a CSS
 * counter drawn by `::before` from an empty link, so a reader who selects the
 * block — or presses VitePress's own copy button, which reads the same text —
 * gets a file they can paste. That is the same reason the catalogue draws them
 * that way, and it is the whole reason this is a stylesheet rule rather than a
 * number in the markup.
 *
 * This is a single page application, so it runs again on every route change,
 * and it is idempotent: a block that already carries its numbers is left
 * alone.
 */

const HASH = /^#B([0-9]+)L([0-9]+)(?:-L([0-9]+))?$/;

/** The listings of the page, in the order they are written. */
const blocks = () => [...document.querySelectorAll('.vp-doc div[class*="language-"] pre code')];

/** `{ block, from, to }` for the current address, or null. */
function picked() {
  const m = HASH.exec(location.hash);
  if (!m) return null;
  const a = Number(m[2]);
  const b = m[3] === undefined ? a : Number(m[3]);
  return { block: Number(m[1]), from: Math.min(a, b), to: Math.max(a, b) };
}

function mark(scroll) {
  for (const el of document.querySelectorAll('.vp-doc .ln.is-marked')) el.classList.remove('is-marked');
  const range = picked();
  if (!range) return;
  let first = null;
  for (let n = range.from; n <= range.to; n++) {
    const el = document.getElementById(`B${range.block}L${n}`);
    if (!el) continue;
    el.classList.add('is-marked');
    if (first === null) first = el;
  }
  if (scroll && first) first.scrollIntoView({ block: 'center' });
}

/** The line a shift-click extends from: the last one picked on its own. */
let anchor = null;

function number(code, bi) {
  if (code.dataset.lines) return;
  const lines = [...code.querySelectorAll(':scope > .line')];
  /* Shiki wraps every line, including the empty one a trailing newline leaves.
   * A number beside nothing at the end of a listing is a line that is not
   * there. */
  if (lines.length && lines[lines.length - 1].textContent === '') lines.pop();
  if (lines.length < 2) return;
  lines.forEach((line, i) => {
    const n = i + 1;
    line.classList.add('ln');
    line.id = `B${bi}L${n}`;
    const a = document.createElement('a');
    a.href = `#B${bi}L${n}`;
    /* An empty link with no name is a link a screen reader cannot announce.
     * The number itself is the stylesheet's, so that it stays out of what a
     * reader copies. */
    a.setAttribute('aria-label', `Line ${n}`);
    /* And out of the tab order: a chapter has 55 of these against 69 of
     * everything else, so a reader on a keyboard pressed Tab two dozen times
     * to get past ONE listing. The gutter is a pointer affordance - the number
     * is drawn by the stylesheet and the link under it is how a mouse picks a
     * line up - and nothing is lost by it: the link still answers a click,
     * `#B1L42` still opens where it always did, and a screen reader still
     * meets the link in the page, because a browse cursor is not the tab
     * order. The listing itself stays a stop, named and scrollable. */
    a.tabIndex = -1;
    line.prepend(a);
  });
  code.dataset.lines = String(lines.length);
  code.dataset.block = String(bi);
}

export function setUpCodeLines() {
  blocks().forEach((code, i) => number(code, i + 1));
  mark(false);
}

/** Once, at boot: the listeners are the document's and outlive every page. */
export function watchCodeLines() {
  document.addEventListener('click', (e) => {
    const a = e.target?.closest?.('.vp-doc .ln > a');
    /* A modified click is the reader opening the line in a tab of their own,
     * which is a thing the browser does better than this. */
    if (!a || e.metaKey || e.ctrlKey) return;
    const m = HASH.exec(a.getAttribute('href'));
    if (!m) return;
    const block = Number(m[1]);
    const line = Number(m[2]);
    let hash = `#B${block}L${line}`;
    if (e.shiftKey && anchor && anchor.block === block) {
      hash = `#B${block}L${Math.min(anchor.line, line)}-L${Math.max(anchor.line, line)}`;
    } else {
      anchor = { block, line };
    }
    e.preventDefault();
    /* replaceState, not a jump: the address bar is the share link, and Back
     * should leave the page rather than walk the selections before this one. */
    history.replaceState(null, '', hash);
    mark(false);
  });

  addEventListener('hashchange', () => {
    const range = picked();
    if (range) anchor = { block: range.block, line: range.from };
    mark(true);
  });
}
