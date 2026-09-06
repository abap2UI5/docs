/*
 * Everything a page of the manual does beyond being HTML.
 *
 * All four of these modules were already framework-free in the theme, because
 * three of them describe behaviour the sample catalogue has too. `index.js`
 * was the only Vue in front of them: it imported them, and hung the same calls
 * on the router's route changes because that site is one application. A page
 * written to disk is not, so the same calls run once, on load, and the router
 * half is simply gone.
 *
 * The search box is NOT here. It is the catalogue's own module, loaded beside
 * this one - see prototype-site.mjs for why that is a borrow and not a copy.
 */
import { setUpPlayground } from './playground.js';
import { setUpCodeLines, watchCodeLines } from './code-lines.js';
import { markDirective, setUpLinkToSelection } from './link-to-selection.js';
import { rememberHere, rememberScroll, restoreScroll } from './site-memory.js';

/* The Run button under a runnable ABAP example, and "copy link to selection":
   one delegated listener each, for the whole document. */
setUpPlayground();
setUpLinkToSelection();

/* A number beside every line of every listing, and an address for it - #B2L42
   is line 42 of the second listing on the page. `watchCodeLines` hangs the
   document's listeners, `setUpCodeLines` numbers what is on this page. */
watchCodeLines();
setUpCodeLines();

/* A link that names the words it points at, followed: the browser's own `:~:`
   does this, and this is for the ones without it. */
if (document.readyState === 'complete') markDirective();
else addEventListener('load', markDirective, { once: true });

/* Where the reader is, for the Documentation item on the other three bars to
   come back to. THE HOME PAGE IS NOT WRITTEN DOWN: if the front door counted
   as a page of the manual, going Home would overwrite the chapter you were
   reading, and Documentation would then open the home page - which is what the
   Home item is for. */
if (!location.pathname.replace(/index\.html$/, '').match(/\/docs\/?$/)) rememberHere('docs');
restoreScroll();

/* How far down the page, which is the other half of coming back to it.
   Throttled: scroll fires per frame and this writes to storage. */
let pending = 0;
addEventListener('scroll', () => {
  if (pending) return;
  pending = setTimeout(() => { pending = 0; rememberScroll(); }, 300);
}, { passive: true });
addEventListener('pagehide', () => rememberScroll());

/* A code group: the renderer gives every tab a radio and marks the first block
   `active`. The blocks sit in a container of their own, so no sibling selector
   reaches from the checked radio to the block it belongs to - this does, once,
   for every group on the page. */
for (const group of document.querySelectorAll('.vp-code-group')) {
  const tabs = [...group.querySelectorAll('.tabs input')];
  const blocks = [...group.querySelectorAll('.blocks > div')];
  group.addEventListener('change', () => {
    const at = tabs.findIndex((t) => t.checked);
    blocks.forEach((b, i) => b.classList.toggle('active', i === at));
  });
}
