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
import { handOff, lastVisited, rememberHere, rememberScroll, restoreScroll } from './site-memory.js';

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

/* ---- the bar remembers where you were ---------------------------------
 *
 * THIS IS THE HALF THAT WAS MISSING. `rememberHere` above writes down which
 * page of the manual the reader is on; what makes that worth writing is the
 * OTHER side - the bar's Samples and Documentation items opening the page you
 * left rather than the front of the section. The Vue bar did this in
 * SiteNav.vue and it went with the theme; the borrowed bar carries the
 * attributes (`data-site`, `data-scope`, `data-back`) and nothing was reading
 * them.
 *
 * The href as it was WRITTEN is kept from the first lift, because after one
 * the attribute is the page that was restored. `lastVisited` does the
 * checking: a stored value is whatever anything on this origin put there, so
 * it is resolved against this origin and kept only if it still falls inside
 * the section the markup declares. */
const written = new Map();
function lift() {
  for (const a of document.querySelectorAll('a[data-site]')) {
    if (!written.has(a)) written.set(a, a.getAttribute('href'));
    const href = written.get(a);
    if (!href) continue;
    a.href = lastVisited(a.dataset.site, href, a.dataset.scope || href);
  }
}
lift();
/* ...and again whenever it can have gone stale while this page stayed open:
   shown again, looked at again, and on the click itself. */
addEventListener('pageshow', lift);
document.addEventListener('visibilitychange', () => {
  if (document.visibilityState === 'visible') lift();
});
document.addEventListener('click', (e) => {
  if (e.target.closest?.('a[data-site]')) lift();
}, true);

/* Where on the page, not only which page. A bar link writes down how far down
   this page the reader is AND where they are being sent; the page that arrives
   within seconds, and only that page, puts them back (restoreScroll above). */
document.addEventListener('click', (e) => {
  const a = e.target.closest?.('a[data-back]');
  if (!a) return;
  rememberScroll();
  handOff(a.href);
}, true);

/* ---- the outline says which section you are in ------------------------
 *
 * The same walk the per-sample pages do, case for case: the heading whose top
 * has passed the line under the bar, `-1` above the first one because there is
 * no section to be in there, and the last row once the page is scrolled to the
 * end whether or not its heading ever crossed. */
(function outline() {
  const nav = document.querySelector('.outline nav');
  if (!nav) return;
  const links = [...nav.querySelectorAll('a')];
  const heads = links.map((a) => document.getElementById(decodeURIComponent(a.getAttribute('href').slice(1))));
  if (!heads.length || heads.includes(null)) return;

  /* The bar is 46px and sticky, plus the air a heading needs under it before
     it counts as reached - the number the outline's own `top` uses. */
  const LINE = 70;
  let at = -1;
  const mark = () => {
    let last = -1;
    if (window.scrollY >= 1) {
      for (let i = 0; i < heads.length; i++) {
        if (heads[i].getBoundingClientRect().top <= LINE) last = i;
      }
      if (window.innerHeight + window.scrollY >= document.body.scrollHeight - 2) last = heads.length - 1;
    }
    if (last === at) return;
    if (at > -1) links[at].classList.remove('here');
    if (last > -1) links[last].classList.add('here');
    at = last;
  };
  let pending = false;
  const schedule = () => {
    if (pending) return;
    pending = true;
    requestAnimationFrame(() => { pending = false; mark(); });
  };
  mark();
  addEventListener('scroll', schedule, { passive: true });
  addEventListener('resize', schedule, { passive: true });
})();

/* ---- the chapter menu keeps the shape you left it in -------------------
 *
 * Every page is a fresh document here, so the sections the build opened - the
 * one holding this page - were the only ones open, and every other section a
 * reader had unfolded shut itself on the next click. What is written down is
 * the set of sections that are OPEN, by the key the build gives each one.
 *
 * The section holding the current page is opened regardless of what is
 * stored: it is where the reader is, and a menu that hid it would be worse
 * than one that forgets. */
(function tree() {
  const KEY = 'abap2ui5-playground:docs-tree';
  const groups = [...document.querySelectorAll('.sidebar details[data-key]')];
  if (!groups.length) return;
  const read = () => {
    try {
      const v = JSON.parse(localStorage.getItem(KEY) || '[]');
      return Array.isArray(v) ? new Set(v.filter((k) => typeof k === 'string')) : null;
    } catch { return null; }
  };
  const open = read();
  if (open) for (const g of groups) g.open = open.has(g.dataset.key);
  /* ...and the way to where you are, whatever was stored. */
  const here = document.querySelector('.sidebar a.here');
  if (here) for (let el = here.closest('details'); el; el = el.parentElement?.closest('details')) el.open = true;

  const write = () => {
    try {
      localStorage.setItem(KEY, JSON.stringify(groups.filter((g) => g.open).map((g) => g.dataset.key)));
    } catch { /* a browser that refuses storage keeps the build's own shape */ }
  };
  for (const g of groups) g.addEventListener('toggle', write);
})();
