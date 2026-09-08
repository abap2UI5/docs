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
import { entryOf, handOff, lastVisited, rememberHere, rememberScroll, restoreScroll, takeHandoff } from './site-memory.js';

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

/* ---- and the bar goes BACK to a page that is still behind you -----------
 *
 * Reported twice. First: "the Playground tab - when I go to it the app is
 * always run again, although it had already run before." Then, once that
 * item went back: the front door, with the example under Try it out now
 * running in it, loaded fresh every time as well - and the runnable example
 * is a whole ABAP runtime in a frame, exactly what the playground is.
 *
 * Every item in the bar is a link, so a press builds a NEW document: the
 * runtime boots and the app starts from the top. Measured against a local
 * copy with no network in the way, that is 2.4 to 2.8 seconds every single
 * time - and the app's own state, a half-filled form or a table scrolled to
 * row 200, is gone with the document that held it. No browser preserves a
 * running page across a forward navigation. Exactly one mechanism preserves
 * it at all, and it is the back/forward cache, which applies to a page the
 * reader has BEEN on.
 *
 * So when the page an item opens is still in this tab's history, the press
 * goes to it there - Home, Documentation, Samples and Playground alike, which
 * is also why this is one rule and not four. Two ways of knowing which entry:
 *
 *   - The Navigation API: navigation.entries() is the tab's same-origin
 *     history, and entryOf( ) (theme/site-memory.js) finds the nearest entry
 *     that IS the item's page - same origin, path and query. Any distance,
 *     either direction: a reader who read two chapters since is two steps
 *     behind it, and one who left it with the Back button is one step in
 *     front.
 *   - Without it, the one case that can be known: this document was opened
 *     FROM that page (a sample page lives under the playground's path and is
 *     a different page, which is why the whole URL is compared), and the
 *     history has not grown since - an anchor or a text fragment pushed onto
 *     it would make one step back something else. A tab the playground
 *     opened with target=_blank has nothing behind it at all.
 *
 * It can only ever land on the link: the item's href is what is looked for,
 * lifted or not. The item for the page the reader is ON - Documentation on
 * every page of the manual - is the browser's to handle, as it always was.
 * What this adds is the browser's option to hand the page back alive instead
 * of rebuilding it - and where the browser declines, a reload is what the
 * link would have done anyway (the playground says why in its console,
 * main.mjs over there).
 *
 * A page handed back alive is where the reader left it, offset and all, so
 * the record a data-back link wrote on the way out is spent: it is taken on
 * pageshow, or the next arrival at that page within the half minute would
 * inherit it.
 *
 * The fallback is for the one case that would be worse than today: a press
 * that does not navigate would be a dead press. A traversal the browser cannot
 * make rejects, and if this document is still here after a moment the link is
 * followed after all - and the timer is dropped on pagehide, so a document
 * that WAS cached does not fire it on the way back in and bounce the reader
 * out of the page they returned to. */
(function barBehindYou() {
  if (!document.querySelector('.bar-nav a[href]')) return;
  const bare = (u) => u.origin + u.pathname.replace(/index\.html$/, '') + u.search;
  const behind = history.length;
  const cameFromIt = (href) => {
    if (history.length < 2 || history.length !== behind) return false;
    try {
      return bare(new URL(document.referrer)) === bare(new URL(href, location.href));
    } catch { return false; }
  };
  addEventListener('pageshow', (e) => { if (e.persisted) takeHandoff(); });
  document.addEventListener('click', (e) => {
    const a = e.target.closest?.('.bar-nav a[href]');
    if (!a || e.defaultPrevented) return;
    /* A press that means "in a new tab" still means that. */
    if (e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    if (a.target && a.target !== '_self') return;
    const href = a.href;
    let want;
    try { want = bare(new URL(href, location.href)); } catch { return; }
    if (want === bare(new URL(location.href))) return;
    const nav = globalThis.navigation;
    let step;
    if (nav?.entries && nav.traverseTo) {
      const key = entryOf(nav.entries(), nav.currentEntry?.index ?? -1, href);
      if (key === null) return;
      step = () => nav.traverseTo(key).committed;
    } else if (cameFromIt(href)) {
      step = () => history.back();
    } else {
      return;
    }
    e.preventDefault();
    const fallback = setTimeout(() => { location.href = href; }, 1500);
    addEventListener('pagehide', () => clearTimeout(fallback), { once: true });
    Promise.resolve().then(step).catch(() => { clearTimeout(fallback); location.href = href; });
  }, true);
})();

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
    /* `aria-current` beside the class, because the mark is information and not
       only a colour: a reader on a screen reader hears "current location" on
       the row the class draws in the accent. The bar names its own item the
       same way, and the menu's row is marked by the build. */
    if (at > -1) { links[at].classList.remove('here'); links[at].removeAttribute('aria-current'); }
    if (last > -1) { links[last].classList.add('here'); links[last].setAttribute('aria-current', 'true'); }
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

/* ---- the chapter menu keeps the shape YOU left it in --------------------
 *
 * Every page is a fresh document here, so the sections the build opened - the
 * one holding this page - were the only ones open, and every other section a
 * reader had unfolded shut itself on the next click. So the menu is written
 * down and put back.
 *
 * WHAT IS WRITTEN DOWN IS WHAT THE READER DID, and nothing else. The first
 * version wrote the whole visible menu on every `toggle` event, which sounds
 * the same and is not: `toggle` is queued, so the events fired by putting the
 * menu BACK arrive after this listener is attached, and every navigation wrote
 * the path to wherever the reader had landed as though they had opened it by
 * hand. Measured over five pages with the menu never touched once: the store
 * grew from 1 entry to 8, and the reader's own choices were buried under a
 * trail of rooms they had merely walked through. The menu remembered a shape;
 * it just was not theirs.
 *
 * So a decision is recorded per section, on the click that makes it - one
 * section, the state it now has - and the store holds only sections somebody
 * actually opened or closed. Nothing a navigation does can reach it.
 *
 * The section holding the current page is opened regardless of what is
 * stored: it is where the reader is, and a menu that hid it would be worse
 * than one that forgets. Opened, and NOT written down - that was the bug. */
(function tree() {
  /* A second name, because every value under the first one was written by the
     navigation bug above and none of it is the reader's. A store that cannot
     be trusted is worse than an empty one. */
  const KEY = 'abap2ui5-playground:docs-sections';
  const groups = [...document.querySelectorAll('.sidebar details[data-key]')];
  if (!groups.length) return;
  /* key -> true when the reader opened that section, false when they closed
     it. A section they never touched is simply absent, and keeps whatever
     shape the build gave it. */
  const read = () => {
    try {
      const v = JSON.parse(localStorage.getItem(KEY) || 'null');
      return v && typeof v === 'object' && !Array.isArray(v) ? v : {};
    } catch { return {}; }
  };
  const chosen = read();
  for (const g of groups) {
    if (Object.prototype.hasOwnProperty.call(chosen, g.dataset.key)) g.open = !!chosen[g.dataset.key];
  }
  /* ...and the way to where you are, whatever was stored. */
  const here = document.querySelector('.sidebar a.here');
  if (here) for (let el = here.closest('details'); el; el = el.parentElement?.closest('details')) el.open = true;

  /* ...AND THE MENU IS SCROLLED TO IT. The manual is 166 rows deep and the
   * menu is its own scrolling box, which every page opened at the top of. A
   * reader who landed on an Insights page saw Getting Started and had to
   * scroll 1800px inside the menu to find out where they were - measured on
   * advanced/insights/36, whose row sits at 1840 in a box 800 tall.
   *
   * Only when the row is actually out of view, and set rather than animated:
   * this runs at load, and a menu that scrolls by itself in front of the
   * reader is a different kind of wrong. Roughly a third down rather than at
   * the very top, so the rows above it - its neighbours in the same section -
   * are on screen too. */
  const box = document.querySelector('.sidebar');
  if (here && box && box.scrollHeight > box.clientHeight) {
    const at = here.getBoundingClientRect().top - box.getBoundingClientRect().top + box.scrollTop;
    if (at < box.scrollTop + 8 || at > box.scrollTop + box.clientHeight - 40)
      box.scrollTop = Math.max(0, at - box.clientHeight / 3);
  }

  const write = () => {
    try {
      localStorage.setItem(KEY, JSON.stringify(chosen));
    } catch { /* a browser that refuses storage keeps the build's own shape */ }
  };
  /* The CLICK, not the toggle: a click on a summary is the reader, and it is
     the only thing that is. Reading the state on the next task, once the
     browser has done the opening the click asked for - and a keyboard reaches
     a summary through a click too, so Enter and Space are the same path. */
  box?.addEventListener('click', (e) => {
    const group = e.target.closest?.('summary')?.parentElement;
    if (!group || !groups.includes(group)) return;
    setTimeout(() => { chosen[group.dataset.key] = group.open; write(); }, 0);
  });
})();
/* ---- copy a listing ---------------------------------------------------
 *
 * The button is the renderer's own, on every code block, and nothing had ever
 * wired it. What it copies is the code as the file has it - the text of the
 * <code>, with the line numbers left out, because those are drawn by a CSS
 * counter off an empty link and are not part of the source.
 *
 * `navigator.clipboard` is not there on an insecure origin or in an older
 * browser, so the button says what happened either way rather than looking
 * broken: the class it takes for a second is the only feedback, and a refusal
 * leaves it alone. */
document.addEventListener('click', async (e) => {
  const button = e.target.closest?.('div[class*="language-"] .copy');
  if (!button) return;
  const code = button.parentElement.querySelector('code');
  if (!code) return;
  /* The gutter's links carry no text, so textContent is already the source -
     but a line is a <span> and they are joined with no newline between them
     when the markup has none, which is why the lines are read one by one. */
  const lines = [...code.querySelectorAll('.line')];
  const text = (lines.length ? lines.map((l) => l.textContent).join('\n') : code.textContent).replace(/\s+$/, '');
  try {
    await navigator.clipboard.writeText(text);
    button.classList.add('done');
    setTimeout(() => button.classList.remove('done'), 1400);
  } catch { /* a browser that refuses the clipboard: the text is still selectable */ }
});

