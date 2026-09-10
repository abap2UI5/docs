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
import { setUpCostCalculator } from './cost-calculator.js';
import { entryOf, forgetOnReload, handOff, lastVisited, rememberHere, rememberScroll, restoreScroll, takeHandoff } from './site-memory.js';

/* ---- a refresh starts over ---------------------------------------------
 *
 * "It now remembers everywhere on the documentation where I was - but when I
 * refresh the page everything should be initial again, the menus folded, when
 * I move between Documentation, Samples and Home."
 *
 * Everything this site writes down is a memory of a JOURNEY between its pages,
 * kept because every page is a fresh document (site-memory.js says the rest).
 * Reload is the one press that has never meant "go somewhere", and it is what
 * a reader reaches for when a page looks wrong - so what it gets back is a
 * first visit: the chapter menu as the build folds it, no offset, and four bar
 * items pointing at their section's front page.
 *
 * FIRST, BEFORE ANYTHING READS ANY OF IT. The menu below is put back from
 * storage as this file runs, the bar is lifted from it a few lines down, and
 * the search box - the catalogue's own module, bundled after this one - opens
 * with the last query. All of them read a store this has already emptied.
 *
 * The two keys here are the ones this deployment owns; the five the memory
 * itself keeps are its own business. The theme is in neither list: a colour
 * scheme is a choice about every page there will ever be, not a place. */
const SECTIONS_KEY = 'abap2ui5-playground:docs-sections';
/* Spelled here and in the search module the bar carries (theme/search-engine.js
   is this repository's copy of it, QUERY_KEY). */
const SEARCH_KEY = 'abap2ui5-playground:search';
forgetOnReload([SECTIONS_KEY, SEARCH_KEY]);

/* The Run button under a runnable ABAP example, and "copy link to selection":
   one delegated listener each, for the whole document. */
setUpPlayground();
setUpLinkToSelection();

/* The cost calculator's sliders and the sheet under them (cost-calculator.js):
   two delegated listeners, and nothing to do on any page but that one. */
setUpCostCalculator();

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
  /* And when THIS page arrived by Back or Forward - a bar item on one of the
     other three sites stepping back to it, or the browser's own button - and
     was built again rather than handed back, the one place the browser says
     why is notRestoredReasons on the arrival. The only console line on this
     site, for the same reason the playground has one: nothing a reader can do
     about it, nothing on the page to say it in, and the person who needs it
     is reading the console anyway. On the front door it is the running
     example that was lost; on a chapter, only the offset and a Run panel. */
  const arrival = performance.getEntriesByType?.('navigation')?.[0];
  if (arrival?.type === 'back_forward' && arrival.notRestoredReasons) {
    console.info('docs: rebuilt rather than restored from the back/forward cache', arrival.notRestoredReasons);
  }
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
  const KEY = SECTIONS_KEY;
  /* A section is a checkbox followed by its group (build-site.mjs): the box
     is what opens it, the group carries the key it is remembered by. */
  const groups = [...document.querySelectorAll('.sidebar .side-group[data-key]')];
  const boxOf = (g) => g.previousElementSibling;
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
    if (Object.prototype.hasOwnProperty.call(chosen, g.dataset.key)) boxOf(g).checked = !!chosen[g.dataset.key];
  }
  /* ...and the way to where you are, whatever was stored. */
  const here = document.querySelector('.sidebar .here');
  if (here) for (let el = here.closest('.side-group'); el; el = el.parentElement?.closest('.side-group')) boxOf(el).checked = true;

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
  /* THE CHANGE, not the state: a box that changes is the reader, and it is
     the only thing that is - nothing above writes through `change`, and a
     keyboard reaches the box the same way a click on its caret does. */
  box?.addEventListener('change', (e) => {
    const toggle = e.target;
    if (!toggle.matches?.('.side-toggle')) return;
    const group = toggle.nextElementSibling;
    if (!group || !groups.includes(group)) return;
    chosen[group.dataset.key] = toggle.checked;
    write();
  });

  /* A SECTION'S OWN WORDS FOLD IT WHEN YOU ARE ALREADY THERE.
   *
   * Reported as: pressing a heading in the menu opens the section, pressing
   * the same heading again does not close it. Both true, and the reason is
   * that the words are a LINK. Most sections here point at a page of their
   * own, so the first press went to that page - and the section holding the
   * page you are on is opened by the build and again by the walk above,
   * whatever is stored, because a menu that hid where you are would be worse
   * than one that forgets. The second press went to that same page a second
   * time: a fresh document, the same section, opened again. Only the caret
   * folded it, which is a target of 24 pixels next to twelve characters that
   * look like they should do it.
   *
   * So when the link is the page the reader is ON, it has nothing to open,
   * and it folds the list instead. Anywhere else it stays what it says it is
   * - the way to that section's first page. A press that means "in a new
   * tab" still means that.
   *
   * The box is flipped through a `change` event rather than by hand, so the
   * decision is written down by the one listener above that writes: assigning
   * `checked` fires nothing, and a fold that was not remembered would come
   * back on the next page as though it had not been asked for. */
  const page = (url) => url.pathname.replace(/index\.html$/, '');
  box?.addEventListener('click', (e) => {
    const label = e.target.closest?.('a.side-label');
    if (!label || e.defaultPrevented) return;
    if (e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    if (label.target && label.target !== '_self') return;
    if (page(new URL(label.href)) !== page(location)) return;
    const group = label.closest('.side-group');
    if (!group || !groups.includes(group)) return;
    e.preventDefault();
    const toggle = boxOf(group);
    toggle.checked = !toggle.checked;
    toggle.dispatchEvent(new Event('change', { bubbles: true }));
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

/* ---- the front door's example folds on a phone --------------------------
 *
 * 137 lines of ABAP under a thumb are three screens of scrolling before
 * anything else on the page; on a desk the running frame replaces the
 * listing (playground.js), and on a phone the example does not start
 * itself, so the listing is what a phone sees. Folded to its first screen,
 * with a button that says how much is behind it. Coarse pointer only - a
 * desk never sees the fold, and a reader who presses Run gets the frame
 * over the whole thing either way. */
(function foldOnPhone() {
  if (!matchMedia?.('(pointer: coarse)').matches) return;
  for (const play of document.querySelectorAll('.a2ui5-play[data-play="edit"]')) {
    const block = play.querySelector('div[class*="language-"]');
    if (!block) continue;
    const lines = block.querySelectorAll('.line').length;
    if (lines < 40) continue;
    play.classList.add('is-folded');
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'a2ui5-play-fold';
    const say = () => { button.textContent = play.classList.contains('is-folded') ? `Show all ${lines} lines` : 'Show less'; };
    say();
    button.addEventListener('click', () => {
      play.classList.toggle('is-folded');
      say();
      if (play.classList.contains('is-folded')) block.scrollIntoView({ block: 'start' });
    });
    block.after(button);
  }
})();
