// Where you were, on each of the two sites the bar moves between.
//
// The documentation, the playground and the sample catalogue are three
// deployments on ONE origin (abap2ui5.github.io/docs, /playground and
// /playground/samples), which is what makes this possible at all: they share a
// localStorage the way they now share the theme. Each page writes down where
// it is, and the nav item pointing at the OTHER site is lifted to whatever
// that site wrote last. Docs -> Samples -> Docs comes back to the page you
// left rather than to the front of the manual, and because the catalogue keeps
// its filters in its URL (?q=table&lib=sap.m), a filtered catalogue comes back
// filtered.
//
// This is the documentation's half. The other half is src/shell/site-memory.mjs
// in abap2UI5/playground, which the playground and the catalogue import and
// the per-sample pages carry as an inline copy - same two keys, same checks.
// Three deployments, three copies, kept in step by hand, for the reason the
// bar's own markup is: a shared package between two repositories that deploy
// separately would have to be versioned to say something this simple.
//
// The href written into the markup stays the section's front page. That is
// what a crawler, a reader with no JavaScript and a first visit all get, and
// it is what every step below falls back to - nothing here ever returns a link
// worse than the one it was given.
//
// THE PLAYGROUND IS NOT REMEMBERED, only consulted. Its URL carries the code
// in the editor (?src=...), and a Playground item that reopened yesterday's
// sample instead of an empty editor would be a different promise from the one
// the word makes. Samples and docs are places; the playground is a workbench.

/* The playground's namespace, for keys this site writes too. It is the wrong
 * word for a value shared by three deployments and it is the namespace every
 * other key on this origin already uses - including the theme, which crossed
 * the same line first. One slightly misnamed prefix beats two prefixes that
 * have to be remembered separately. */
const KEY = {
  samples: "abap2ui5-playground:last-samples",
  docs: "abap2ui5-playground:last-docs",
};

/** Write down that the reader is on this site's page. */
export function rememberHere(site = "docs") {
  if (typeof localStorage === "undefined" || !KEY[site]) return;
  try {
    localStorage.setItem(KEY[site], location.pathname + location.search + location.hash);
  } catch {
    /* A refused or full storage. The reader simply is not remembered. */
  }
}

/**
 * The page `site` was last left on, as something an href can be set to - or
 * `fallback`, which is the link as it was written.
 *
 * A stored value is untrusted: anything running on this origin can put
 * anything in it, and a stale one outlives the page it named. So it is not
 * returned, it is CHECKED - resolved against this origin first, and kept only
 * if what comes back is still inside `scope`, which is `fallback`'s own path
 * unless the caller names a wider one.
 *
 * `scope` is for a link written DEEPER than the section it restores inside:
 * the other three bars point Documentation at the first page of the manual and
 * still come back to wherever the reader was in it (`data-scope` on the link
 * over there, src/shell/site-memory.mjs). It is a value the CALLER passes,
 * never one that comes out of storage, so it widens nothing - what may be
 * restored is still declared by this document and still checked against this
 * origin. That is what turns
 * "//elsewhere/x" (a different origin), "/docs/../x" (a path that normalises
 * out of the section) and "javascript:…" (an origin of "null") into three
 * values that are simply ignored.
 */
export function lastVisited(site, fallback, scope = fallback) {
  if (typeof localStorage === "undefined") return fallback;
  try {
    const last = localStorage.getItem(KEY[site]);
    if (!last) return fallback;
    /* The section, from the link that is already written, so this works
     * unchanged on a dev server where the three sites sit at other paths - and
     * so a link to another HOST, which shares no storage and therefore has
     * nothing to restore, falls through the origin test. */
    const base = new URL(scope, location.href);
    if (base.origin !== location.origin) return fallback;
    const target = new URL(last, location.origin);
    if (target.origin !== location.origin) return fallback;
    if (!target.pathname.startsWith(base.pathname)) return fallback;
    return target.pathname + target.search + target.hash;
  } catch {
    return fallback;
  }
}

/* ── WHERE ON THE PAGE, not only which page ─────────────────────────────────
 *
 * The item above comes back to the page you left. It came back to the TOP of
 * it, which on the two pages a reader actually leaves half-read - the manual's
 * long chapters, and a catalogue of 770 rows - is most of the way to not
 * having been remembered at all: the reader who was at sample 400, looked
 * something up in the manual and pressed Samples, arrived at sample 1.
 *
 * So the offset is written down per path, and restored on ARRIVAL BY THE BAR
 * and nowhere else. That last part is the whole design. A page that restored
 * its offset on every load would fight the browser (which already restores it
 * for back and forward) and would surprise a reader who followed a link to a
 * page they happen to have read before - landing them mid-chapter with no
 * scrollbar movement to explain it. The bar says, in one record, "I am sending
 * you back to X"; the page that IS X, and arrives within seconds, honours it.
 * Anything else ignores it.
 *
 * A hash in the URL wins: `#section` is a destination the reader named, and an
 * offset is only ever a memory of one.
 */

const SCROLL_KEY = "abap2ui5-playground:scroll";
const HANDOFF_KEY = "abap2ui5-playground:returning";
/* Enough paths for moving between the four sections without losing any of
 * them, and small enough that the value stays a few hundred bytes. */
const SCROLL_MAX = 12;
/* The bar's click and the page that arrives are one navigation. Half a minute
 * is a slow connection; a record older than that belongs to a journey that
 * ended some other way. */
const HANDOFF_TTL = 30_000;

/** This document, as the scroll map keys it. The hash is deliberately not part
 *  of it: it is one page, wherever in it the reader entered. */
const here = () => location.pathname + location.search;

const readMap = () => {
  try {
    const raw = JSON.parse(localStorage.getItem(SCROLL_KEY) || "{}");
    return raw && typeof raw === "object" && !Array.isArray(raw) ? raw : {};
  } catch {
    return {};
  }
};

/** Write down how far down this page the reader is. Cheap enough to call from
 *  a scroll handler, which is where it is called from - `pagehide` alone loses
 *  the offset whenever a browser decides not to fire it. */
export function rememberScroll(y = window.scrollY, path = here()) {
  if (typeof localStorage === "undefined") return;
  if (!Number.isFinite(y) || y < 0) return;
  try {
    const map = readMap();
    /* Re-inserted, so the key order is least-recently-written first and the
     * oldest is the one that falls off. */
    delete map[path];
    map[path] = Math.round(y);
    for (const old of Object.keys(map).slice(0, -SCROLL_MAX)) delete map[old];
    localStorage.setItem(SCROLL_KEY, JSON.stringify(map));
  } catch {
    /* A refused or full storage. The reader lands at the top, as before. */
  }
}

/** The offset stored for `path`, or 0. Checked, not followed: anything on this
 *  origin can write anything here, and `scrollTo` will take whatever it is
 *  given. */
export function scrollOf(path = here()) {
  if (typeof localStorage === "undefined") return 0;
  try {
    const y = readMap()[path];
    return Number.isFinite(y) && y >= 0 && y < 1e7 ? y : 0;
  } catch {
    return 0;
  }
}

/**
 * "I am sending the reader back to `href`" — written by a bar item as it is
 * clicked, read by the page that arrives.
 *
 * The href is resolved against this origin and stored as a path, so a link to
 * another host (which shares no storage) writes nothing.
 */
export function handOff(href) {
  if (typeof localStorage === "undefined" || !href) return;
  try {
    const to = new URL(href, location.href);
    if (to.origin !== location.origin) return;
    localStorage.setItem(HANDOFF_KEY, JSON.stringify({ to: to.pathname + to.search, at: Date.now() }));
  } catch {
    /* Not a URL, or no storage. Nothing is restored, which is the old
     * behaviour and not a broken one. */
  }
}

/**
 * The offset this page should be restored to, if the bar just sent the reader
 * here — and `null` in every other case, which is most of them.
 *
 * Reading it CONSUMES it: the record describes one arrival, and a second read
 * would be a later navigation inheriting somebody else's destination.
 */
export function takeHandoff() {
  if (typeof localStorage === "undefined") return null;
  let record = null;
  try {
    record = JSON.parse(localStorage.getItem(HANDOFF_KEY) || "null");
    localStorage.removeItem(HANDOFF_KEY);
  } catch {
    return null;
  }
  if (!record || typeof record.to !== "string" || typeof record.at !== "number") return null;
  const age = Date.now() - record.at;
  if (!(age >= 0 && age < HANDOFF_TTL)) return null;
  /* The record has to name THIS page. It is written before a navigation that
   * may not happen (a middle click, a refused link, a reader who went
   * somewhere else instead), so arriving anywhere but at `to` means it was
   * not this journey. */
  if (record.to !== here()) return null;
  /* A destination the reader named beats one this remembered. */
  if (location.hash) return null;
  const y = scrollOf(record.to);
  return y > 0 ? y : null;
}

/**
 * Take the handoff and act on it: scroll to the offset, and keep scrolling to
 * it until it takes.
 *
 * One `scrollTo` is not enough. The router draws the new page and scrolls it
 * to the top itself, not always in the same frame, and a page whose content is
 * still arriving is a page too short to reach the offset - which the browser
 * clamps to the top, the exact thing this is here to stop. So it is re-applied
 * as the page settles, for up to three seconds.
 *
 * It stops the moment the offset takes, and the moment the READER scrolls:
 * a scroll that was not this one is the reader saying where they want to be,
 * and it wins. Without that a page shorter than the stored offset would hold
 * them at the bottom of it.
 */
export function restoreScroll() {
  const y = takeHandoff();
  if (y === null) return;

  /* WHAT CANCELS THIS IS THE READER, AND NOTHING ELSE.
   *
   * It used to stop as soon as `scrollY` was not where the last frame put it,
   * on the theory that a scroll this did not cause is the reader taking over.
   * It is not: a page still loading moves its own scroll. The browser's scroll
   * anchoring shifts the offset to keep the content under your eyes steady as
   * things arrive above it - and the home page, whose hero image is the
   * largest thing on the site to arrive late, is where that happens every
   * time. The offset was applied to a page still a few hundred pixels short,
   * anchoring nudged it, this read the nudge as a reader and gave up. It went
   * "a little way down" and stopped, on that page and no other.
   *
   * So the reader is asked directly. A wheel, a touch, a key, a pointer:
   * those are somebody saying where they want to be, and they win. Layout
   * settling is not one of them. */
  const until = Date.now() + 2000;
  const MOVED = ['wheel', 'touchstart', 'keydown', 'pointerdown'];
  let stopped = false;
  const stop = () => {
    stopped = true;
    for (const e of MOVED) removeEventListener(e, stop, true);
  };
  for (const e of MOVED) addEventListener(e, stop, { capture: true, passive: true });

  /* IT HOLDS THE POSITION, it does not merely reach it. Reaching it once is
   * not enough and stopping there is how this failed: the router draws the
   * page and scrolls it to the top ITSELF, after this - measured, in that
   * order, `scrollTo(0, 1600)` and then `scrollTo(0, 0)` - so the offset was
   * applied, taken away again a frame later, and the loop had already
   * congratulated itself and gone. Two seconds of holding covers that, and a
   * page still growing underneath, and costs about a hundred and twenty
   * frames of one assignment each.
   *
   * Holding is only safe BECAUSE the reader can take it back: the four events
   * above end it on the first wheel, key, touch or pointer - a scrollbar drag
   * included - so nothing is ever fought with. */
  const put = () => {
    if (stopped) return;
    if (Math.round(scrollY) !== y) scrollTo(0, y);
    if (Date.now() < until) requestAnimationFrame(put);
    else stop();
  };
  requestAnimationFrame(put);
}
