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
 * if what comes back is still inside `fallback`'s own path. That is what turns
 * "//elsewhere/x" (a different origin), "/docs/../x" (a path that normalises
 * out of the section) and "javascript:…" (an origin of "null") into three
 * values that are simply ignored.
 */
export function lastVisited(site, fallback) {
  if (typeof localStorage === "undefined") return fallback;
  try {
    const last = localStorage.getItem(KEY[site]);
    if (!last) return fallback;
    /* The section, from the link that is already written, so this works
     * unchanged on a dev server where the three sites sit at other paths - and
     * so a link to another HOST, which shares no storage and therefore has
     * nothing to restore, falls through the origin test. */
    const base = new URL(fallback, location.href);
    if (base.origin !== location.origin) return fallback;
    const target = new URL(last, location.origin);
    if (target.origin !== location.origin) return fallback;
    if (!target.pathname.startsWith(base.pathname)) return fallback;
    return target.pathname + target.search + target.hash;
  } catch {
    return fallback;
  }
}
