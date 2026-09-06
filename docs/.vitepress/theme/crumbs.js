/*
 * Where in the manual a page stands, worked out from the sidebar.
 *
 * The half with no Vue in it, for the same reason `text-fragment.js` and
 * `search-engine.js` are: this is the part that can be WRONG in a way nobody
 * sees — a trail that quietly loses a level, or gains one, on a page nobody
 * opened this week. `Crumbs.vue` is the markup around it, and
 * `test/crumbs.test.mjs` is what holds this file to the sidebar's actual
 * shapes.
 */

/** The manual's first page — what the word Documentation opens on all four
 *  bars of this project (`DOCS` in SiteNav.vue). The first crumb is always
 *  this, including on a page the sidebar does not name. */
export const DOCS = '/get_started/about'

/** One spelling for a page, so a sidebar entry and the page being rendered can
 *  be compared at all: no extension, no `index`, no trailing slash, and one
 *  slash at the front. The sidebar writes `/tutorials/walkthrough/` and the
 *  file is `tutorials/walkthrough/index.md`; both have to end as the same
 *  string. The leading slash is COLLAPSED rather than assumed, because the
 *  trail is built by putting one in front of a `relativePath` — and a value
 *  that already carried one would otherwise match nothing, which reads as a
 *  page the sidebar does not name rather than as the mistake it is. */
export function key(path) {
  return (
    String(path || '')
      .split(/[#?]/)[0]
      .replace(/^\/+/, '/')
      .replace(/\.(md|html)$/, '')
      .replace(/(^|\/)index$/, '$1')
      .replace(/\/+$/, '') || '/'
  )
}

/* The DEEPEST entry that opens the page wins, and several do by design: a
 * section's own `link` is its first chapter's, so /cookbook/view/definition is
 * named by Cookbook, by View and by Definition. Taking the first match would
 * file a page three levels down under "Documentation" alone; taking the
 * longest gives it "Documentation › Cookbook › View".
 *
 * Returns the whole chain including the match; the caller drops its last
 * element, because the page's own name is the heading directly underneath. */
function deepest(items, path, above) {
  let best = null
  for (const item of items || []) {
    const mine = [...above, item]
    if (key(item.link) === path && (!best || mine.length > best.length)) best = mine
    const deeper = deepest(item.items, path, mine)
    if (deeper && (!best || deeper.length > best.length)) best = deeper
  }
  return best
}

/* A sidebar is an array here and has been since the manual had one sidebar for
 * all of it; VitePress also allows an object keyed by path prefix, and if this
 * repository ever grows a second sidebar the entries under the LONGEST
 * matching prefix are the ones to walk. */
function sectionsOf(sidebar, path) {
  if (Array.isArray(sidebar)) return sidebar
  if (!sidebar || typeof sidebar !== 'object') return []
  const groups = Object.entries(sidebar)
    .filter(([prefix]) => path.startsWith(key(prefix)))
    .sort((a, b) => b[0].length - a[0].length)
  return (groups[0] || Object.entries(sidebar)[0] || [, []])[1] || []
}

/**
 * The trail for one page, outermost first: `[{ text, link }, …]`.
 *
 * `link` is absent on a crumb the sidebar gives nowhere to open — "Quickstart"
 * and "Walkthrough" are labels over a list of steps, not pages — and such a
 * crumb is drawn as plain text, which is what the catalogue does with its own
 * last one.
 *
 * @param {unknown} sidebar `themeConfig.sidebar`, as an array or as an object
 * @param {string} page the page's `relativePath`, e.g. `cookbook/model/trees.md`
 */
export function trailFor(sidebar, page) {
  const path = key('/' + String(page || ''))
  const found = deepest(sectionsOf(sidebar, path), path, []) || []
  return [
    { text: 'Documentation', link: DOCS },
    ...found.slice(0, -1).map(({ text, link }) => (link ? { text, link } : { text })),
  ]
}
