/*
 * design — the values four documents have to agree on, and where each one
 * keeps them.
 *
 * The documentation, the playground, the sample catalogue and the per-sample
 * pages are four deployments wearing one bar. The palette, the two type stacks
 * and the two radii behind that bar are COPIED between the repositories by
 * hand — deliberately: a stylesheet fetched across two deployments would be a
 * request in front of the first paint, and a shared package between two
 * repositories that release separately would have to be versioned to say
 * something this simple.
 *
 * What was never true is that anything checked the copies. They agreed because
 * whoever last touched one remembered the other, and the day that stopped
 * being true nothing would have said so: a bar in two greys reads as a bug in
 * the OTHER site, which is exactly how the router's 404 read. One value drifted
 * already and was found by eye — the two font stacks led with different
 * families, which is the same face on macOS and Windows and two different ones
 * on Linux, so the same four words in the same bar measured 59/122/78/97px on
 * one site and 65/141/87/110 on the next.
 *
 * So the copies are declared here and compared by scripts/check-design.mjs.
 * The playground's half is `src/catalogue/catalogue.css` (and `shell.css`,
 * which carries the same block); a checkout of it is optional, and without one
 * the check says so rather than passing quietly.
 */
import fs from 'node:fs';
import path from 'node:path';

/** Where a playground checkout might be, in the order countCatalogue looks. */
const HOMES = ['PLAYGROUND_HOME', '.playground', '../playground'];

/** The published copy, for a run with no checkout at hand. */
const PUBLISHED = 'https://abap2ui5.github.io/playground/samples/catalogue.css';

/**
 * The values both sides declare, by the name each side gives them.
 *
 * `ours` is the custom property in docs/.vitepress/theme/style.css; `theirs` is
 * the one in catalogue.css. They are not always spelled the same - this site
 * writes the type stacks as VitePress's own variables, because that is what
 * the theme reads - and that is the whole reason this table exists rather than
 * a diff of two files.
 */
export const SHARED = [
  { ours: '--bg', theirs: '--bg', what: 'the page' },
  { ours: '--bg-sunken', theirs: '--bg-sunken', what: 'the bar, and anything recessed into it' },
  { ours: '--fg', theirs: '--fg', what: 'text' },
  { ours: '--fg-dim', theirs: '--fg-dim', what: 'text that is not the point' },
  { ours: '--line', theirs: '--line', what: 'every hairline' },
  { ours: '--accent', theirs: '--accent', what: 'a link, a button, the mark on the one you are on' },
  { ours: '--accent-fg', theirs: '--accent-fg', what: 'text on the accent' },
  { ours: '--vp-font-family-base', theirs: '--font-ui', what: 'the type the bar is set in' },
  { ours: '--vp-font-family-mono', theirs: '--font-mono', what: 'the type ABAP is set in' },
  { ours: '--radius-control', theirs: '--radius-control', what: 'a pill, a button, an input, the search box' },
  { ours: '--radius-surface', theirs: '--radius-surface', what: 'a panel, a dialog' },
];

/* Both sides declare the palette twice - once for light and once for dark -
 * and the dark half is what a reader crossing at night sees, so both are
 * compared. They do NOT declare it the same way, and that is not drift: this
 * site is a VitePress application whose appearance script puts `.dark` on the
 * root, and the catalogue is a static page that follows the system unless a
 * choice is stored, so its dark values live under a media query AND under
 * `[data-theme="dark"]`. Two mechanisms, one set of values - which is exactly
 * why this table names the block per side instead of diffing two files.
 *
 * The playground's `[data-theme="dark"]` block is the one read: it is the
 * explicit choice, and it carries the same values as the media query above it.
 */
const BLOCK = {
  docs: {
    light: /^:root[^{]*\{([\s\S]*?)^\}/gm,
    dark: /^\.dark[^{]*\{([\s\S]*?)^\}/gm,
  },
  playground: {
    light: /^:root\s*\{([\s\S]*?)^\}/gm,
    dark: /^:root\[data-theme=["']dark["']\]\s*\{([\s\S]*?)^\}/gm,
  },
};

/** Every `--name: value;` in a block of CSS, whitespace flattened so that a
 *  value wrapped over three lines compares equal to the same value on one. */
export function declarations(css) {
  const out = new Map();
  for (const m of css.matchAll(/(--[a-z0-9-]+)\s*:\s*([^;]+);/gi)) {
    out.set(m[1], m[2].replace(/\s+/g, ' ').trim());
  }
  return out;
}

/** The light and dark declarations of one stylesheet. `side` is which of the
 *  two mechanisms above to read it by. */
export function palette(css, side) {
  const blocks = BLOCK[side];
  if (!blocks) throw new Error(`no palette blocks known for ${side}`);
  /* EVERY block of that shape, merged, not the first one. A stylesheet may
   * open `:root` more than once - this one does: the palette is one block and
   * the type and the radii are another, four hundred lines apart - and a walk
   * that stopped at the first would report the rest as "not declared", which
   * is drift that is not there. `[^{]*` so that a grouped selector
   * (`:root, .dark {`) is read as both. */
  const read = (which) => {
    const out = new Map();
    for (const m of css.matchAll(blocks[which])) {
      for (const [k, v] of declarations(m[1])) out.set(k, v);
    }
    return out;
  };
  return { light: read('light'), dark: read('dark') };
}

/** A playground checkout's catalogue.css, or null. `source` says which. */
export async function theirStylesheet(root, { fetchFn = globalThis.fetch } = {}) {
  for (const dir of HOMES) {
    const at = dir.endsWith('_HOME') ? process.env[dir] : path.join(root, dir);
    if (!at) continue;
    const file = path.join(at, 'src', 'catalogue', 'catalogue.css');
    if (fs.existsSync(file)) return { css: fs.readFileSync(file, 'utf8'), source: `checkout ${path.relative(root, at) || at}` };
  }
  try {
    const res = await fetchFn(PUBLISHED, { signal: AbortSignal.timeout(10_000) });
    if (res && res.ok) return { css: await res.text(), source: 'the published catalogue.css' };
  } catch { /* no network, no comparison - the caller reports that */ }
  return null;
}

/**
 * What the two sides disagree about: `[{ what, ours, theirs, mine, yours }]`,
 * empty when they agree. A value missing on either side is a disagreement too
 * - a token that was renamed on one side is exactly the drift this looks for.
 */
export function compare(mine, theirs) {
  const out = [];
  for (const scheme of ['light', 'dark']) {
    for (const row of SHARED) {
      /* THE EFFECTIVE VALUE, WHICH IS WHAT A READER SEES. A custom property
       * that the dark block does not redeclare keeps its light value - that is
       * how the cascade works, and it is how both sides write the values that
       * do not change with the scheme (the two type stacks, the two radii).
       * One side happens to declare them in a `:root, .dark` group and the
       * other in `:root` alone; comparing the blocks literally reported four
       * differences where a reader sees none. */
      const a = mine[scheme].get(row.ours) ?? mine.light.get(row.ours);
      const b = theirs[scheme].get(row.theirs) ?? theirs.light.get(row.theirs);
      if (a === undefined && b === undefined) continue;
      if (a === b) continue;
      out.push({ scheme, ...row, mine: a ?? '(not declared)', yours: b ?? '(not declared)' });
    }
  }
  return out;
}
