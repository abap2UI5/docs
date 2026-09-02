/*
 * Two markdown-it rules that VitePress does not provide, and that this site
 * turned out to need. Both are renderer overrides in the same shape as
 * `playground.mjs`, and both exist because a claim made in CSS was not true of
 * the HTML underneath it.
 */

/*
 * 1. A scroll box around every table.
 *
 * VitePress 1.6.4 emits a bare `<table tabindex="0">` with no wrapper, and its
 * own stylesheet handles a wide one with `display: block; overflow-x: auto` on
 * the table itself. That scrolls, and it also makes the table shrink to its
 * CONTENT width — which is why the four-column parameter tables on
 * `resources/api.md` used to sit at about 40% of the column with the rest of
 * the page empty beside them.
 *
 * The fix for that was `display: table; width: 100%`, and it came with a
 * promise that the scroll had moved to a `.vp-doc-table` wrapper. It had not.
 * No such wrapper exists in this version of VitePress; the selector matched
 * nothing, and the tables simply lost their scroll. Nothing showed while the
 * column was 830px wide, because no table was wider than that. At 640px the
 * widest are, and a 900px window scrolled sideways.
 *
 * So the wrapper is made here, where it can actually be made, and the CSS rule
 * that names it becomes true.
 */
export function tableScroll(md) {
  md.renderer.rules.table_open = () => '<div class="vp-doc-table">\n<table>\n';
  md.renderer.rules.table_close = () => '</table>\n</div>\n';
}

/*
 * 2. A filename above a code block.
 *
 * ```cds [srv/travel-flows.cds] is valid VitePress markdown, and in a plain
 * fence the title is parsed off the info string and then DROPPED — only
 * `::: code-group` renders it, as a tab label. So a single block cannot say
 * which file it is without being wrapped in a one-tab group, which is a lot of
 * markup for a label.
 *
 * This renders it as the same tab the group would draw, so the two look alike
 * and a block can be promoted into a group later without the page changing
 * shape.
 *
 * Deliberately NOT applied to the ABAP examples, which are the overwhelming
 * majority here: an abap2UI5 example is a global class, its name is the first
 * token of its own first line, and it is pasted into a system rather than
 * saved to a path. A tab reading `zcl_app_hello_world.clas.abap` over a block
 * that opens `CLASS zcl_app_hello_world DEFINITION PUBLIC.` is the same word
 * twice. The blocks that get one are the ones that really are files — a
 * manifest, a controller, a workflow, a linter config — where the path is
 * information the code itself does not carry.
 */
const TITLE = /\[(.+)\]/;

export function codeTitle(md) {
  /* The title has to be taken in the CORE phase, not in the fence renderer.
   * VitePress strips `[…]` off `token.info` before its own fence rule runs —
   * by the time any renderer override sees the token, `info` is just `xml`.
   * Core rules all run before rendering, so one pushed here still sees the
   * info string as it was written, and parks the title on `token.meta` where
   * the renderer below can find it. */
  md.core.ruler.push('a2ui5_code_title', (state) => {
    for (const token of state.tokens) {
      if (token.type !== 'fence') continue;
      const match = TITLE.exec(token.info || '');
      if (match) token.meta = { ...(token.meta || {}), a2ui5Title: match[1].trim() };
    }
    return true;
  });

  const fence = md.renderer.rules.fence;
  md.renderer.rules.fence = (tokens, idx, options, env, self) => {
    const rendered = fence(tokens, idx, options, env, self);
    const title = tokens[idx].meta?.a2ui5Title;
    if (!title) return rendered;
    return `<div class="a2ui5-titled"><div class="a2ui5-titled-name"><span>${md.utils.escapeHtml(title)}</span></div>${rendered}</div>`;
  };
}
