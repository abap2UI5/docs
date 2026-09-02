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
 * So the wrapper is made here, where it can actually be made.
 *
 * The table is rendered through `self.renderToken` rather than written out as
 * a literal `<table>`: a literal drops whatever attributes markdown-it and its
 * plugins put on the token, and a rule that silently discards other people's
 * output is a trap for whoever adds the next plugin.
 *
 * `tabindex` is the one attribute that deliberately MOVES. VitePress puts it
 * on the table because in its layout the table is what scrolls, and a
 * scrollable box has to be reachable from the keyboard or it can only be read
 * with a mouse. Here the wrapper scrolls, so the wrapper is what needs to be
 * focusable — and leaving it on both would put two tab stops on one table.
 */
export function tableScroll(md) {
  const open = md.renderer.rules.table_open;
  const close = md.renderer.rules.table_close;

  md.renderer.rules.table_open = (tokens, idx, options, env, self) => {
    const inner = open ? open(tokens, idx, options, env, self) : self.renderToken(tokens, idx, options);
    // The `tabindex` is not on the token — VitePress writes it in its own
    // `table_open` renderer, so it arrives here already inside the string. It
    // is taken off that string rather than left in place: two tab stops for
    // one table is worse than one, and the inner one would land on an element
    // that no longer scrolls. Anchored and single, so it can only ever touch
    // the opening tag this function produced a line above.
    return `<div class="vp-doc-table" tabindex="0">\n${inner.replace(/^<table tabindex="0"/, '<table')}`;
  };

  md.renderer.rules.table_close = (tokens, idx, options, env, self) => {
    const inner = close ? close(tokens, idx, options, env, self) : self.renderToken(tokens, idx, options);
    return `${inner}</div>\n`;
  };
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
 * Two things it must NOT do, and the first version of this did both:
 *
 *   - A fence inside a `::: code-group` already has its label drawn, as the
 *     tab of the group. Wrapping it again printed `ABAP` twice on
 *     `get_started/quickstart.md`, once as the group's tab and once as a title
 *     bar inside it. So the core rule below tracks the container depth and
 *     leaves anything inside a group alone.
 *
 *   - Not every bracketed title is a filename. This documentation also uses
 *     them as plain labels — `[lcl_help]` over the local helper class on
 *     `translation_i18n.md` — and a label is not a path. So a title only
 *     becomes a filename tab if it looks like a file: a dot or a slash in it.
 *     That also keeps this off the 838 ABAP examples for good, which is the
 *     point: an abap2UI5 example is a global class whose name is the first
 *     token of its own first line, pasted into a system rather than saved to a
 *     path, so a tab repeating it is the same word twice.
 *
 * Both were caught in review rather than by any gate here, which is worth
 * remembering: `check:examples` reads the ABAP, and nothing reads the shape of
 * the HTML around it.
 */
const TITLE = /\[(.+?)\]/;
const LOOKS_LIKE_A_FILE = /[./]/;

export function codeTitle(md) {
  /* The title has to be taken in the CORE phase, not in the fence renderer.
   * VitePress strips `[…]` off `token.info` before its own fence rule runs —
   * by the time any renderer override sees the token, `info` is just `xml`.
   * Core rules all run before rendering, so one pushed here still sees the
   * info string as it was written, and parks the title on `token.meta` where
   * the renderer below can find it. */
  md.core.ruler.push('a2ui5_code_title', (state) => {
    let insideGroup = 0;
    for (const token of state.tokens) {
      if (token.type === 'container_code-group_open') insideGroup++;
      else if (token.type === 'container_code-group_close') insideGroup--;
      if (token.type !== 'fence' || insideGroup > 0) continue;
      const match = TITLE.exec(token.info || '');
      if (!match) continue;
      const title = match[1].trim();
      if (!LOOKS_LIKE_A_FILE.test(title)) continue;
      token.meta = { ...(token.meta || {}), a2ui5Title: title };
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
