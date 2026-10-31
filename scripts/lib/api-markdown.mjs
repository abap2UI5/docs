/*
 * api-markdown — the few escapes a generated reference block needs, shared
 * by the two renderers that write one: the client API (resources/api.md,
 * from z2ui5_if_client) and the user exit (advanced/extensibility/
 * user_exits.md, from z2ui5_if_ui5_exit). The parsed model carries PLAIN
 * text; this is where it becomes VitePress markdown.
 */

/* The EMPTY ABAP literal, `` - which the interface's ABAP-Doc writes the way
 * ABAP writes it ("abap_bool as `X`/``", "keeps its slot as ``"). In markdown
 * two backticks OPEN a code span, and it runs to the next `` in the
 * paragraph: on start_timer and binding_call that is the opening of the
 * example after it, so the page printed the sentence as code and the example
 * as prose with every literal stripped out - `VALUE #( (  ) (  ) (  ) )`. A
 * `` that stands alone between spaces or punctuation is the literal, and is
 * written as a code span of its own, three backticks either side. An
 * opening `` is followed by its code and a closing one preceded by it, so
 * neither matches. */
export const EMPTY_LITERAL = /(^|[\s(/])``(?=$|[\s.,;:)/])/g;

/* An inline code span as markdown reads it: a run of backticks up to the next
 * run of the same length. Splitting on single backticks took a
 * double-backtick example apart at its first literal, so a `<` in the rest of
 * it was escaped INSIDE the span and printed as `&lt;`. */
export const CODE_SPAN = /(?<!`)(`+)(?!`)[\s\S]*?[^`]\1(?!`)/g;

/** `<` becomes an HTML tag in VitePress prose, but stays literal inside an
 *  inline-code span - so escape around the spans, never inside them. */
export const prose = (s) => {
  const text = s.replace(EMPTY_LITERAL, '$1``` `` ```');
  let out = '';
  let at = 0;
  for (const span of text.matchAll(CODE_SPAN)) {
    out += text.slice(at, span.index).replace(/</g, '&lt;') + span[0];
    at = span.index + span[0].length;
  }
  return out + text.slice(at).replace(/</g, '&lt;');
};

/** A table cell: prose, on one line, with every `|` escaped (GFM reads an
 *  escaped pipe as a pipe even inside a code span). */
export const cell = (s) => prose(s).replace(/\|/g, '\\|').replace(/\s*\r?\n\s*/g, ' ');

/** `text` as an inline code span, however many backticks it holds itself:
 *  one more than its longest run, padded so a backtick at either end does
 *  not fuse with the delimiter. */
export function codeSpan(text) {
  const longest = Math.max(0, ...[...text.matchAll(/`+/g)].map((m) => m[0].length));
  const fence = '`'.repeat(longest + 1);
  const pad = longest ? ' ' : '';
  return `${fence}${pad}${text}${pad}${fence}`;
}
