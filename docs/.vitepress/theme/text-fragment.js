/*
 * A link to the WORDS a reader selected, rather than to the lines they are on.
 *
 * The per-sample pages in abap2UI5/playground answer "look at line 40 to 55"
 * with #L40-L55: one class per page, printed whole, so a line number is an
 * address. A page of this manual is not that. It carries a dozen fences and a
 * page of prose around them, and — this is the part that decides it — nothing
 * here has a commit behind it. GitHub's line links are permalinks because they
 * name a SHA; a line number on a documentation page names whatever is on that
 * line today, and the next correction to the example silently moves it. A link
 * somebody pasted into an issue would then point at the wrong line, and
 * nothing would be red.
 *
 * So the link names the text instead, using the browser's own text fragments:
 *
 *   .../get_started/quickstart.html#:~:text=INTERFACES%20z2ui5_if_app
 *
 * The browser finds those words and highlights them. Correct an example above
 * it and the link still lands; delete the words themselves and it lands at the
 * top of the page, which is the one failure mode of a link that has no id to
 * miss. Nothing is stored, nothing is generated, and no markup is added to a
 * single fence.
 *
 * This file is the part with no DOM in it — what to quote, and how to spell
 * it — so it can be checked as a unit (test/text-fragment.test.mjs).
 * theme/link-to-selection.js is the browser half.
 *
 * The directive it writes is the standard one:
 *
 *   text=[prefix-,]textStart[,textEnd][,-suffix]
 *
 * and the four decisions in it are:
 *
 *  - A SHORT selection is quoted whole. Anything longer is quoted as its first
 *    and last few words, because a directive carrying two paragraphs is a URL
 *    nobody can paste into a chat window.
 *  - A selection that CROSSES LINES is always quoted as start and end, however
 *    short it is. An exact match has to be found inside one block, and every
 *    line of a highlighted code fence is its own element — a two-line quote
 *    would match nothing at all. start,end is the form that spans blocks; that
 *    is what it is for.
 *  - The words are compared with their whitespace collapsed, which is how the
 *    fragment itself is matched: an indented chain selected out of a fence is
 *    the same text as the one line the directive spells.
 *  - The match begins at the first place textStart occurs, so a textStart the
 *    page carries twice is an address for the wrong one. When that happens the
 *    words in front of the selection are added as a prefix, and if the page
 *    repeats those too, the words after it as a suffix.
 */

/** Whitespace as a text fragment matches it: one space, and none at the ends. */
export const normalise = (text) => String(text ?? "").replace(/\s+/g, " ").trim();

/* What has to be escaped inside a part of the directive. encodeURIComponent
 * already takes "," and "&"; "-" it leaves alone, because "-" is unreserved in
 * a URL — and it is exactly the character that marks a part as the prefix or
 * the suffix, so a hyphen inside the quoted words has to go by hand. */
const enc = (part) => encodeURIComponent(part).replace(/-/g, "%2D");

const words = (text) => (text === "" ? [] : text.split(" "));

/** Words of the text before and after the selection, when one is needed. */
const CONTEXT = 5;
/** Words quoted at each end of a selection too long to quote whole. */
const EDGE = 5;
/** A selection of at most this many words is quoted whole. */
const EXACT = 12;

/** How many places in the page this run of words occurs. */
const count = (page, needle) => (needle === "" ? 0 : page.split(needle).length - 1);

/**
 * The `text=` directive for a selection, or null when there is nothing to
 * quote. `prefix` and `suffix` are the text around the selection inside the
 * same block, and `pageText` the page it was made on — both are only read to
 * decide whether the quote is ambiguous, and a caller that passes neither gets
 * the shortest directive that describes the selection.
 */
export function fragmentFor({ text, prefix = "", suffix = "", pageText = "" } = {}) {
  const selection = normalise(text);
  if (selection === "") return null;

  const page = normalise(pageText);
  const all = words(selection);
  /* Across lines, or simply long: quoted as its two ends. The halves never
   * overlap - a textEnd is looked for AFTER textStart, so a short selection is
   * split down the middle rather than quoted twice. */
  const spans = /\n/.test(String(text)) || all.length > EXACT;
  const half = Math.min(EDGE, Math.floor(all.length / 2));

  const start = spans && half > 0 ? all.slice(0, half).join(" ") : selection;
  const end = spans && half > 0 ? all.slice(all.length - half).join(" ") : "";

  /* Where the match BEGINS is what a repeated quote gets wrong, so it is the
   * start that has to be made unique: first with the words in front of it,
   * then - for a quote whose end is its own end - with the words after. */
  let before = "";
  let after = "";
  if (page !== "" && count(page, start) > 1) {
    before = words(normalise(prefix)).slice(-CONTEXT).join(" ");
    if (before === "" || count(page, `${before} ${start}`) > 1) {
      after = words(normalise(suffix)).slice(0, CONTEXT).join(" ");
    }
  }

  return [
    "text=",
    before === "" ? "" : `${enc(before)}-,`,
    enc(start),
    end === "" ? "" : `,${enc(end)}`,
    after === "" ? "" : `,-${enc(after)}`,
  ].join("");
}

/**
 * The parts of a `:~:text=` directive in a URL, or null when it carries none.
 * The browsers that support text fragments never need this — they find the
 * words themselves; it is read by the fallback in link-to-selection.js, which
 * is what a browser too old for them gets instead of a link that does nothing.
 */
export function parseDirective(url) {
  const at = String(url ?? "").indexOf(":~:");
  if (at === -1) return null;
  const directive = String(url).slice(at + 3).split("&").find((part) => part.startsWith("text="));
  if (directive === undefined) return null;

  const parts = directive.slice("text=".length).split(",").filter((part) => part !== "");
  if (parts.length === 0) return null;
  const decode = (part) => {
    try {
      return decodeURIComponent(part);
    } catch {
      /* Somebody else's URL, hand-edited or truncated in a chat window. */
      return part;
    }
  };

  let prefix = "";
  let suffix = "";
  if (parts.length > 1 && parts[0].endsWith("-")) prefix = decode(parts.shift().slice(0, -1));
  if (parts.length > 1 && parts[parts.length - 1].startsWith("-")) suffix = decode(parts.pop().slice(1));

  return { prefix, start: decode(parts[0] ?? ""), end: parts.length > 1 ? decode(parts[1]) : "", suffix };
}
