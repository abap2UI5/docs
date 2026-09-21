// What a PROSE line of this manual is, and how wide it may be.
//
// The pages are not written to one column. 39 of them are wrapped, 59 are one
// line per paragraph, and 64 are somewhere between - so a site-wide column
// would be a reformat of two thirds of the manual, not a gate. What IS
// decidable, and what actually went wrong twice in a row, is narrower: a page
// that is wrapped stops being wrapped. An editor rewrites a paragraph, the
// browser hands it back as one long line, and the next diff of that paragraph
// is one changed line instead of three - so every later correction to it reads
// as a rewrite.
//
// Hence: a page whose prose is ALREADY within the column has to stay within
// it. A page that is not wrapped is left alone; it can be wrapped later, and
// the day it is, this gate starts holding it too.

export const LIMIT = 80;

// A page counts as wrapped when at most this share of its prose sits over the
// limit. Not zero, because one unwrappable line must not exempt a whole page
// from the rule - those lines are named below instead.
const WRAPPED_BELOW = 0.05;
const MIN_PROSE_LINES = 5;

const GENERATED = [
  // Written by scripts/link-samples.mjs and scripts/generate-api-reference.mjs.
  // Wrapping one of these by hand is a change the next regeneration undoes.
  [/^<!-- samples:start\b/, /^<!-- samples:end -->/],
  [/^<!-- api:start\b/, /^<!-- api:end -->/],
];

// A token nothing can break: a URL, or a root-absolute path of some length.
const UNBREAKABLE = /\S*(?:https?:\/\/|\/[a-z0-9_/-]{12,})\S*/g;

/**
 * The prose lines of a page, as {line, length, text} - everything the column
 * rule applies to, and nothing else.
 */
export function proseLines(source) {
  const lines = source.split('\n');
  const out = [];
  let fenced = false;
  let frontmatter = lines[0] === '---';
  let generatedEnd = null;
  let lastAlt = null;

  for (let i = 0; i < lines.length; i++) {
    const text = lines[i].replace(/\r$/, '');
    const trimmed = text.trimStart();

    if (frontmatter) {
      if (i > 0 && text === '---') frontmatter = false;
      continue;
    }
    if (generatedEnd) {
      if (generatedEnd.test(trimmed)) generatedEnd = null;
      continue;
    }
    if (trimmed.startsWith('```') || trimmed.startsWith('~~~')) {
      fenced = !fenced;
      continue;
    }
    if (fenced || !trimmed) continue;

    const opens = GENERATED.find(([start]) => start.test(trimmed));
    if (opens) { generatedEnd = opens[1]; continue; }

    // A table row wraps at the cell, not at the column; HTML is markup, not
    // prose; an image carries its alt text on one line by construction.
    if (trimmed.startsWith('|') || trimmed.startsWith('<')) continue;

    // ::: tip / ::: warning / ::: details and their closing line are VitePress
    // container directives. Each has to stand on a line of its own.
    if (trimmed.startsWith(':::')) continue;

    // A heading is one line by construction - there is no wrapping it.
    if (trimmed.startsWith('#')) continue;

    // An image carries its alt text on one line by construction.
    const image = /^!\[(.*)\]\(/.exec(trimmed);
    if (image) { lastAlt = image[1]; continue; }

    // ...and the caption under it repeats that alt text word for word, on
    // purpose: the two are meant to be comparable at a glance. Wrapping the
    // caption alone would end that. Only a caption that IS the alt text is
    // exempt - an ordinary italic line is prose like any other.
    const caption = /^\*(.+)\*$/.exec(trimmed);
    if (caption && lastAlt && caption[1].trim() === lastAlt.trim()) continue;

    // A line that is only over because of one thing nobody can break.
    const longest = Math.max(0, ...(text.match(UNBREAKABLE) ?? []).map((t) => t.length));
    if (longest > 20 && text.length - longest <= LIMIT) continue;

    out.push({ line: i + 1, length: text.length, text });
  }
  return out;
}

/**
 * How this page stands: whether the column rule applies to it at all, and
 * which of its lines break it.
 */
export function judge(source) {
  const prose = proseLines(source);
  const over = prose.filter((l) => l.length > LIMIT);
  if (prose.length < MIN_PROSE_LINES) {
    return { wrapped: false, reason: 'too few prose lines', prose, over: [] };
  }
  const wrapped = over.length / prose.length < WRAPPED_BELOW;
  return { wrapped, prose, over: wrapped ? over : [] };
}

// --- rewrapping -----------------------------------------------------------

// Things that must not be split across a line break: a code span, a link (the
// `](` in the middle of it is the whole point), an image, and a bare URL.
const ATOMIC = /`[^`]*`|!?\[[^\]]*\]\([^)]*\)|\S*https?:\/\/\S*/g;

// Split on whitespace - but never on whitespace that sits INSIDE one of those
// constructs, and never between one of them and the punctuation attached to it.
// `ele( )` holds a space and is one word; so does [a link](url); and the comma
// after a code span belongs to the same token as the span.
function tokenize(text) {
  const inside = new Array(text.length).fill(false);
  for (const m of text.matchAll(ATOMIC)) {
    for (let i = m.index; i < m.index + m[0].length; i++) inside[i] = true;
  }
  const tokens = [];
  let current = '';
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (/\s/.test(c) && !inside[i]) {
      if (current) { tokens.push(current); current = ''; }
    } else {
      current += c;
    }
  }
  if (current) tokens.push(current);
  return tokens;
}

/** The paragraph around `index`, as [from, to] inclusive line indices. */
function paragraphAround(lines, index) {
  const holds = (l) => {
    const t = l.trimStart();
    return t && !t.startsWith('```') && !t.startsWith('~~~') && !t.startsWith('|') &&
      !t.startsWith('<') && !t.startsWith('#') && !t.startsWith(':::') &&
      !/^!\[/.test(t);
  };
  let from = index;
  let to = index;
  // A list item or a quote starts its own block - do not swallow the one above.
  const starts = (l) => /^\s*(?:[-*+]|\d+\.|>)\s/.test(l);
  while (from > 0 && holds(lines[from - 1]) && !starts(lines[from])) from--;
  while (to < lines.length - 1 && holds(lines[to + 1]) && !starts(lines[to + 1])) to++;
  return [from, to];
}

/**
 * Rewrap the paragraphs that hold the given 1-based line numbers. Returns the
 * new source, or null when nothing could be changed.
 */
export function rewrap(source, lineNumbers) {
  const lines = source.split('\n');
  const done = new Set();
  let touched = false;

  for (const number of [...lineNumbers].sort((a, b) => b - a)) {
    const index = number - 1;
    if (done.has(index)) continue;
    const [from, to] = paragraphAround(lines, index);
    for (let i = from; i <= to; i++) done.add(i);

    const first = lines[from];
    const marker = /^(\s*(?:[-*+]|\d+\.|>)\s+)/.exec(first);
    const lead = marker ? marker[1] : (/^\s*/.exec(first)?.[0] ?? '');
    const hang = marker ? ' '.repeat(marker[1].length) : lead;

    const text = lines.slice(from, to + 1)
      .map((l, i) => (i === 0 ? l.slice(lead.length) : l.trimStart()))
      .join(' ');

    const out = [];
    let current = lead;
    let empty = true;
    for (const token of tokenize(text)) {
      const candidate = empty ? current + token : `${current} ${token}`;
      if (!empty && candidate.length > LIMIT) {
        out.push(current);
        current = hang + token;
      } else {
        current = candidate;
        empty = false;
      }
    }
    if (!empty) out.push(current);

    if (out.join('\n') !== lines.slice(from, to + 1).join('\n')) {
      lines.splice(from, to - from + 1, ...out);
      touched = true;
    }
  }
  return touched ? lines.join('\n') : null;
}
