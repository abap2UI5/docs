/*
 * What resources/deprecations.md says, as data.
 *
 * The page is the truth and stays hand-written: its table is prose for a
 * person, with links and half-sentences, and a deprecation is a decision
 * somebody makes and writes down there. docs/public/api/deprecations.json is
 * a PROJECTION of that table for tools - the linter, an agent that is asked
 * about a name it learned from a three-year-old example - and it is generated
 * from the table by generate-deprecations.mjs, never typed.
 *
 * A table written for people cannot be read by guessing. `message_box_display( )`
 * stands in a first cell and is not removed (its options are); so does
 * `cs_event-control_by_id`. So the reader is strict and fails towards LEAVING
 * A ROW OUT:
 *
 *   a row is data when its first cell is nothing but code spans, separated by
 *   `,` or `/`, each optionally followed by a remark in parentheses
 *
 * and every other row is prose, which only the page carries. Inside a data
 * row nothing is guessed either: a code span this file cannot classify, a
 * status it cannot read or a moved row without a repository link is a PROBLEM
 * and fails the gate, because the alternative is a fact silently missing from
 * a file whose whole point is to be complete where it speaks.
 *
 * What a code span is:
 *
 *   z2ui5_cl_…  z2ui5_cx_…          class      (a trailing * makes it a pattern)
 *   z2ui5_if_…  z2ui5_if_…=>…        interface
 *   z2ui5_if_…~name                  attribute
 *   cs_event-name                    constant
 *   cs_config-name                   field
 *   Z2UI5_T_…                        dictionary-object
 *   name( )                          method
 *   name( p = … q = … )              parameter p, parameter q   (of: name)
 *   name( s_ctrl-comp )              field s_ctrl-comp          (of: name)
 *   client->name( )-comp             field comp                 (of: name)
 *
 * The status cell: a bare release is `deprecated` since that release;
 * `**removed**, 1.2.3` and `**moved out**, 1.2.3` are what they say;
 * `deprecated 1.2.0, **removed**, 1.2.3` is removed since the second, with
 * the first kept in the note. A removed row that names no release is left
 * out - `since` is not optional in the file.
 *
 * One entry per name. The page lists a few names twice on purpose - first
 * deprecated with a successor, later moved out of the framework - and the
 * entry carries the later status with the earlier row's successor.
 *
 * Pinned by test/deprecations.test.mjs.
 */

export const PAGE = 'docs/resources/deprecations.md';
export const SITE = 'https://abap2ui5.github.io/docs';
export const SOURCE = `${SITE}/resources/deprecations`;
export const SCHEMA = 1;

const HEADING = '## Version status';
const HEADER = ['What you have', 'What to write', 'Status'];
const RELEASE = String.raw`\d+\.\d+\.\d+`;
const RANK = { deprecated: 1, moved: 2, removed: 3 };

/** The cells of one table line; a `\|` is a pipe inside a cell. */
export function cells(line) {
  const inner = line.trim().replace(/^\|/, '').replace(/\|$/, '');
  return inner.split(/(?<!\\)\|/).map((c) => c.trim().replace(/\\\|/g, '|'));
}

/** A link target as an address a reader outside the site can open. */
function absolute(href) {
  if (/^[a-z]+:/i.test(href)) return href;
  if (href.startsWith('#')) return SOURCE + href;
  if (href.startsWith('/')) return SITE + href;
  return new URL(href, `${SITE}/resources/`).href;
}

/** A cell without its markdown: a link becomes `text (address)`. */
export function plain(cell) {
  return cell
    .replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (_, text, href) => `${text} (${absolute(href)})`)
    .replace(/`/g, '')
    .replace(/\*\*/g, '')
    .replace(/(^|\s)\*([^*\s][^*]*)\*(?=\W|$)/g, '$1$2')
    .replace(/\s+/g, ' ')
    .trim();
}

/** The first cell as code spans, or the reason it is prose. */
export function names(cell) {
  const spans = [];
  const separators = new Set();
  let rest = cell.trim();
  while (rest) {
    const span = /^`([^`]+)`/.exec(rest);
    if (!span) return { prose: true };
    rest = rest.slice(span[0].length);
    const remark = /^\s*\(([^()]*)\)/.exec(rest);
    if (remark) rest = rest.slice(remark[0].length);
    spans.push({ code: span[1].trim(), remark: remark ? plain(remark[1]) : '' });
    if (!rest.trim()) break;
    const sep = /^\s*([,/])\s*/.exec(rest);
    if (!sep) return { prose: true };
    separators.add(sep[1]);
    rest = rest.slice(sep[0].length);
    if (!rest) return { prose: true };
  }
  if (!spans.length) return { prose: true };
  return { spans, separator: separators.size === 1 ? [...separators][0] : '' };
}

/** What a code span names: one item, several (parameters), or null. */
export function classify(code) {
  let m;
  if ((m = /^(z2ui5_c[lx]_[a-z0-9_]+)(\*?)$/.exec(code))) {
    return [{ name: m[1] + m[2], kind: 'class', ...(m[2] ? { pattern: true } : {}) }];
  }
  if ((m = /^(z2ui5_if_[a-z0-9_]+)(=>…)?$/.exec(code))) return [{ name: m[1], kind: 'interface' }];
  if (/^z2ui5_if_[a-z0-9_]+~[a-z0-9_]+$/.test(code)) return [{ name: code, kind: 'attribute' }];
  if (/^cs_event-[a-z0-9_]+$/.test(code)) return [{ name: code, kind: 'constant' }];
  if (/^cs_config-[a-z0-9_]+$/.test(code)) return [{ name: code, kind: 'field' }];
  if (/^Z2UI5_T_[A-Z0-9_]+$/.test(code)) return [{ name: code.toLowerCase(), kind: 'dictionary-object' }];
  if ((m = /^([a-z_][a-z0-9_]*)\( \)$/.exec(code))) return [{ name: m[1], kind: 'method' }];
  if ((m = /^client->([a-z_][a-z0-9_]*)\( \)-([a-z0-9_]+)$/.exec(code))) {
    return [{ name: m[2], kind: 'field', of: m[1] }];
  }
  if ((m = /^([a-z_][a-z0-9_]*)\( (.+) \)$/.exec(code))) {
    const [, method, inner] = m;
    if (/^[a-z_][a-z0-9_]*-[a-z0-9_]+$/.test(inner)) return [{ name: inner, kind: 'field', of: method }];
    const params = inner.split(/\s*=\s*…\s*/).filter(Boolean);
    if (/^(?:[a-z_][a-z0-9_]* = …\s*)+$/.test(inner) && params.length) {
      return params.map((name) => ({ name, kind: 'parameter', of: method }));
    }
  }
  return null;
}

/** The status cell. */
export function status(cell) {
  let m;
  if ((m = new RegExp(`^(${RELEASE})$`).exec(cell))) return { status: 'deprecated', since: m[1] };
  if (cell === '**removed**') return { status: 'removed', since: '' };
  if ((m = new RegExp(`^\\*\\*removed\\*\\*, (${RELEASE})$`).exec(cell))) return { status: 'removed', since: m[1] };
  if ((m = new RegExp(`^deprecated (${RELEASE}), \\*\\*removed\\*\\*, (${RELEASE})$`).exec(cell))) {
    return { status: 'removed', since: m[2], deprecated: m[1] };
  }
  if ((m = new RegExp(`^\\*\\*moved out\\*\\*, (${RELEASE})$`).exec(cell))) return { status: 'moved', since: m[1] };
  return null;
}

/** The second cell as one successor per name - only when it is written that
 *  way: as many code spans as the first cell has, with the same separator. */
function pairwise(cell, first) {
  if (!first.separator || first.spans.length < 2) return null;
  const second = names(cell);
  if (second.prose || second.separator !== first.separator) return null;
  if (second.spans.length !== first.spans.length || second.spans.some((s) => s.remark)) return null;
  return second.spans.map((s) => s.code);
}

const keyOf = (e) => [e.kind, e.of ?? '', e.name].join(' ');

/** The rows of the version table, with the line each stands on. */
export function rows(markdown) {
  const lines = markdown.split('\n');
  const start = lines.findIndex((l) => l.trim() === HEADING);
  if (start < 0) return null;
  const out = [];
  let inTable = false;
  for (let i = start + 1; i < lines.length; i++) {
    const line = lines[i];
    if (/^#{1,6} /.test(line)) break;
    if (!line.trim().startsWith('|')) {
      if (inTable) break;
      continue;
    }
    const c = cells(line);
    if (!inTable) {
      if (c.join('|') !== HEADER.join('|')) return null;
      inTable = true;
      i++; // the |---| line
      continue;
    }
    out.push({ line: i + 1, cells: c });
  }
  return inTable ? out : null;
}

/**
 * The page as data.
 *   entries   what goes into the file
 *   skipped   rows only the page carries, each with the reason
 *   problems  rows that look like data and cannot be read - these fail
 */
export function parse(markdown) {
  const table = rows(markdown);
  if (!table) return { entries: [], skipped: [], problems: [`no "${HEADING}" table with the columns ${HEADER.join(' | ')}`] };

  const byKey = new Map();
  const skipped = [];
  const problems = [];

  for (const row of table) {
    const at = `line ${row.line}`;
    if (row.cells.length !== 3) {
      problems.push(`${at}: ${row.cells.length} cells, expected 3`);
      continue;
    }
    const [what, write, state] = row.cells;
    const first = names(what);
    if (first.prose) {
      skipped.push({ line: row.line, cell: what, reason: 'prose in the first cell' });
      continue;
    }
    const st = status(state);
    if (!st) {
      problems.push(`${at}: cannot read the status "${state}"`);
      continue;
    }
    if (!st.since) {
      skipped.push({ line: row.line, cell: what, reason: 'the status names no release' });
      continue;
    }
    let movedTo;
    if (st.status === 'moved') {
      movedTo = /github\.com\/([\w.-]+\/[\w.-]+?)(?:[/)#]|$)/.exec(write)?.[1];
      if (!movedTo) {
        problems.push(`${at}: moved out, and the second cell links no repository`);
        continue;
      }
    }
    const pairs = pairwise(write, first);
    const text = plain(write);

    first.spans.forEach((span, n) => {
      const items = classify(span.code);
      if (!items) {
        problems.push(`${at}: \`${span.code}\` is not a shape this reader knows - name it in a form lib/deprecations.mjs lists, or put prose in the cell to keep the row out of the file`);
        return;
      }
      for (const item of items) {
        const entry = {
          ...item,
          status: st.status,
          replacement: st.status === 'moved' ? '' : pairs ? plain(`\`${pairs[n]}\``) : text,
          ...(movedTo ? { movedTo } : {}),
          since: st.since,
          notes: [
            span.remark,
            st.deprecated ? `deprecated since ${st.deprecated}` : '',
            st.status === 'moved' ? text : '',
          ].filter(Boolean),
          line: row.line,
        };
        const key = keyOf(entry);
        const seen = byKey.get(key);
        if (!seen) {
          byKey.set(key, entry);
          continue;
        }
        if (RANK[seen.status] === RANK[entry.status]) {
          problems.push(`${at}: \`${entry.name}\` is already ${seen.status} on line ${seen.line} - one row per status`);
          continue;
        }
        const [low, high] = RANK[seen.status] < RANK[entry.status] ? [seen, entry] : [entry, seen];
        if (low.status !== 'deprecated') {
          problems.push(`${at}: \`${entry.name}\` is ${low.status} on line ${low.line} and ${high.status} on line ${high.line} - the page has to say which`);
          continue;
        }
        const merged = {
          ...high,
          replacement: high.replacement || low.replacement,
          notes: [...new Set([...low.notes, `deprecated since ${low.since}`, ...high.notes])],
          line: seen.line,
        };
        byKey.set(key, merged);
      }
    });
  }

  const entries = [...byKey.values()].map(({ notes, line, ...e }) => {
    const { name, kind, of, pattern, status: s, replacement, movedTo, since } = e;
    return {
      name,
      kind,
      ...(of ? { of } : {}),
      ...(pattern ? { pattern: true } : {}),
      status: s,
      replacement,
      ...(movedTo ? { movedTo } : {}),
      since,
      ...(notes.length ? { note: notes.join('; ') } : {}),
    };
  });
  return { entries, skipped, problems };
}

/** The file, byte for byte. */
export function render(entries) {
  return `${JSON.stringify({
    schema: SCHEMA,
    source: SOURCE,
    matching: 'An entry with "pattern": true matches every name its * stands for. An exact entry outranks a pattern, a longer pattern a shorter one.',
    entries,
  }, null, 2)}\n`;
}
