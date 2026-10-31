/*
 * user-exit — the user exit as a generated block, the way the client API is.
 *
 * z2ui5_if_ui5_exit is the framework's second public interface: two methods,
 * each handed a context and a configuration structure, and every field of
 * those structures documented in the interface's own source - twenty lines on
 * the CSP tag, seventeen on X-Forwarded-Host. The page used to retype a
 * handful of the fields and name no defaults at all, and a hand-written list
 * of fields is wrong the release after it is written, which is the samples-
 * block argument and the api.md argument once more.
 *
 * Two sources go into the block, both read at the ref every API gate uses:
 *
 *   src/02/z2ui5_if_ui5_exit.intf.abap        the methods, the parameters, the
 *                                             fields with their types and the
 *                                             comment written on each
 *   src/01/04/z2ui5_cl_ui5_user_exit.clas.abap  the DEFAULTS: the shipped exit
 *                                             seeds `cs_config-…` before a
 *                                             customer exit runs, so what it
 *                                             assigns is what an exit that
 *                                             sets nothing gets
 *
 * The defaults are read out of the class, not retyped: every top-level
 * `cs_config-field = …` statement of the two interface methods, with a
 * literal, abap_true/abap_false, a constant of the method or the class, a
 * VALUE #( ) table of n/v pairs, or - for the CSP tag, which is assembled
 * from a template - the name of the expression, so the page can say "built
 * by the framework" instead of guessing. A field no statement assigns is
 * initial, and the page says so by type.
 *
 * Pinned by test/user-exit.test.mjs.
 */
import { cell, codeSpan, prose } from './api-markdown.mjs';

/** The floor for the interface: fewer than this and the parser has stopped
 *  matching, not the interface shrunk. Two methods and three structures
 *  today. */
export const EXIT_FLOOR = { methods: 2, types: 3 };

/* ------------------------------------------------------------- the defaults */

/** The statements of an ABAP method body, comment lines dropped, each
 *  statement joined onto one line. A statement ends at a line whose last
 *  character outside a literal is a period; a template or a backtick literal
 *  broken over lines (the CSP tag is seven of them) does not end one. */
function statements(body) {
  const out = [];
  let current = [];
  for (const raw of body.split(/\r?\n/)) {
    const line = raw.trim();
    if (!line || line.startsWith('"') || line.startsWith('*')) continue;
    current.push(line);
    if (endsStatement(line)) {
      out.push(current.join(' '));
      current = [];
    }
  }
  if (current.length) out.push(current.join(' '));
  return out;
}

/** Whether `line` closes a statement: its last character outside every
 *  literal and before any trailing comment is a period. */
function endsStatement(line) {
  let quote = null;
  let last = null;
  for (const c of line) {
    if (quote) {
      if (c === quote) quote = null;
      continue;
    }
    if (c === '`' || c === "'" || c === '|') quote = c;
    else if (c === '"') break;
    else if (!/\s/.test(c)) last = c;
  }
  return !quote && last === '.';
}

/** `( n = `X` v = `Y` )` rows of a VALUE #( ) table, in order. */
const tableRows = (expr) =>
  [...expr.matchAll(/\(\s*n\s*=\s*`([^`]*)`\s+v\s*=\s*`([^`]*)`\s*\)/g)].map((m) => ({ n: m[1], v: m[2] }));

/**
 * The defaults the shipped exit assigns, per interface method:
 *
 *   Map { 'set_config_http_get' => Map { 'theme' => { kind: 'literal', value: 'sap_horizon' }, … },
 *         'set_config_http_post' => Map { … } }
 *
 * A value is one of { kind: 'literal', value }, { kind: 'bool', value },
 * { kind: 'table', rows: [{ n, v }] } or { kind: 'expression', text } - the
 * last for a right-hand side that is neither a literal nor a constant nor a
 * table, such as the CSP tag assembled from a template. The FIRST top-level
 * assignment of a field is its default; the post method assigns the draft
 * expiry twice, the second time inside the clamp that keeps it positive.
 */
export function parseExitDefaults(classSource) {
  const out = new Map();
  const constants = new Map();
  for (const m of classSource.matchAll(/\bCONSTANTS\s+([a-z0-9_]+)\s+TYPE\s+\S+\s+VALUE\s+(`[^`]*`|'[^']*'|\S+?)\s*\./gi)) {
    const v = m[2];
    constants.set(m[1].toLowerCase(), /^[`']/.test(v) ? v.slice(1, -1) : v);
  }
  const literal = (text) => {
    const t = text.trim();
    const quoted = /^`([^`]*)`$|^'([^']*)'$/.exec(t);
    if (quoted) return { kind: 'literal', value: quoted[1] ?? quoted[2] };
    if (/^abap_(true|false)$/i.test(t)) return { kind: 'bool', value: t.toLowerCase() };
    if (/^-?\d+$/.test(t)) return { kind: 'literal', value: t };
    if (constants.has(t.toLowerCase())) return { kind: 'literal', value: constants.get(t.toLowerCase()) };
    if (/^VALUE\s+#\s*\(/i.test(t)) return { kind: 'table', rows: tableRows(t) };
    return { kind: 'expression', text: t.replace(/\.\s*$/, '') };
  };
  const method = /^\s*METHOD\s+z2ui5_if_ui5_exit~([a-z0-9_]+)\s*\.\s*$([\s\S]*?)^\s*ENDMETHOD\s*\./gim;
  for (const m of classSource.matchAll(method)) {
    const name = m[1].toLowerCase();
    const fields = new Map();
    let depth = 0;
    for (const stmt of statements(m[2])) {
      if (/^(IF|CASE|DO|WHILE|LOOP|TRY)\b/i.test(stmt)) depth++;
      else if (/^(ENDIF|ENDCASE|ENDDO|ENDWHILE|ENDLOOP|ENDTRY)\b/i.test(stmt)) depth--;
      const assign = /^cs_config-([a-z0-9_]+)\s*=\s*([\s\S]+?)\s*\.\s*$/i.exec(stmt);
      if (!assign || depth > 0) continue;
      const field = assign[1].toLowerCase();
      if (!fields.has(field)) fields.set(field, literal(assign[2]));
    }
    out.set(name, fields);
  }
  return out;
}

/* ------------------------------------------------------------- the block */

const CONTEXT = 'ty_s_http_context';

/** What an unassigned field holds, by its type. */
function initialOf(type) {
  const t = type.toLowerCase();
  if (t === 'abap_bool') return '`abap_false`';
  if (/^(i|int[48]|p|f|decfloat\d*|n)$/.test(t)) return '`0`';
  return '*(empty)*';
}

/** A default as a table cell. */
function defaultCell(d, type, classLink) {
  if (!d) return initialOf(type);
  if (d.kind === 'bool') return `\`${d.value}\``;
  if (d.kind === 'literal') return d.value === '' ? '*(empty)*' : codeSpan(d.value);
  if (d.kind === 'table') {
    if (!d.rows.length) return '*(empty)*';
    return d.rows.map((r) => codeSpan(`${r.n}: ${r.v}`)).join(', ');
  }
  return `built by [the shipped exit](${classLink}) (\`${d.text}\`)`;
}

/** A comment run as one paragraph: the lines indented past the comment's
 *  own column are ABAP an installation writes, and become one code span. */
export function noteParagraph(note) {
  const parts = [];
  let code = [];
  const flush = () => {
    if (code.length) parts.push(codeSpan(code.join(' ').replace(/\s+/g, ' ').trim()));
    code = [];
  };
  for (const line of note) {
    if (/^\s/.test(line)) { code.push(line.trim()); continue; }
    flush();
    parts.push(prose(line.trim()));
  }
  flush();
  /* a word hyphenated over the line break ("client-" / "suppliable") joins
   * back up; a dash the notes use as punctuation stands between spaces */
  return parts.join(' ').replace(/\s+/g, ' ').replace(/(\w)- (\w)/g, '$1-$2').trim();
}

/** Every field of a structure, nested groups flattened the way an exit
 *  writes them (cs_config-a-b). */
function fieldRows(members, prefix, rows) {
  for (const f of members) {
    if (f.members) fieldRows(f.members, `${prefix}${f.name}-`, rows);
    else rows.push({ ...f, name: `${prefix}${f.name}` });
  }
  return rows;
}

/**
 * The generated block of user_exits.md: the two methods with their
 * parameters, then every structure they take with its fields, types,
 * defaults and the note the interface writes on each.
 *
 * @param {object} model   parseInterface( ) of z2ui5_if_ui5_exit, with memberNotes
 * @param {Map} defaults   parseExitDefaults( ) of z2ui5_cl_ui5_user_exit
 * @param {{ref, interfacePath, classPath, start, end}} where
 */
export function renderExitBlock(model, defaults, { ref, interfacePath, classPath, start, end }) {
  const repo = 'https://github.com/abap2UI5/abap2UI5/blob';
  const ifaceLink = `${repo}/${ref}/${interfacePath}`;
  const classLink = `${repo}/${ref}/${classPath}`;
  const out = [
    start,
    '',
    `Generated from [\`${model.name}\`](${ifaceLink}) on **${ref}** — ${model.methods.length} methods,`,
    `${model.types.length} structures — with the defaults [\`z2ui5_cl_ui5_user_exit\`](${classLink})`,
    'seeds before an exit of your own runs. A field your exit leaves alone keeps the default.',
    '',
  ];

  /* the structure each method changes, so a field's default is read from
   * the method that seeds it */
  const seededBy = new Map();
  for (const m of model.methods) {
    for (const p of m.params) if (p.kind === 'changing' || /^cs_/.test(p.name)) seededBy.set(p.type.toLowerCase(), m.name);
  }

  for (const m of model.methods) {
    out.push(`### \`${m.name}\``, '');
    for (const para of m.doc) out.push(prose(para), '');
    out.push('| Parameter | Type | |', '|---|---|---|');
    for (const p of m.params) {
      const how = p.name.startsWith('cs_') ? 'changing' : 'importing';
      out.push(`| \`${p.name}\` | \`${cell(p.type)}\` | ${how}${p.optional ? ', optional' : ''} |`);
    }
    out.push('');
  }

  for (const t of model.types) {
    out.push(`### \`${t.name}\``, '');
    for (const para of t.doc ?? []) out.push(prose(para), '');
    const rows = fieldRows(t.members ?? [], '', []);
    const seeds = defaults.get(seededBy.get(t.name)) ?? new Map();
    const context = t.name === CONTEXT;
    out.push(context ? '| Field | Type |' : '| Field | Type | Default |');
    out.push(context ? '|---|---|' : '|---|---|---|');
    for (const f of rows) {
      const type = `\`${cell(f.type)}\``;
      out.push(context ? `| \`${f.name}\` | ${type} |` : `| \`${f.name}\` | ${type} | ${defaultCell(seeds.get(f.name), f.type, classLink)} |`);
    }
    out.push('');
    for (const f of rows) {
      if (!f.note) continue;
      out.push(`**\`${f.name}\`** — ${noteParagraph(f.note)}`, '');
    }
  }
  out.push(end);
  return out.join('\n');
}
