/*
 * A Run button on the examples that can actually run.
 *
 * The playground (abap2UI5/playground) is the whole framework compiled into a
 * page: ABAP in, a running UI5 app out, no server and no SAP system anywhere.
 * It can be embedded, and the code it runs can travel in the URL fragment —
 * which means an example printed here can be started from here, without this
 * repository hosting anything and without the example being written down a
 * second time somewhere it could drift from the page.
 *
 * What this module decides is the only hard part: WHICH fenced block gets a
 * button. A button on a block the playground cannot start is worse than no
 * button — the reader clicks, waits several seconds for an ABAP runtime to
 * boot, and is shown an error about their own documentation. So the rule is
 * narrow, and it is a rule about what the BROWSER has rather than about what is
 * good ABAP:
 *
 *   1. a complete global class — DEFINITION and IMPLEMENTATION, because the
 *      playground compiles a whole abapGit object and nothing less,
 *   2. implementing `z2ui5_if_app`, because that is what the framework starts,
 *   3. that displays something, because a demo of a blank frame demonstrates
 *      nothing,
 *   4. and that needs nothing which is not in the page: no database table of
 *      its own, no CDS entity, no behavior definition, no add-on repository,
 *      no on-premise SAP class the transpiler does not implement.
 *
 * **Every rule below was put there by watching an example fail in a real
 * playground**, not by reasoning about what might. All 61 complete app classes
 * in this documentation were opened in one: 38 started, and the 23 that did not
 * are the list of shapes here — except for three, which were defects in the
 * documentation and were fixed instead. The 39 this module now offers a button
 * for were then run again, and all 39 started and rendered.
 *
 * AGENTS.md says how to repeat that measurement, and that is also the honest
 * limit of this file: the playground is the only thing that can decide the
 * question and it is not in this repository, so these rules are a conservative
 * approximation of it, maintained by hand. They fail towards NOT offering a
 * button.
 *
 * An example that trips one of them is still printed, still checked by
 * `check:examples` and still copied by readers. It simply has no button.
 *
 * The code the button runs is read from the rendered block at click time
 * (`theme/playground.js`), not copied into an attribute here: the reader runs
 * exactly the text they can see and copy, and a page cannot carry two versions
 * of one example.
 */

/** A complete global class: both halves, not a fragment of a method. */
const CLASS_DEFINITION = /CLASS\s+(\S+)\s+DEFINITION/i;
const CLASS_IMPLEMENTATION = /CLASS\s+\S+\s+IMPLEMENTATION/i;

/** What the framework starts. An app is a class that implements this. */
const IS_AN_APP = /INTERFACES\s+z2ui5_if_app\b/i;

/* The declared object name, read the way the playground reads it
 * (`src/editor/registry.mjs`), because the file the code is put into is named
 * after it: abaplint derives an object's name from its file name and refuses
 * the pair when they disagree. */
const DECLARED_NAME = /^\s*(?:CLASS|INTERFACE)\s+([a-zA-Z_]\w*)\s+(?:DEFINITION|PUBLIC)/im;

/* An ABAP object name is at most 30 characters. Longer than that is not a
 * playground limit at all - it is a class nobody can create in any system, and
 * two of them were printed here for years. `check:examples` could not see them
 * because it renames every example to `zcl_docs_example_NN` before compiling
 * it, which is exactly 20. */
const MAX_NAME = 30;

/** Something has to arrive on screen. */
const DISPLAYS_SOMETHING = /->\s*(?:view|popup|message_box|message_toast|nest_view)[a-z_]*_display\s*\(/i;

/* The database in the page holds the framework's own tables and what open-abap
 * ships. A business table is not among them, and `SELECT` from one is the
 * single most common reason an example here cannot run - ten of the twenty-three
 * failures. T100 is on the list because open-abap has it and because a page
 * uses it. */
const TABLES_THE_PAGE_HAS = new Set(['t100']);

/*
 * What the browser has not got, in the words the failure arrives in. Order
 * matters only for which reason is reported first.
 */
const NEEDS_MORE_THAN_A_BROWSER = [
  {
    // draft_handling, eml, fuzzy_search
    why: 'needs a CDS entity, a behavior definition or a HANA database',
    what: /\bREAD\s+ENTITIES\b|\bMODIFY\s+ENTITIES\b|\bCOMMIT\s+ENTITIES\b|TABLE\s+FOR\s+(?:READ|CREATE|UPDATE)\b|\bFUZZY\b/i,
  },
  {
    // launchpad — z2ui5_if_lp_kpi lives in abap2UI5-addons, not in the framework
    why: 'needs an add-on repository that is not part of the framework',
    what: /\bz2ui5_(?:if|cl)_lp_/i,
  },
  {
    // email (cl_bcs_message), snippets (cl_demo_output), srtti (a DDIC lookup)
    why: 'uses an on-premise SAP class the transpiler does not implement',
    what: /\bcl_bcs\w*\b|\bcl_demo_output\b|\bcl_gui_\w+|\bcl_salv_\w+|\bdescribe_by_name\s*\(/i,
  },
  {
    why: 'calls a function module, and there is no application server to call one on',
    what: /\bCALL\s+FUNCTION\b/i,
  },
];

/*
 * ABAP with its comments taken out and its text blanked, so a rule can only be
 * tripped by code. Both halves were put here by a false positive:
 *
 *   - `get_started/full_example.md` shows the SELECT a reader would write, as a
 *     comment, above the demo data it uses instead — and it runs perfectly
 *     well.
 *   - `clipboard.md` says `mv_text = \`Hello from abap2UI5\``, and a rule
 *     looking for `FROM <table>` read that as a table called abap2ui5.
 *
 * A comment is `*` in the first column, or `"` outside a literal. Finding that
 * "outside" is what needs the three literal forms tracked — `'...'`,
 * `` `...` `` and `|...|` — and a string template is full of the double quotes
 * a simpler version would stop at. The delimiters are kept and only what is
 * between them is blanked, so every offset in the line still means what it did.
 */
export function abapOnly(code) {
  return code
    .split('\n')
    .map((line) => {
      if (line.startsWith('*')) return '';
      let out = '';
      let quote;
      for (const c of line) {
        if (quote) {
          out += c === quote ? c : ' ';
          if (c === quote) quote = undefined;
        } else if (c === "'" || c === '`' || c === '|') {
          quote = c;
          out += c;
        } else if (c === '"') {
          break;
        } else {
          out += c;
        }
      }
      return out;
    })
    .join('\n');
}

/* Names that follow INSERT / UPDATE / MODIFY / FROM and are not tables:
 * `INSERT VALUE #( … ) INTO TABLE lt_rows` is an internal-table statement, and
 * it read as a table called VALUE. */
const NOT_A_TABLE = new Set([
  'value', 'table', 'corresponding', 'new', 'ref', 'conv', 'cond', 'switch',
  'reduce', 'filter', 'exact', 'lines', 'initial',
]);

/** Every name the code introduces itself — a variable, a type, a parameter. */
function declaredNames(code) {
  const names = new Set();
  const add = (re, group = 1) => {
    for (const m of code.matchAll(re)) names.add(m[group].toLowerCase());
  };
  add(/\bDATA[:(]?\s*\(?\s*([a-z_]\w*)/gi);
  add(/\bTYPES[:]?\s*(?:BEGIN\s+OF\s+)?([a-z_]\w*)/gi);
  add(/\bFIELD-SYMBOLS[:]?\s*<([a-z_]\w*)>/gi);
  add(/\b(?:IMPORTING|EXPORTING|CHANGING|RETURNING\s+VALUE\(|VALUE\()\s*([a-z_]\w*)/gi);
  add(/^\s*([a-z_]\w*)\s+TYPE\s/gim);
  return names;
}

/** The database tables this example reads or writes, as far as they can be told
 *  apart from internal tables and from the class hierarchy. */
function tablesUsed(code) {
  const declared = declaredNames(code);
  const out = new Set();
  /* `FROM x`, except the `INHERITING FROM` of a class definition and except a
   * work area handed to an internal-table statement — which is a name the code
   * declared itself. */
  const consider = (raw) => {
    const name = raw.toLowerCase();
    if (declared.has(name) || NOT_A_TABLE.has(name) || TABLES_THE_PAGE_HAS.has(name)) return;
    out.add(name);
  };
  for (const m of code.matchAll(/(\bINHERITING\s+)?\bFROM\s+@?([a-z_]\w*)/gi)) {
    if (!m[1]) consider(m[2]);
  }
  for (const m of code.matchAll(/^\s*(?:INSERT|UPDATE|MODIFY)\s+([a-z_]\w*)\s/gim)) consider(m[1]);
  return [...out];
}

/** A method the class promises and never writes. abaplint stops the run on it,
 *  and so would an activation in a real system: the example does not compile
 *  as printed. */
function unimplementedMethods(code) {
  const definition = code.split(CLASS_IMPLEMENTATION)[0];
  const declared = [];
  for (const m of definition.matchAll(/^\s*METHODS:?\s+([^.]+)\./gim)) {
    for (const one of m[1].split(',')) {
      const name = /^\s*([a-z_]\w*)/i.exec(one)?.[1];
      /* A redefinition or an interface method is implemented under another
       * name, or by the superclass. */
      if (name && !/\bREDEFINITION\b/i.test(one) && !one.includes('~')) declared.push(name);
    }
  }
  return declared.filter((name) => !new RegExp(`^\\s*METHOD\\s+${name}\\s*\\.`, 'im').test(code));
}

/*
 * A local class or interface, whether the example defines one or only calls
 * one. Both are out, and for the same reason: a playground file is one abapGit
 * object, and a class-local class is not an object - abapGit serializes it into
 * a `.clas.locals_imp.abap` beside the class, which there is nowhere to put
 * here. So an example calling `lcl_help=>…` has nothing to resolve, and an
 * example defining `lcl_help` would have the file named after the wrong class.
 */
function localObjects(code) {
  const names = new Set();
  for (const m of code.matchAll(/\b(l(?:cl|if)_\w+)\b/gi)) names.add(m[1].toLowerCase());
  return [...names];
}

/**
 * The class this fence would run in the playground, or the reason it would not.
 *
 * @param {string} code the ABAP inside a ```abap fence
 * @returns {{ name: string } | { why: string }}
 */
export function playgroundExample(code) {
  if (!CLASS_DEFINITION.test(code) || !CLASS_IMPLEMENTATION.test(code)) {
    return { why: 'not a complete class — the playground compiles whole objects' };
  }
  if (!IS_AN_APP.test(code)) {
    return { why: 'does not implement z2ui5_if_app, so there is no app to start' };
  }
  const name = DECLARED_NAME.exec(code)?.[1];
  if (!name) return { why: 'declares no global class' };
  if (name.length > MAX_NAME) {
    return { why: `${name} is ${name.length} characters — an ABAP object name is at most ${MAX_NAME}` };
  }

  const abap = abapOnly(code);
  /* The reasons come before the "shows nothing" one on purpose: an example
   * that needs an add-on also displays through it, and naming the add-on is
   * the useful half of that sentence. */
  for (const { why, what } of NEEDS_MORE_THAN_A_BROWSER) {
    if (what.test(abap)) return { why };
  }
  const tables = tablesUsed(abap);
  if (tables.length) {
    return { why: `reads ${tables.join(', ')} — the database in the page holds the framework's tables and nothing else` };
  }
  const missing = unimplementedMethods(abap);
  if (missing.length) {
    return { why: `declares ${missing.join(', ')} and never implements it — this does not compile anywhere` };
  }
  const locals = localObjects(abap);
  if (locals.length) {
    return { why: `uses ${locals.join(', ')} — a class-local object has no file of its own here` };
  }
  if (!DISPLAYS_SOMETHING.test(abap)) {
    return { why: 'displays nothing, so there would be nothing to see' };
  }
  return { name: name.toLowerCase() };
}

/** True when this fence should carry a Run button. */
export const isRunnable = (code) => 'name' in playgroundExample(code);

/*
 * The markdown-it side. VitePress renders a fence into
 * `<div class="language-abap">…</div>`; this wraps that in a container and
 * hangs a button off it. The button does nothing until the theme's client
 * script picks it up, so a page with JavaScript switched off is exactly the
 * page it was before — printed code, and the copy button VitePress adds.
 */
export function playgroundButton(md) {
  const fence = md.renderer.rules.fence;
  md.renderer.rules.fence = (tokens, idx, options, env, self) => {
    const html = fence(tokens, idx, options, env, self);
    const token = tokens[idx];
    if (token.info.trim().split(/\s+/)[0] !== 'abap') return html;
    if (!isRunnable(token.content)) return html;
    return (
      '<div class="a2ui5-play">' +
      html +
      '<button class="a2ui5-play-run" type="button">Run this example</button>' +
      '</div>'
    );
  };
}
