/*
 * lifecycle — does an example display its view where the manual says to?
 *
 * Every page of the manual teaches the same shape of main( ): build and
 * display the view under `client->check_on_init( )` or
 * `client->check_on_navigated( )`, handle events in the branches after it,
 * and never call `client->view_display( )` on every roundtrip - a view sent
 * on every POST rebuilds the whole screen on every click, loses the scroll
 * position and the focus, and is the single most copied mistake in apps
 * written from the examples (cookbook/event_navigation/life_cycle.md,
 * "Lifecycle Pitfalls"). The pages taught it in prose and a third of the
 * examples did the opposite in code, because nothing read the code for it.
 *
 * This reads it. In every complete app class the playground would run (the
 * same decision as the Run button, so an example a reader can START is held
 * to the shape a reader will copy), the statements of `z2ui5_if_app~main`
 * are walked with a stack of the open IF / CASE blocks, and a
 * `client->view_display( )` statement is a finding unless some block on
 * the stack was opened or re-entered by a lifecycle condition:
 *
 *   IF client->check_on_init( ).          ELSEIF client->check_on_navigated( ).
 *   CASE abap_true. WHEN client->check_on_init( ).
 *
 * A nested IF inside such a branch inherits it. ELSE, a WHEN on an event, and
 * an ENDIF / ENDCASE leave it. A condition written the other way round -
 * `check_on_init( ) = abap_false`, `NOT client->check_on_init( )` - is not a
 * lifecycle branch: the display after it is the unconditional one.
 *
 * Only main( ) is read. The canonical shape calls a method of the class
 * (`view_display( )`) from the branch and the `client->view_display( )`
 * stands in THAT method, where it is right by construction; a scanner that
 * followed the call graph would be a compiler. So the one thing it catches
 * is the one thing that is wrong on its face: the display in main( ) with no
 * lifecycle branch around it. A page that wants exactly that - an essay on
 * the roundtrip, a deliberately minimal class - says so above the fence:
 *
 *   <!-- lifecycle: allow unconditional display — the page is about the roundtrip itself -->
 *
 * and a marker whose fence has nothing to allow any more is refused, the way
 * the playground marker is.
 *
 * Pinned by test/lifecycle.test.mjs.
 */
import { abapOnly, isRunnable } from '../../docs/.vitepress/playground.mjs';

/** The marker, exactly: intent first, then the reason a page editor needs. */
export const MARKER = /^<!--\s*lifecycle:\s*allow unconditional display — (.+?)\s*-->$/;

/** The one display call the rule is about. `nest_view_display`, the popups
 *  and the message box are not the main view and are not judged. */
const DISPLAY = /\bclient->view_display\s*\(/i;

/** A condition that opens a lifecycle branch - and not its negation. */
const LIFECYCLE = /\bcheck_on_(?:init|navigated)\s*\(/i;
const NEGATED = /\babap_false\b|\bNOT\s+client->check_on_/i;
const opensBranch = (stmt) => LIFECYCLE.test(stmt) && !NEGATED.test(stmt);

/** The statements of `main( )`, each with the 0-based line of its first
 *  line in `code`, comments and literals blanked first so a display in a
 *  comment or a word in a string cannot trip the rule. Null when the class
 *  has no main - then it is not an app, whatever it declares. */
export function mainStatements(code) {
  const lines = abapOnly(code).split('\n');
  const from = lines.findIndex((l) => /^\s*METHOD\s+z2ui5_if_app~main\s*\./i.test(l));
  if (from === -1) return null;
  const out = [];
  let current = null;
  for (let i = from + 1; i < lines.length; i++) {
    const text = lines[i];
    if (/^\s*ENDMETHOD\s*\./i.test(text)) break;
    if (!text.trim()) continue;
    if (!current) current = { line: i, text: '' };
    current.text += (current.text ? ' ' : '') + text.trim();
    if (/\.\s*$/.test(text)) { out.push(current); current = null; }
  }
  if (current) out.push(current);
  return out;
}

/**
 * Every `client->view_display( )` in main( ) that no lifecycle branch
 * encloses: `[{ line, text }]`, the line 0-based within `code`. An empty
 * list for a class whose displays all sit under check_on_init /
 * check_on_navigated - and for a class with no main( ) at all.
 */
export function unconditionalDisplays(code) {
  const statements = mainStatements(code);
  if (!statements) return [];
  const stack = [];
  const inBranch = () => stack.some((b) => b.allowed);
  const out = [];
  for (const s of statements) {
    const head = /^([A-Z]+)\b/i.exec(s.text)?.[1]?.toUpperCase();
    switch (head) {
      case 'IF': stack.push({ kind: 'if', allowed: opensBranch(s.text) }); continue;
      case 'ELSEIF': if (stack.length) stack.at(-1).allowed = opensBranch(s.text); continue;
      case 'ELSE': if (stack.length) stack.at(-1).allowed = false; continue;
      case 'ENDIF': stack.pop(); continue;
      case 'CASE': stack.push({ kind: 'case', allowed: false }); continue;
      case 'WHEN': if (stack.length) stack.at(-1).allowed = opensBranch(s.text); continue;
      case 'ENDCASE': stack.pop(); continue;
      default: break;
    }
    if (DISPLAY.test(s.text) && !inBranch()) out.push({ line: s.line, text: s.text });
  }
  return out;
}

/**
 * One page's fences, judged.
 *
 * @param {string} md the page's markdown
 * @returns {{
 *   apps: number,                                   complete app classes with a Run button, judged
 *   allowed: number,                                of those, carrying a marker
 *   findings: {name: string, line: number, text: string}[],   file line, 1-based
 *   stale: string[],
 * }}
 */
export function auditLifecycle(md) {
  const out = { apps: 0, allowed: 0, findings: [], stale: [] };
  const lines = md.split('\n');
  const markerAt = new Map();
  lines.forEach((line, i) => {
    const m = MARKER.exec(line.trim());
    if (m) markerAt.set(i, m[1]);
  });
  const used = new Set();

  for (let start = 0; start < lines.length; start++) {
    if (!lines[start].trim().startsWith('```abap')) continue;
    const end = lines.findIndex((l, k) => k > start && l.trim().startsWith('```'));
    if (end === -1) break;
    let above = start - 1;
    while (above >= 0 && lines[above].trim() === '') above--;
    const reason = markerAt.get(above);
    if (reason !== undefined) used.add(above);
    const code = lines.slice(start + 1, end).join('\n') + '\n';
    /* code line k (0-based) is file line open + 1 + k, 0-based - so
     * open + 2 + k as a reader counts */
    const open = start;
    start = end;

    if (!isRunnable(code)) {
      if (reason !== undefined) out.stale.push(`a marker above a fence that is not a runnable app class - the rule never reads those (line ${above + 1})`);
      continue;
    }
    out.apps++;
    const name = /CLASS\s+(\S+)\s+DEFINITION/i.exec(code)?.[1]?.toLowerCase() ?? '?';
    const found = unconditionalDisplays(code);
    if (reason !== undefined) {
      out.allowed++;
      if (!found.length) out.stale.push(`${name} displays its view under a lifecycle branch, and a marker says it may not - one of the two is wrong (line ${above + 1})`);
      continue;
    }
    for (const f of found) out.findings.push({ name, line: open + 2 + f.line, text: f.text });
  }
  for (const [i, reason] of markerAt) {
    if (!used.has(i)) out.stale.push(`a marker attached to no ABAP fence: "${reason}" (line ${i + 1})`);
  }
  return out;
}
