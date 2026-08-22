/*
 * Whether every example without a Run button is without one ON PURPOSE.
 *
 * `docs/.vitepress/playground.mjs` decides which fenced ABAP example gets a
 * Run button, and it fails towards NO button. That is the right direction for
 * the reader — but it leaves a silent gap: an example that could run and
 * simply never got measured looks exactly like an example that can never run.
 * The two used to be told apart by hand, in a ledger in AGENTS.md, and the
 * ledger went stale the first time somebody added examples without re-reading
 * it.
 *
 * This module makes the gap decidable. Every complete `z2ui5_if_app` class
 * printed on a page either
 *
 *   - gets a button from the rule engine, or
 *   - carries a marker, written directly above its fence, saying why it
 *     cannot run:
 *
 *       <!-- playground: no Run button — SELECTs from VBAK, which no browser database has -->
 *
 * An app example with neither is an undecided one: either it runs — then
 * measure it in a real playground (AGENTS.md says how) and let the engine give
 * it its button — or it does not, and the marker records why. A marker above
 * an example that HAS a button is stale and refused just as loudly, so a
 * marker can never quietly outlive a fix that made its example runnable.
 *
 * What this cannot decide — and does not claim to — is whether a buttoned
 * example actually starts. Only a playground answers that, and re-measuring
 * against one is still the rule when examples are added. This gate holds the
 * *bookkeeping* honest between measurements: no example is ever buttonless by
 * accident.
 */
import { playgroundExample } from '../../docs/.vitepress/playground.mjs';

/** The marker, exactly: intent first, then the reason a page editor needs. */
export const MARKER = /^<!--\s*playground:\s*no Run button — (.+?)\s*-->$/;

const IS_AN_APP = /INTERFACES\s+z2ui5_if_app\b/i;
const COMPLETE =
  (code) => /CLASS\s+\S+\s+DEFINITION/i.test(code) && /CLASS\s+\S+\s+IMPLEMENTATION/i.test(code);

/**
 * One page's fences, judged.
 *
 * @param {string} md the page's markdown
 * @returns {{
 *   classes: number, apps: number, buttons: number,
 *   excluded: {name: string, why: string, reason: string}[],
 *   undecided: {name: string, why: string}[],
 *   stale: string[],
 * }}
 */
export function auditPage(md) {
  const out = { classes: 0, apps: 0, buttons: 0, excluded: [], undecided: [], stale: [] };
  const lines = md.split('\n');
  const markerAt = new Map(); // line number -> the reason it carries
  lines.forEach((line, i) => {
    const m = MARKER.exec(line.trim());
    if (m) markerAt.set(i, m[1]);
  });
  const used = new Set();

  /* Fence positions, walked line by line so a marker attaches by line number
   * rather than by text — two identical markers stay two markers. */
  const startLine = [];
  lines.forEach((line, i) => {
    if (line.startsWith('```abap')) startLine.push(i);
  });

  for (const start of startLine) {
    /* the nearest non-blank line above the fence */
    let above = start - 1;
    while (above >= 0 && lines[above].trim() === '') above--;
    const reason = markerAt.get(above);
    if (reason !== undefined) used.add(above);

    /* the fence body */
    const end = lines.indexOf('```', start + 1);
    if (end === -1) continue;
    const code = lines.slice(start + 1, end).join('\n') + '\n';

    if (!COMPLETE(code)) {
      if (reason !== undefined) {
        out.stale.push('a marker above a fence that is not a complete class — the engine never offers those a button');
      }
      continue;
    }
    out.classes++;
    const name = /CLASS\s+(\S+)\s+DEFINITION/i.exec(code)?.[1]?.toLowerCase() ?? '?';
    if (!IS_AN_APP.test(code)) {
      if (reason !== undefined) {
        out.stale.push(`${name} is not an app — the engine never offers it a button, so its marker only adds noise`);
      }
      continue;
    }
    out.apps++;
    const decision = playgroundExample(code);
    if ('name' in decision) {
      if (reason !== undefined) {
        out.stale.push(`${name} HAS a Run button, and a marker saying it must not — one of the two is wrong`);
      } else {
        out.buttons++;
      }
    } else if (reason !== undefined) {
      out.excluded.push({ name, why: decision.why, reason });
    } else {
      out.undecided.push({ name, why: decision.why });
    }
  }

  /* A marker no fence attached to: it sits above prose, above a non-ABAP
   * fence, or was orphaned by an edit. It records an intent about nothing. */
  for (const [i, reason] of markerAt) {
    if (!used.has(i)) out.stale.push(`a marker attached to no ABAP fence: "${reason}" (line ${i + 1})`);
  }
  return out;
}
