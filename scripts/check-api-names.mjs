#!/usr/bin/env node
/*
 * check-api-names — every `client->` name on this site has to exist in the
 * release this site names.
 *
 * check-examples already compiles the fenced examples that are WHOLE CLASSES,
 * and that is the strongest check here. It cannot see the rest of a page: a
 * sentence in the prose, a two-line snippet that is not a class, the constant
 * block a page reproduces for reference. That is most of what a reader
 * actually copies, and it is where the API drift of 1.143.0 survived:
 *
 *   routing.md      opened with `client->set_nav_routing( )`, a METHOD deleted
 *                   in that release, and navigated with
 *                   `cs_event-nav_to_route`, a constant deleted with it
 *   navigation.md   named the same method in a sentence
 *   frontend.md     listed `nav_to_route` and `history_back` in the cs_event
 *                   block it prints for reference
 *   url_handling.md announced "two client methods" for the browser history and
 *                   showed one - the second was the deleted `history_back`
 *
 * Four pages, one release, nothing red. So this gate reads the SAME interface
 * check-examples compiles against and asks three questions of every page:
 *
 *   `client->NAME(`        is NAME a method of z2ui5_if_client?
 *   `NAME = ` inside one   is it a parameter of that method?  (fenced ABAP only)
 *   `cs_GROUP-MEMBER`      is MEMBER in that constant group?
 *
 * …and a fourth, of the same kind: a `github.com/abap2UI5/abap2UI5/blob/main/`
 * link has to resolve. The user-exits page pointed at
 * `src/02/z2ui5_if_exit.intf.abap` after the interface had been retired to
 * `src/99` - a 404 for every reader who clicked it. Those links say `main`, so
 * `main` is what they are checked against, not the release.
 *
 * TRUTH is the release, never main. A page is correct when it matches what the
 * reader can install, and main is ahead of that by definition - judging
 * against main would pass a page teaching API that does not exist yet. The
 * release number comes from lib/release.mjs, the same one check-examples
 * pins to, and `A2UI5_REF` overrides it for a canary run against main.
 *
 * When the interface cannot be fetched the run SAYS SO and passes. A
 * documentation gate must not go red because github.com is unreachable, and
 * must not claim to have verified something it did not.
 *
 * Pages whose SUBJECT is what was removed are exempt - deprecations and the
 * changelog exist to name the old names, and a gate that forbids that forbids
 * documenting a migration at all.
 *
 *   node scripts/check-api-names.mjs      (npm run check:api-names)
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { declaredRelease } from './lib/release.mjs';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const PAGES = path.join(ROOT, 'docs');

/* The pages that are ABOUT the names that went away. */
const EXEMPT = new Set([
  'resources/deprecations.md',
  'resources/changelog.md',
  'cookbook/event_navigation/action.md',
]);

const REF = process.env.A2UI5_REF || declaredRelease(ROOT);
if (!REF) {
  console.log('the three places naming the release DISAGREE, so there is nothing to pin to.');
  console.log('SKIPPED: run npm run check:version.');
  process.exit(0);
}

const SOURCE = `https://raw.githubusercontent.com/abap2UI5/abap2UI5/${REF}/src/02/z2ui5_if_client.intf.abap`;

let iface;
try {
  const res = await fetch(SOURCE, { signal: AbortSignal.timeout(20000) });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  iface = await res.text();
} catch (err) {
  console.log(`z2ui5_if_client at ${REF}: not resolved (${err.message})`);
  console.log('SKIPPED: nothing was verified.');
  process.exit(0);
}

/* ------------------------------------------------------------- the contract */

/** method name -> Set of IMPORTING parameter names. */
const methods = new Map();
/** constant group (cs_event, cs_view, ...) -> Set of member names. */
const groups = new Map();

{
  let method = null;
  let importing = false;
  const open = [];
  for (const raw of iface.split(/\r?\n/)) {
    // a comment can hold anything, including the names this gate looks for
    const line = raw.replace(/^\s*"[!]?.*$/, '').replace(/\s"\s.*$/, '').trimEnd();

    const begin = /^\s*BEGIN OF ([a-z_0-9]+),?\s*$/i.exec(line);
    if (begin) { open.push(begin[1].toLowerCase()); groups.set(open.at(-1), new Set()); continue; }
    if (/^\s*END OF ([a-z_0-9]+)/i.test(line)) { open.pop(); continue; }
    if (open.length) {
      // a member of every group it is nested in, so cs_device-system-phone
      // resolves whether the page writes the outer or the inner name
      const member = /^\s*([a-z_0-9]+)\s+TYPE\s/i.exec(line);
      if (member) for (const g of open) groups.get(g).add(member[1].toLowerCase());
      continue;
    }

    const decl = /^\s*METHODS\s+([a-z_0-9]+)/i.exec(line);
    if (decl) { method = decl[1].toLowerCase(); methods.set(method, new Set()); importing = false; continue; }
    if (!method) continue;
    if (/^\s*IMPORTING\s*$/i.test(line)) { importing = true; continue; }
    if (/^\s*(RETURNING|EXPORTING|CHANGING|RAISING|PREFERRED)/i.test(line)) { importing = false; continue; }
    if (/^\s*(CONSTANTS|TYPES|DATA|ENDINTERFACE)/i.test(line)) { method = null; continue; }
    if (!importing) continue;
    const param = /^\s*(?:VALUE\()?([a-z_0-9]+)\)?\s+TYPE\s/i.exec(line);
    if (param) methods.get(method).add(param[1].toLowerCase());
  }
}

if (methods.size === 0 || !groups.has('cs_event')) {
  console.log(`z2ui5_if_client at ${REF} parsed to nothing - the interface changed shape.`);
  console.log('Fix the parser in scripts/check-api-names.mjs, or this gate silently stops checking.');
  process.exit(1);
}

/* ------------------------------------------------------------------ the pages */

function markdownFiles(dir, out = []) {
  for (const name of fs.readdirSync(dir)) {
    if (name === '.vitepress' || name === 'public' || name === 'node_modules') continue;
    const full = path.join(dir, name);
    if (fs.statSync(full).isDirectory()) markdownFiles(full, out);
    else if (full.endsWith('.md')) out.push(full);
  }
  return out;
}

/** The fenced ABAP blocks of a page, as one string with everything else blanked. */
const abapOnly = (text) => text.replace(/```(\w*)\n([\s\S]*?)```/g, (all, lang, body) => (
  /^(abap)?$/i.test(lang) ? all.replace(body, body) : ' '.repeat(all.length)
));

/** Blank ABAP string literals so an XML attribute inside one is not a parameter. */
const withoutLiterals = (text) => text
  .replace(/`[^`\n]*`/g, (x) => ' '.repeat(x.length))
  .replace(/\|[^|\n]*\|/g, (x) => ' '.repeat(x.length));

const problems = [];
let checked = 0;

for (const file of markdownFiles(PAGES)) {
  const rel = path.relative(PAGES, file).split(path.sep).join('/');
  if (EXEMPT.has(rel)) continue;
  const text = fs.readFileSync(file, 'utf8');

  // 1 + 2: `client->NAME(` anywhere; its arguments only where they are code
  for (const source of [{ text, params: false }, { text: withoutLiterals(abapOnly(text)), params: true }]) {
    for (const call of source.text.matchAll(/client->([a-z_0-9]+)\s*\(/gi)) {
      const name = call[1].toLowerCase();
      checked += 1;
      if (!methods.has(name)) {
        if (!source.params) {
          problems.push(`${rel}: \`client->${name}( )\` is not a method of z2ui5_if_client in ${REF}`);
        }
        continue;
      }
      if (!source.params) continue;

      // walk to the matching close paren, then read the top-level `name =`
      let i = call.index + call[0].length;
      let depth = 1;
      while (i < source.text.length && depth > 0) {
        if (source.text[i] === '(') depth += 1;
        else if (source.text[i] === ')') depth -= 1;
        i += 1;
      }
      const args = source.text.slice(call.index + call[0].length, i - 1);
      let nested = 0;
      for (const token of args.matchAll(/([()])|\b([a-z_0-9]+)\s*=(?![=>])/gi)) {
        if (token[1] === '(') { nested += 1; continue; }
        if (token[1] === ')') { nested -= 1; continue; }
        if (nested !== 0) continue;
        const param = token[2].toLowerCase();
        if (!methods.get(name).has(param)) {
          problems.push(
            `${rel}: \`client->${name}( ${param} = ... )\` - no such parameter in ${REF}\n`
            + `    it takes: ${[...methods.get(name)].join(', ') || '(none)'}`,
          );
        }
      }
    }
  }

  // 3: cs_<group>-<member>, wherever it is written
  for (const use of text.matchAll(/\b(cs_[a-z_0-9]+)-([a-z_0-9]+)/gi)) {
    const group = use[1].toLowerCase();
    const member = use[2].toLowerCase();
    if (!groups.has(group)) continue;
    checked += 1;
    if (!groups.get(group).has(member)) {
      problems.push(`${rel}: \`${use[0]}\` is not in ${group} in ${REF}`);
    }
  }
}

/* 4: the source links. Distinct URLs only - the same file is linked from
 * several pages - and one HEAD each, which is a handful of requests. A network
 * failure here SKIPS this question and leaves the three above intact. */
let links = 0;
{
  const seen = new Map();
  for (const file of markdownFiles(PAGES)) {
    const rel = path.relative(PAGES, file).split(path.sep).join('/');
    for (const m of fs.readFileSync(file, 'utf8')
      .matchAll(/https:\/\/github\.com\/abap2UI5\/abap2UI5\/blob\/main\/([^)`"\s]+)/g)) {
      const target = m[1].split('#')[0];
      if (!seen.has(target)) seen.set(target, new Set());
      seen.get(target).add(rel);
    }
  }
  links = seen.size;
  const raw = (f) => `https://raw.githubusercontent.com/abap2UI5/abap2UI5/main/${f}`;
  const verdicts = await Promise.all([...seen.keys()].map(async (target) => {
    try {
      const res = await fetch(raw(target), { method: 'HEAD', signal: AbortSignal.timeout(20000) });
      if (res.status === 404) return target;
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return null;
    } catch {
      return undefined;      // unreachable, not absent
    }
  }));
  if (verdicts.some((v) => v === undefined)) {
    console.log('source links: not resolved (network) - that question was skipped.');
    links = 0;
  } else {
    for (const target of verdicts.filter(Boolean)) {
      problems.push(
        `${[...seen.get(target)].join(', ')}: links ${target} in abap2UI5, which is not there on main\n`
        + '    the file moved or was deleted - a link every reader who clicks it gets a 404 from',
      );
    }
  }
}

console.log(`check-api-names: ${checked} name(s) and ${links} source link(s) on ${markdownFiles(PAGES).length} page(s), against abap2UI5 ${REF}`);

if (problems.length) {
  console.log(`\n${problems.length} problem(s):`);
  for (const p of problems) console.log(`  ${p}`);
  console.log('\n  Something the reader cannot follow: a name their install does not');
  console.log('  have, or a link that 404s. Either the page describes a release that');
  console.log('  has not happened yet, or it describes one that is gone - see');
  console.log('  resources/deprecations.md for what replaced it.');
  process.exit(1);
}
console.log('every client-> name on the site exists in the release it names - OK');
