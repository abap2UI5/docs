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
 * TRUTH is main - the framework as it IS, the same branch the source links
 * above resolve against and the same one check-examples compiles to. It used
 * to be the release; lib/release.mjs (frameworkRef) carries why that changed
 * and what now answers "does the reader's install have this" instead.
 *
 * When the interface cannot be fetched the run SAYS SO and passes. A
 * documentation gate must not go red because github.com is unreachable, and
 * must not claim to have verified something it did not.
 *
 * Pages whose SUBJECT is what was removed are exempt - deprecations and the
 * changelog exist to name the old names, and a gate that forbids that forbids
 * documenting a migration at all.
 *
 * The same exemption carries a fifth question: no page outside it names an
 * object of the framework's FROZEN package (src/99 - the util classes, the
 * built-in popups, the predecessor view builder, the superseded exit and
 * types interfaces, the handler shim). Those still ship, so a page teaching
 * them would compile and pass every question above - four pages did, with
 * z2ui5_cl_util=>conv_encode_x_base64( ) where cl_web_http_utility is the
 * answer. The rule the deprecations page exists for: what is superseded is
 * named there, with its successor, and nowhere else.
 *
 *   node scripts/check-api-names.mjs      (npm run check:api-names)
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { frameworkRef } from './lib/release.mjs';
import { fetchInterface, interfaceSource } from './lib/client-interface.mjs';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const PAGES = path.join(ROOT, 'docs');

/* The pages that are ABOUT the names that went away. Each one has to exist:
 * an exemption for a page that was renamed or folded away exempts nothing, and
 * one for a path that comes back later exempts a page nobody meant to. */
const EXEMPT = new Set([
  'resources/deprecations.md',
  'resources/changelog.md',
]);
const missingExempt = [...EXEMPT].filter((rel) => !fs.existsSync(path.join(PAGES, rel)));
if (missingExempt.length) {
  console.log(`check-api-names: EXEMPT names ${missingExempt.join(', ')}, which is not a page under docs/.`);
  console.log('  Take it off the list, or point it at where that page went.');
  process.exit(1);
}

const REF = frameworkRef();
if (!REF) {
  console.log('the three places naming the release DISAGREE, so there is nothing to pin to.');
  console.log('SKIPPED: run npm run check:version.');
  process.exit(0);
}

let iface;
try {
  iface = await fetchInterface(REF);
  console.log(`read ${interfaceSource(REF)}`);
} catch (err) {
  console.log(`z2ui5_if_client at ${REF}: not resolved (${err.message})`);
  console.log('SKIPPED: nothing was verified.');
  process.exit(0);
}

/* ------------------------------------------------------------- the contract */

/** method name -> Set of IMPORTING parameter names. */
const methods = new Map();
/** constant group -> Set of member names, keyed by the group's FULL path:
 *  `cs_device` holds `system`, and `cs_device-system` holds `phone`. Keyed by
 *  the inner name alone, `cs_device-system-phone` could only ever be checked
 *  as far as `system` - so `cs_device-system-phonee` passed - and the inner
 *  names are not unique either: `browser` and `os` are groups of cs_device
 *  AND of ty_s_get's s_device, with different members. */
const groups = new Map();

{
  let method = null;
  let importing = false;
  const open = [];
  for (const raw of iface.split(/\r?\n/)) {
    // a comment can hold anything, including the names this gate looks for
    const line = raw.replace(/^\s*"[!]?.*$/, '').replace(/\s"\s.*$/, '').trimEnd();

    const begin = /^\s*BEGIN OF ([a-z_0-9]+),?\s*$/i.exec(line);
    if (begin) {
      const name = begin[1].toLowerCase();
      // the nested group is itself a member of what encloses it: a page
      // spelling the full path (`cs_device-system-phone`) walks through it
      if (open.length) groups.get(open.join('-')).add(name);
      open.push(name);
      groups.set(open.join('-'), new Set());
      continue;
    }
    if (/^\s*END OF ([a-z_0-9]+)/i.test(line)) { open.pop(); continue; }
    if (open.length) {
      const member = /^\s*([a-z_0-9]+)\s+TYPE\s/i.exec(line);
      if (member) groups.get(open.join('-')).add(member[1].toLowerCase());
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

if (methods.size === 0 || !groups.has('cs_event') || !groups.has('cs_device-system')) {
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

/** The fenced ABAP blocks of a page, as one string with everything else blanked.
 *
 *  Line by line, the way markdown reads a fence: it opens at the start of a
 *  line and closes on a line of the same mark and nothing else. The regex
 *  this replaces opened a "fence" at any three backticks - the inline code
 *  span that prints the empty ABAP literal is one - paired fences from there
 *  on by position, and blanked nothing outside them: the prose went through
 *  as if it were ABAP. */
const abapOnly = (text) => {
  let fence = null;
  return text.split('\n').map((line) => {
    const mark = /^ {0,3}(`{3,}|~{3,})\s*([^\s`]*)/.exec(line);
    if (fence) {
      if (mark && mark[1][0] === fence.mark[0] && mark[1].length >= fence.mark.length && !mark[2]) {
        fence = null;
        return '';
      }
      return fence.abap ? line : '';
    }
    if (mark) fence = { mark: mark[1], abap: /^(abap)?$/i.test(mark[2]) };
    return '';
  }).join('\n');
};

/** Blank ABAP string literals and comments, so an XML attribute inside a
 *  literal is not a parameter - but keep the embedded expressions of a string
 *  template, which are code: `|{ client->_bind( val = x path = abap_true ) }|`
 *  is a call like any other, and blanking the template whole let any
 *  parameter name through inside one. An embedded expression may run over
 *  several lines (formatter.md breaks a _bind( ) call inside one); a literal
 *  may not, so only the `{` survives a line end. */
const withoutLiterals = (text) => {
  const open = [];          // what is open: ` ' | (literals), { (code inside a template)
  return text.split('\n').map((line) => {
    while (open.length && open.at(-1) !== '{') open.pop();
    if (!open.length && line.startsWith('*')) return '';
    let out = '';
    for (let i = 0; i < line.length; i++) {
      const c = line[i];
      const top = open.at(-1);
      if (top === '`' || top === "'") {
        if (c === top && line[i + 1] === top) { out += '  '; i++; continue; }   // doubled: an escaped delimiter
        if (c === top) open.pop();
        out += c === top ? c : ' ';
        continue;
      }
      if (top === '|') {
        if (c === '\\') { out += '  '; i++; continue; }                       // \| \{ \} \\ and the rest
        if (c === '{') open.push('{');
        else if (c === '|') open.pop();
        out += c === '{' || c === '|' ? c : ' ';
        continue;
      }
      if (c === '`' || c === "'" || c === '|') { open.push(c); out += c; continue; }
      if (c === '}' && top === '{') { open.pop(); out += c; continue; }
      if (c === '"') break;                                                    // a comment, to the end of the line
      out += c;
    }
    return out;
  }).join('\n');
};

/* The frozen package, by the name segments renaming.md documents for it
 * (`util`, `pop`, `xml_view`) plus the four objects that were relocated into
 * it whole. A name is matched wherever it is written - prose or code. */
const FROZEN = /\bz2ui5_(?:cl_(?:pop_|util|xml_view|http_handler)\w*|if_(?:exit|types)|cx_util_error|t_91)\b/gi;

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
          problems.push(`${rel}: \`client->${name}( )\` is not a method of z2ui5_if_client on ${REF}`);
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
            `${rel}: \`client->${name}( ${param} = ... )\` - no such parameter on ${REF}\n`
            + `    it takes: ${[...methods.get(name)].join(', ') || '(none)'}`,
          );
        }
      }
    }
  }

  // 5: nothing from the frozen package, wherever it is written
  for (const use of text.matchAll(FROZEN)) {
    checked += 1;
    problems.push(`${rel}: \`${use[0]}\` is in the framework's frozen package (src/99) - resources/deprecations.md is the one page that names it`);
  }

  // 3: cs_<group>-<member>, wherever it is written - every segment of it,
  // down through the nested groups (`cs_device-system-phone`)
  for (const use of text.matchAll(/\b(cs_[a-z_0-9]+)((?:-[a-z_0-9]+)+)/gi)) {
    let at = use[1].toLowerCase();
    if (!groups.has(at)) continue;
    checked += 1;
    for (const member of use[2].slice(1).toLowerCase().split('-')) {
      const members = groups.get(at);
      if (!members) break;          // a constant, not a group: the name ended before this
      if (!members.has(member)) {
        problems.push(`${rel}: \`${use[0]}\` is not in ${at} on ${REF}`);
        break;
      }
      at = `${at}-${member}`;
    }
  }
}

/* 4: the source links. Distinct URLs only - the same file is linked from
 * several pages - and one HEAD each, which is a handful of requests. A link
 * the network could not resolve is SKIPPED, by itself: it used to skip the
 * whole question, so one timeout threw away every 404 that HAD come back. */
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
  const targets = [...seen.keys()];
  const unresolved = targets.filter((_, i) => verdicts[i] === undefined);
  if (unresolved.length) {
    console.log(`source links: ${unresolved.length} of ${targets.length} not resolved (network) - skipped:`);
    for (const target of unresolved) console.log(`  ${target}`);
    links -= unresolved.length;
  }
  for (const target of verdicts.filter(Boolean)) {
    problems.push(
      `${[...seen.get(target)].join(', ')}: links ${target} in abap2UI5, which is not there on main\n`
      + '    the file moved or was deleted - a link every reader who clicks it gets a 404 from',
    );
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
console.log('every client-> name on the site exists in the framework it tracks - OK');
