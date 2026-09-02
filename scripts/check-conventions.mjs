#!/usr/bin/env node
// Checks the ABAP examples in this documentation against the two house
// conventions the sample repositories gate, and this one did not.
//
// Why a gate of its own, next to check-examples.mjs: that one asks whether an
// example COMPILES and whether the view it builds names API that exists. Both
// questions are about the framework. Neither can see that a snippet is written
// in a different style from every sample a reader lands on next - and a
// documentation site is where the style is learned. A survey against
// abap2UI5/samples-controls (637 classes, all gated) found the two places
// where the pages had drifted:
//
//   chain layout    5 chains whose indentation showed the reader a different
//                   tree than the one that renders - the exact failure the
//                   `view-chain-layout` skill exists for. samples-controls
//                   gates this as `npm run check:chains` and stands at zero;
//                   here nothing looked, because the linter's rule is OPT-IN
//                   and check-examples.mjs writes its config without a `rules`
//                   block, so the rule was never emitted at all.
//
//   class sections  57 of 86 app-class definitions carried neither
//                   PROTECTED SECTION. nor PRIVATE SECTION. AGENTS.md in
//                   abap2UI5 asks for all three blocks in every class, even
//                   empty; the sample corpus has them in 637 of 637. A reader
//                   copying a snippet copies the shell as well.
//
// Both are decidable, which is the whole test for whether something belongs in
// this repository's CI. What is NOT here, deliberately: the blank-line rules
// around `end( )` and the `t_arg` continuation column. They are pattern-lint
// WARNINGS over in samples-controls, and that corpus itself carries 382 of
// them - gating here what the reference corpus does not gate there would make
// this repository the strictest one in the organisation by accident.
//
// Usage: node scripts/check-conventions.mjs [--fix]
//
// `--fix` reformats the drifted chains in place (npm run fmt:chains). The
// rewrite is the linter's own, whitespace-only and verified as such - a layout
// fix can never change what the view builds. The sections are NOT auto-fixed:
// where a block goes is a judgement about the class, not about whitespace.

import { readFileSync, writeFileSync, readdirSync, statSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { checkAbapSource } from '@abap2ui5/linter';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const DOCS = join(ROOT, 'docs');
const FIX = process.argv.includes('--fix');

/* Same walk as check-examples.mjs, and for the same reason: `public` holds a
 * raw-markdown COPY of every page, written on each build, so walking into it
 * would report every finding twice and once under a path nobody edits. */
const walk = (dir) =>
  readdirSync(dir).flatMap((e) => {
    const p = join(dir, e);
    if (e === '.vitepress' || e === 'node_modules' || e === 'public') return [];
    return statSync(p).isDirectory() ? walk(p) : [p];
  });

/** Every fenced abap block on a page, with the line its first line sits on. */
function fences(md) {
  const lines = md.split('\n');
  const out = [];
  for (let i = 0; i < lines.length; i++) {
    if (!lines[i].trim().startsWith('```abap')) continue;
    const start = i + 1;
    let j = start;
    while (j < lines.length && !lines[j].trim().startsWith('```')) j++;
    out.push({ start, end: j, code: lines.slice(start, j).join('\n') });
    i = j;
  }
  return out;
}

const pages = walk(DOCS).filter((f) => f.endsWith('.md')).sort();

/* ------------------------------------------------------------------ chains */

/* The rule is opt-in in the linter (it encodes ONE house style), so a `rules`
 * entry is what turns it on - without one the run is green no matter how
 * mangled the chain. `properties: false` keeps this to the layout: whether a
 * control exists is check-examples.mjs's question and is asked there against
 * the framework at the pinned release. */
const CHAIN_OPTS = { properties: false, rules: { 'chain-house-layout': 'warning' } };

const chainFindings = [];
let chainsJudged = 0;

for (const file of pages) {
  const page = file.slice(ROOT.length + 1);
  const md = readFileSync(file, 'utf8');
  const rewrites = [];

  for (const fence of fences(md)) {
    /* A fence on the frozen builder reconstructs no chain and is check-
     * examples.mjs's business (it fails an unmigrated page and exempts the one
     * page that shows the old API on purpose). Nothing to judge here. */
    if (!/z2ui5_cl_ui5_view_builder/i.test(fence.code)) continue;
    const result = checkAbapSource(fence.code, CHAIN_OPTS);
    if (!result.usesBuilder) continue;
    chainsJudged++;
    const found = result.findings.filter((f) => f.type === 'chain-house-layout');
    if (!found.length) continue;
    if (FIX) {
      rewrites.push({ fence, fixes: found.flatMap((f) => f.fixes ?? []) });
      continue;
    }
    for (const f of found) {
      chainFindings.push({ page, line: fence.start + f.line - 1, count: f.count });
    }
  }

  if (!rewrites.length) continue;
  /* Apply the linter's own fixes inside each fence, then splice the fences
   * back from the bottom up so the earlier line numbers stay valid. */
  const lines = md.split('\n');
  for (const { fence, fixes } of rewrites.sort((a, b) => b.fence.start - a.fence.start)) {
    let code = fence.code;
    for (const fix of [...fixes].sort((a, b) => b.start - a.start)) {
      code = code.slice(0, fix.start) + fix.text + code.slice(fix.end);
    }
    lines.splice(fence.start, fence.end - fence.start, ...code.split('\n'));
  }
  writeFileSync(file, lines.join('\n'));
  console.log(`reformatted: ${page}  (${rewrites.length} chain(s))`);
}

/* ---------------------------------------------------------------- sections */

/* An app class in this documentation is a class whose DEFINITION declares
 * z2ui5_if_app - the one shape a reader copies as a whole. Anything else in a
 * fence (an ICF handler, a local helper, an exit implementation) is a
 * different animal with a different shell, and is left alone. */
const sectionFindings = [];
let classesJudged = 0;

for (const file of pages) {
  const page = file.slice(ROOT.length + 1);
  const md = readFileSync(file, 'utf8');
  for (const fence of fences(md)) {
    const lines = fence.code.split('\n');
    for (let i = 0; i < lines.length; i++) {
      if (!/^\s*CLASS\s+\S+\s+DEFINITION/i.test(lines[i])) continue;
      let j = i;
      while (j < lines.length && !/^\s*ENDCLASS\s*\./i.test(lines[j])) j++;
      if (j >= lines.length) break;                       // a definition cut off mid-fence
      const block = lines.slice(i, j + 1).join('\n');
      i = j;
      if (!/INTERFACES\s+z2ui5_if_app/i.test(block)) continue;
      classesJudged++;
      const missing = [
        [/^\s*PUBLIC\s+SECTION\s*\./im, 'PUBLIC SECTION.'],
        [/^\s*PROTECTED\s+SECTION\s*\./im, 'PROTECTED SECTION.'],
        [/^\s*PRIVATE\s+SECTION\s*\./im, 'PRIVATE SECTION.'],
      ].filter(([re]) => !re.test(block)).map(([, name]) => name);
      if (missing.length) {
        sectionFindings.push({ page, line: fence.start + lines.slice(0, j + 1).length - 1, missing });
      }
    }
  }
}

/* ----------------------------------------------------------------- verdict */

console.log(`check-conventions: ${chainsJudged} view chain(s) and ${classesJudged} app class(es) in fenced examples`);

if (chainFindings.length) {
  console.error(`\n${chainFindings.length} chain(s) are not in the house layout — one call per line including`);
  console.error('attributes, four spaces per level of the tree, end( ) in the column of the ele( )');
  console.error('it closes:\n');
  for (const f of chainFindings) console.error(`  - ${f.page}:${f.line}  ${f.count} line(s) differ`);
  console.error('\nnpm run fmt:chains rewrites them. The fix is whitespace-only and verified as');
  console.error('such — it cannot change what the view builds.');
}

if (sectionFindings.length) {
  console.error(`\n${sectionFindings.length} app class(es) do not carry all three section blocks:\n`);
  for (const f of sectionFindings) console.error(`  - ${f.page}:${f.line}  missing ${f.missing.join(', ')}`);
  console.error('\nEvery class in abap2UI5 and in both sample corpora has all three, empty or not');
  console.error('(abap2UI5 AGENTS.md, "Class sections"). A reader copies the shell with the class.');
}

if (chainFindings.length || sectionFindings.length) process.exit(1);
if (!FIX) console.log('             layout and class shell match the sample corpora');
