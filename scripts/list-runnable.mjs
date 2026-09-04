#!/usr/bin/env node
/*
 * The worklist for the Run-button measurement: every fenced example on the
 * site that carries a Run button, with the ABAP the button would hand the
 * playground.
 *
 * WHY THIS EXISTS. `check:playground` decides the bookkeeping - every complete
 * app class either gets a button or carries a marker saying why it cannot run
 * - and says plainly what it cannot decide: whether a BUTTONED example
 * actually starts. That stays a measurement in a browser, and AGENTS.md
 * describes it as one. What it did not have was its input: whoever redoes it
 * had to find the examples by hand, and "63 with a button" is a number the
 * gate prints rather than a list anybody can act on.
 *
 * So this prints the list, out of the same `playground.mjs` that decides the
 * button - never a second copy of those rules. With `--json` it prints the
 * examples with their ABAP, ready to drive a served playground with; the
 * driving half lives where the browser harness already is (abap2UI5/playground
 * has one), not in a documentation repository that would need Playwright as a
 * dependency to run it once a quarter.
 *
 *   npm run runnable            the list, one line per example
 *   npm run runnable -- --json  the same plus each example's ABAP, as JSON
 *
 * The ABAP is the fence VERBATIM, which is what the Run button sends: the
 * client reads `pre code`'s textContent off the rendered page. Not
 * `abapOnly( )` - that one blanks every literal so a rule cannot be tripped by
 * prose, and a view built from blanked literals has no control names left in
 * it at all.
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { isRunnable, playgroundExample } from '../docs/.vitepress/playground.mjs';

const ROOT = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const DOCS = path.join(ROOT, 'docs');
const JSON_OUT = process.argv.includes('--json');

/** Every page under docs/, minus the two directories that hold no pages. */
function pages(dir, found = []) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (entry.name !== 'public' && entry.name !== '.vitepress') pages(full, found);
    } else if (entry.name.endsWith('.md')) {
      found.push(full);
    }
  }
  return found;
}

const examples = [];
for (const file of pages(DOCS)) {
  const md = fs.readFileSync(file, 'utf8');
  for (const fence of md.matchAll(/```abap\n([\s\S]*?)```/g)) {
    const code = fence[1];
    if (!isRunnable(code)) continue;
    examples.push({
      page: path.relative(DOCS, file),
      class: playgroundExample(code).name,
      abap: code,
    });
  }
}

if (JSON_OUT) {
  process.stdout.write(`${JSON.stringify(examples, null, 1)}\n`);
} else {
  for (const e of examples) {
    console.log(`${e.page}  ${e.class}`);
  }
  const onPages = new Set(examples.map((e) => e.page)).size;
  console.log(`\nlist-runnable: ${examples.length} example(s) with a Run button on ${onPages} page(s)`);
  console.log('These are what the measurement opens - see AGENTS.md, "The Run button".');
}
