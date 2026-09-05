/*
 * The eleven gates are written out in three places. Do all three name the
 * same set?
 *
 * `package.json`'s `check` script is what a contributor runs; check.yml is
 * what a pull request has to pass; deploy.yml is what stands in front of the
 * published site. Nothing compared them, and they had already drifted twice
 * in the same direction - a gate in one list and not in another, which is a
 * gate that can be walked past:
 *
 *   - `npm test` was in the script and in neither workflow, and went a
 *     release without CI - the pin added BECAUSE the catalogue parser broke
 *     twice in silence;
 *   - `check:conventions` was in the script and in check.yml but not in
 *     deploy.yml, from the day it was added, so main could be published past
 *     the one gate that reads the house style.
 *
 * Both were found by eye, weeks apart. This is the same reading done by a
 * machine, in `npm test`, so a twelfth gate added to one list and forgotten
 * in the other two is red before it is merged rather than after.
 *
 * The ORDER is deliberately not compared: deploy.yml has to build the site it
 * publishes, so `docs:build` and `check:cross-site` - which judges the built
 * HTML - necessarily come last there. What has to match is the set.
 *
 *   npm test
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const read = (f) => readFileSync(join(ROOT, f), 'utf8');

/** Every gate the `check` script chains, as the npm script names it. */
function scriptGates() {
  const pkg = JSON.parse(read('package.json'));
  const chain = pkg.scripts.check;
  assert.ok(chain, 'package.json has a `check` script');
  return chain
    .split('&&')
    .map((part) => part.trim().replace(/^npm run /, ''))
    .filter(Boolean);
}

/** Every gate a workflow runs, by the npm script behind the step. */
function workflowGates(file) {
  const yml = read(file);
  const out = [];
  for (const m of yml.matchAll(/^\s*run:\s*(npm (?:run [\w:-]+|test|ci))\s*$/gm)) {
    const cmd = m[1].replace(/^npm run /, '').replace(/^npm /, '');
    if (cmd === 'ci') continue; // installing is not a gate
    out.push(cmd);
  }
  return out;
}

/* The floor. A pattern that matched nothing would compare three empty sets
 * and report that they agree - which is the exact failure this file exists to
 * catch, one level up. */
test('all three lists are actually read', () => {
  assert.ok(scriptGates().length >= 5, 'the check script chains gates');
  for (const f of ['.github/workflows/check.yml', '.github/workflows/deploy.yml']) {
    assert.ok(workflowGates(f).length >= 5, `${f} runs gates`);
  }
});

test('check.yml runs every gate the check script does, and no other', () => {
  assert.deepEqual(new Set(workflowGates('.github/workflows/check.yml')), new Set(scriptGates()));
});

test('deploy.yml runs every gate the check script does, and no other', () => {
  /* The one that had drifted. A gate missing here is a gate main can be
   * published past, and nothing about the deploy would look wrong. */
  assert.deepEqual(new Set(workflowGates('.github/workflows/deploy.yml')), new Set(scriptGates()));
});

test('no gate is run twice in one workflow', () => {
  for (const f of ['.github/workflows/check.yml', '.github/workflows/deploy.yml']) {
    const gates = workflowGates(f);
    assert.equal(gates.length, new Set(gates).size, `${f} lists a gate twice`);
  }
});

test('check.yml keeps the script order, so a green run locally is a green run there', () => {
  assert.deepEqual(workflowGates('.github/workflows/check.yml'), scriptGates());
});

test('the documents say eleven, and there are eleven', () => {
  /* AGENTS.md, README.md, CONTRIBUTING.md and CLAUDE.md all count them in
   * prose. A twelfth gate that left the four documents saying "eleven" is the
   * same drift as a gate missing from a workflow, one document over. */
  const gates = scriptGates();
  assert.equal(gates.length, 11, `the count in the four documents is 11, the lists have ${gates.length}`);
  for (const doc of ['AGENTS.md', 'README.md', 'CONTRIBUTING.md', 'CLAUDE.md']) {
    assert.match(read(doc), /eleven/, `${doc} counts the gates`);
  }
});
