#!/usr/bin/env node
/*
 * check-version — the release number this documentation names has to be the
 * release that exists.
 *
 * What this was written for, on the day it was written: abap2UI5 1.143.0 was
 * published at 12:20, and for the rest of that day the documentation said the
 * released version was 1.142.0 - in the navigation bar, in the deprecations
 * page, and by omission in the changelog. Worse than the number: the
 * deprecations page carried a warning box reading "Not in 1.142.0 -
 * z2ui5_cl_ui5_view_builder arrives with the next release", so the page a
 * reader opens to find out whether they may use the current view builder told
 * them no, hours after it shipped.
 *
 * Nothing was broken to make that happen. The number simply lives in three
 * hand-maintained places here and moves in a different repository, and prose
 * has no compiler. Which is the whole argument for a gate: the release is a
 * FACT about another repository, and a fact can be checked.
 *
 * Three places, and they must agree with each other and with the newest
 * release tag of abap2UI5/abap2UI5:
 *
 *   docs/.vitepress/config.mjs    the version in the nav bar
 *   docs/resources/deprecations.md   "The released version is **x.y.z**"
 *   docs/resources/changelog.md      the topmost "### x.y.z" heading
 *
 * The tag is fetched over the network. When that fails - offline, rate limit,
 * a sandbox with no route to github.com - the three are still checked against
 * EACH OTHER and the run says clearly that the outside half did not happen.
 * A documentation build should not go red because GitHub is unreachable, and
 * it should not quietly claim to have verified something it did not.
 *
 *   node scripts/check-version.mjs        (npm run check:version)
 */
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const API = 'https://api.github.com/repos/abap2UI5/abap2UI5/releases/latest';

/* The framework publishes each release twice: `1.143.0` and, minutes later,
 * `1.143.0-702` - the same code downported for NetWeaver 7.02, shipped as its
 * own repository. So `releases/latest` answers `1.143.0-702` for most of the
 * time after a release, and it is not a different version: it is the same
 * version of a second distribution, and the documentation names the version.
 *
 * Found by running this gate rather than by reasoning about it - the first run
 * against the real API failed on a repository that had just been corrected. */
const versionOf = (tag) => tag.replace(/-\w+$/, '');

/** Where the version is written, and how to find it in each file. */
const SITES = [
  {
    file: 'docs/.vitepress/config.mjs',
    what: 'the version in the nav bar',
    re: /text:\s*"(\d+\.\d+\.\d+)"/,
  },
  {
    file: 'docs/resources/deprecations.md',
    what: 'the "Version status" sentence',
    re: /The released version is \*\*(\d+\.\d+\.\d+)\*\*/,
  },
  {
    file: 'docs/resources/changelog.md',
    what: 'the newest release heading',
    re: /^###\s+(\d+\.\d+\.\d+)\s*$/m,
  },
];

const problems = [];
const found = [];

for (const site of SITES) {
  const full = path.join(ROOT, site.file);
  if (!fs.existsSync(full)) {
    problems.push(`${site.file}: gone — this gate names it as one of the places the version lives`);
    continue;
  }
  const m = site.re.exec(fs.readFileSync(full, 'utf8'));
  if (!m) {
    problems.push(
      `${site.file}: no version found where ${site.what} should be\n`
      + '    the file changed shape — fix the pattern in scripts/check-version.mjs,\n'
      + '    or this gate silently stops checking that place',
    );
    continue;
  }
  found.push({ ...site, version: m[1] });
}

/* Half the check needs no network: three places naming three different
 * versions is wrong whatever the tag says. */
const distinct = [...new Set(found.map((f) => f.version))];
if (distinct.length > 1) {
  problems.push(
    `the three places disagree: ${distinct.join(' / ')}\n`
    + found.map((f) => `      ${f.version}  ${f.file} (${f.what})`).join('\n'),
  );
}

/* And the half that does. */
let tag = null;
let latest = null;
let why = '';
try {
  const res = await fetch(API, {
    headers: { accept: 'application/vnd.github+json', 'user-agent': 'abap2ui5-docs-check-version' },
    signal: AbortSignal.timeout(15000),
  });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  tag = (await res.json()).tag_name;
  latest = versionOf(tag);
} catch (err) {
  why = err.message;
}

if (latest) {
  for (const f of found) {
    if (f.version !== latest) {
      problems.push(
        `${f.file}: ${f.what} says ${f.version}, the newest release is ${latest}`,
      );
    }
  }
}

console.log(`check-version: ${found.length} place(s) name a version`);
for (const f of found) console.log(`  ${f.version}  ${f.file} — ${f.what}`);
console.log(
  latest
    ? `newest release of abap2UI5/abap2UI5: ${latest}`
      + (tag === latest ? '' : `  (tag ${tag} — the 7.02 downport of the same version)`)
    : `could NOT reach the release API (${why}) — the three places were checked\n`
      + '  against each other only. Whether they match the actual release is UNVERIFIED.',
);

if (problems.length) {
  console.error(`\n${problems.length} problem(s):`);
  for (const p of problems) console.error(`  ${p}`);
  console.error('\nA release moves in another repository and nothing here moves with it.');
  console.error('Update all three places together — and read the pages around them: a');
  console.error('sentence like "arrives with the next release" goes stale with the number.');
  process.exit(1);
}
console.log('the documentation names the release that exists - OK');
