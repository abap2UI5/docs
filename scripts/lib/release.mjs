/*
 * release — where this documentation writes the framework release number, and
 * how to read it back.
 *
 * Two gates need the same answer and must not drift apart in how they get it:
 *
 *   check-version   holds the three places against each other and against the
 *                   newest release tag of abap2UI5/abap2UI5
 *   check-examples  compiles every documentation example against that release,
 *                   so a page cannot teach API the reader's install does not
 *                   have
 *
 * check-version is what keeps the number honest; check-examples is entitled to
 * trust it because of that, and reads it from here rather than carrying a
 * second copy of the pattern.
 */
import fs from 'fs';
import path from 'path';

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
    /* Any heading level, because the LEVEL is not what this gate is about and
     * pinning it made the gate fail for the wrong reason: closing up the
     * heading-level gaps across the site turned these from `###` into `##`,
     * and a check on the release NUMBER went red over a `#`. It reported that
     * honestly - "the file changed shape, fix the pattern" - rather than
     * silently matching nothing, which is the failure mode it was written to
     * avoid. Still: a gate that fires on a change it does not care about is a
     * gate people learn to route around. */
    re: /^#{2,4}\s+(\d+\.\d+\.\d+)\s*$/m,
  },
];

/**
 * Read the release number out of the three places above.
 * @returns {{found: Array, problems: string[]}}
 */
export function readSites(ROOT) {
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
  return { found, problems };
}

/**
 * The one release number this documentation names, or null when the three
 * places disagree or cannot be read - the caller decides whether that is
 * fatal. check-version reports it properly; check-examples falls back.
 */
export function declaredRelease(ROOT) {
  const { found } = readSites(ROOT);
  const distinct = [...new Set(found.map((f) => f.version))];
  return distinct.length === 1 ? distinct[0] : null;
}
