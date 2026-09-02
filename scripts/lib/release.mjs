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

/**
 * WHICH framework do the API gates judge a page against?
 *
 * Until 2026-09-02: the release above. The reasoning was that a reader
 * installs a RELEASE, so a page is correct when it matches one - and main is
 * ahead of that by design, so judging against main would pass a page teaching
 * API that does not exist yet.
 *
 * That answered the wrong half of the question. The maintainer's cadence is
 * monthly releases and daily merges, and the same coupling had already been
 * cut everywhere else: the sample corpora resolve the framework's main branch
 * (abap2UI5's .github/shared/check-framework-pin.mjs, adopted by samples#815,
 * samples-stack#67, samples-controls#179), because "releases never gate a
 * merge". This repository was the last one still waiting for a tag. It cost
 * exactly what the framework predicted: the hash_* / app_state_* API landed on
 * main on 2026-08-31 and these pages could not be corrected to it - not
 * "should not", COULD not, the gate rejected the new names - so the site kept
 * teaching the old spellings while the samples the pages link to had already
 * migrated to the new ones.
 *
 * So: main. The docs and the framework advance together and a tag is cut for
 * both. What a reader on the newest RELEASE does not have yet is a question of
 * PROSE - resources/deprecations.md carries the "next release" column for
 * exactly that - and check-version keeps the release number in the nav bar,
 * the deprecations page and the changelog honest, which is where the reader
 * looks for "what do I have". A2UI5_REF still overrides, now for pinning a
 * gate run BACK to a release rather than forward to main.
 */
export function frameworkRef() {
  return process.env.A2UI5_REF || 'main';
}
