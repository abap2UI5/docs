/*
 * facts — the counted figures the front door prints (the Evaluate page, now in the playground, reads the same block).
 *
 * This repository owns no number about the framework, and keeps none by hand:
 * the one page that carried four figures of its own (`resources/samples.md`)
 * went stale and was taken out (AGENTS.md, "Build & verify"). What a reader
 * who is deciding still wants is the size of the thing - how many tests hold
 * it, how many gates a change has to pass - so the figures are READ at build
 * time from where they are counted: the `facts:start` / `facts:end` block in
 * the framework's README, which `npm run facts` over there regenerates from
 * the tree and a gate holds current. Plus the one figure GitHub counts, the
 * stars.
 *
 * Both reads are allowed to fail. No network, a 5xx, a README whose block
 * moved - each costs the LINE, never the build, and the build says so. A
 * figure this site cannot read is a figure it does not print; it never falls
 * back to a number somebody typed.
 *
 * The README is read the way the API gates read the interface
 * (lib/client-interface.mjs): `A2UI5_HOME` names a clone and the file comes
 * out of it with `git show`, from `main` or `origin/main`, never the working
 * tree; without it, raw.githubusercontent.com.
 */
import { execFileSync } from 'node:child_process';

const README_URL = 'https://raw.githubusercontent.com/abap2UI5/abap2UI5/main/README.md';
const REPO_URL = 'https://api.github.com/repos/abap2UI5/abap2UI5';

/** What the block carries that this site does not repeat: the release span
 *  (the tagline says it - the arrow is its mark), the workflow count (a maintainer's figure, not a
 *  reader's) and the trailing "counted from this repository by …". */
const SKIP = /workflows|→|counted/;

const readmeFromClone = (home) => {
  const tried = [];
  for (const spec of ['main', 'origin/main']) {
    try {
      return execFileSync('git', ['-C', home, 'show', `${spec}:README.md`], {
        encoding: 'utf8',
        stdio: ['ignore', 'pipe', 'pipe'],
      });
    } catch (err) {
      tried.push(`${spec}: ${String(err.stderr ?? err.message).trim().split('\n')[0]}`);
    }
  }
  throw new Error(`README.md not in the clone at ${home} - ${tried.join('; ')}`);
};

const fetchJson = async (url) => {
  const res = await fetch(url, {
    signal: AbortSignal.timeout(15000),
    headers: { accept: 'application/vnd.github+json', 'user-agent': 'abap2ui5-docs-build' },
  });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.json();
};

/** The figures of the README block as plain strings - "1,529 ABAP unit
 *  tests", "51 CI gates" - or an empty array when the block is not there. */
export function factsIn(readme) {
  const block = readme.match(/<!-- facts:start -->([\s\S]*?)<!-- facts:end -->/);
  if (!block) return [];
  const text = block[1].replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ').trim();
  return text.split(/\s·\s/).map((s) => s.trim()).filter((s) => s && !SKIP.test(s));
}

/**
 * The line to print, as an array of figures, and where each half came from.
 * @returns {Promise<{items: string[], notes: string[]}>}
 */
export async function readFacts({ home = process.env.A2UI5_HOME || null, fetchImpl = fetch } = {}) {
  const items = [];
  const notes = [];
  try {
    const readme = home ? readmeFromClone(home) : await (async () => {
      const res = await fetchImpl(README_URL, { signal: AbortSignal.timeout(15000) });
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return res.text();
    })();
    const found = factsIn(readme);
    if (found.length) {
      items.push(...found);
      notes.push(`facts: ${found.length} figure(s) from ${home ? `README.md at main in the clone at ${home} (A2UI5_HOME)` : README_URL}`);
    } else {
      notes.push('facts: the README carries no facts block - the line prints without the counts');
    }
  } catch (err) {
    notes.push(`facts: README not read (${err.message}) - the line prints without the counts`);
  }
  try {
    const repo = await fetchJson(REPO_URL);
    if (Number.isInteger(repo.stargazers_count)) {
      items.push(`${repo.stargazers_count.toLocaleString('en-US')} GitHub stars`);
      notes.push(`facts: stars from ${REPO_URL}`);
    }
  } catch (err) {
    notes.push(`facts: stars not read (${err.message}) - the line prints without them`);
  }
  return { items, notes };
}
