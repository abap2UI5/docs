# AGENTS.md

The abap2UI5 documentation site (VitePress). Every other repository in the
organisation has one of these; this one did not, which is part of why it drifted
out of the toolchain bump that reached all the others.

**This site is written for people.** Instructions aimed at AI agents were
deliberately taken out of the pages — an agent reads `llms.txt` (below), a
person reads the page. Do not put "as an AI, …" prose back into `docs/`.

## Layout

| Path | Purpose |
| --- | --- |
| `docs/` | The pages. Sidebar and nav live in `docs/.vitepress/config.mjs` |
| `docs/public/` | Static assets — **and** the generated `llms.txt`, `llms-full.txt` and per-page `.md`, which are gitignored |
| `scripts/check-examples.mjs` | Extracts every fenced ABAP block that builds a view, compiles it against the real framework and lints the view it produces |
| `scripts/link-samples.mjs` | Generates the *Working Samples* block on a page from its `samples:` frontmatter plus `SAMPLES.md` in an `abap2UI5/samples` checkout, and checks the link in both directions |
| `scripts/generate-llms.mjs` | Builds `llms.txt` / `llms-full.txt` / per-page markdown from the sidebar. Runs inside `docs:build`, so the deploy publishes them |
| `scripts/check-version.mjs` | The release number in the nav bar, the deprecations page and the changelog, against the newest release tag of the framework |
| `scripts/lib/catalogue.mjs` | Parses a sample catalogue's rows; pinned by `test/catalogue.test.mjs`, because it has stopped matching twice and both times answered wrongly instead of failing |

## Build & verify — run before every commit

```bash
npm run check          # test + check:version + docs:build + check:examples + check:samples + check:counts
```

`.github/workflows/check.yml` runs the same six, in the same order. Keep the
two in step: a step that exists only in `package.json` is a step no pull
request has to pass, which is how `npm test` — the pin added *because* the
catalogue parser broke twice in silence — went a release without CI.

`check:samples` needs an `abap2UI5/samples` checkout — set `SAMPLES_HOME`, or
clone it as a sibling. Without one it *skips* rather than fails, so verify the
output says what you think it says. CI checks out `abap2UI5/samples@main`
explicitly for this reason.

`check:counts` reads all three catalogues the same way, and skips per
repository: with only `samples` at hand it verifies that one figure, says the
other two were not verified, and leaves the total alone (it needs all three).
CI sparse-checks out `SAMPLES.md` from `samples-controls` and `samples-stack`
so the page is fully checked; both are `continue-on-error`, because an
unreachable repository must cost a figure and not the run.

## Things that will trip you up

- **The nav bar and the sidebar contain byte-identical lines.**
  `Contribution` and `Sponsor` appear in both `themeConfig.nav` and
  `themeConfig.sidebar` in `config.mjs`. A replace-first edit hits the wrong
  one and looks like it worked. Verify by reading the built config, not by
  grepping the source.
- **A fenced ABAP example is code, and it is checked.** `check:examples`
  compiles it and lints the view. It also refuses `z2ui5_cl_xml_view=>` — the
  frozen builder — unless the page carries the migration banner, and refuses a
  chain step calling anything but `ele` / `tag` / `a` / `end` / `stringify`,
  whether it is written mid-chain (`)->input( )`) or on its receiver
  (`page->input( )`). Examples are the most-copied ABAP in the project; that
  gate is the reason the pages could be migrated at all.
- **`llms.txt` is generated from the SIDEBAR, not from a directory walk.** A
  page in no sidebar is reported as an orphan and published anyway. If you add
  a page, add it to the sidebar or accept that nothing navigates to it.
- **The summaries in `llms.txt` keep the underscore.** `*` and backticks are
  stripped, `_` is not: almost every identifier here carries one (`_bind`,
  `check_on_init`, `s_device`), and an index advertising `client->bind( )` is a
  plausible, citable, wrong API name aimed at the reader least able to catch it.
- **A generated block in a page is committed.** The *Working Samples* blocks are
  written into the markdown so the site builds without a samples checkout. Run
  `npm run link:samples` after changing a page's `samples:` frontmatter;
  `check:samples` fails if a rewrite would change anything.

## Toolchain

Node 22, matching the rest of the organisation. Actions are pinned to commits
with the tag in a comment; `.github/dependabot.yml` moves both the npm packages
and the action pins every Monday. The `@abap2ui5/linter` version decides what
`check:examples` catches, so a bump there is a content decision, not just a
dependency one — read what the new rules say before merging it.
