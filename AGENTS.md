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
| `scripts/generate-api-reference.mjs` | Generates the client API reference — the block in `docs/resources/api.md` and `docs/public/api/client-api.json` — from `z2ui5_if_client` on the framework branch this site tracks (`main`); `--check` is the freshness gate |
| `scripts/lib/client-interface.mjs` | Where `z2ui5_if_client` is fetched from (the ref comes from `lib/release.mjs`, shared with `check-api-names.mjs`) and the full parser `generate-api-reference.mjs` renders from |
| `scripts/check-version.mjs` | The release number in the nav bar, the deprecations page and the changelog, against the newest release tag of the framework |
| `docs/.vitepress/playground.mjs` | Decides which fenced ABAP example gets a **Run** button, and wraps the fence; `theme/playground.js` is the browser half |
| `scripts/check-playground.mjs` | The Run-button bookkeeping: every complete app class either gets a button from `playground.mjs` or carries a `<!-- playground: no Run button — … -->` marker above its fence saying why it cannot run; a stale marker fails as loudly as a missing one. `--list` prints the deliberate exclusions with both reasons |
| `scripts/check-conventions.mjs` | The two house conventions the sample corpora gate and this one did not: the view-chain layout in every fenced chain (the linter's `chain-house-layout`, which is opt-in — `check-examples.mjs` writes its config without a `rules` block, so the rule was never emitted), and the three class section blocks in every fenced app class. `--fix` (`npm run fmt:chains`) reformats a drifted chain; the sections are a judgement and stay by hand |
| `scripts/lib/catalogue.mjs` | Parses and counts a sample catalogue, for `link-samples.mjs` and for the figures `generate-llms.mjs` writes into `llms.txt` — from a sibling checkout when one is here, else from the `catalogue.json` each sample repository commits at its root; pinned by `test/catalogue.test.mjs`, because it has stopped matching twice and both times answered wrongly instead of failing |

## Build & verify — run before every commit

```bash
npm run check          # test + check:version + docs:build + check:examples + check:conventions + check:playground + check:api-names + check:api-reference + check:samples
```

A documentation repository has no compiler for its prose, but nine things in
it are decidable, and all nine are decided before a merge:

| | |
|---|---|
| `test` | the sample-catalogue parser in `scripts/lib/`, against a row of every shape the three sample repositories generate |
| `check:version` | the release number in the nav bar, the deprecations page and the changelog, against the newest release tag of the framework — this one goes stale without anybody touching this repository |
| `docs:build` | a page that does not build is a page nobody can read |
| `check:examples` | the ABAP in the fenced blocks, against the real framework: does it compile, and does the view it builds name controls and properties that exist on the UI5 floor this documentation targets |
| `check:api-names` | every `client->` name on the site — method, parameter, `cs_*` constant — against `z2ui5_if_client` on `main`, plus every `blob/main/` link into the framework's tree. `check:examples` compiles the fenced blocks that are whole CLASSES; this is the rest of the page: the sentence, the two-line snippet, the constant block a page reproduces, the source link. Four pages taught API that 1.143.0 had deleted and nothing was red |
| `check:api-reference` | the committed client API reference — the generated block in `resources/api.md` and `docs/public/api/client-api.json` — regenerated from `z2ui5_if_client` on `main` and compared byte for byte. Goes stale whenever the interface changes over there and the committed reference still describes the shape before it. `npm run generate:api` rewrites both |
| `check:conventions` | the fenced ABAP against the house style the reader meets next: the view-chain layout, and the three section blocks of an app class. `check:examples` asks whether an example compiles and names real API — both questions about the framework; neither can see that a snippet is written in a different style from every sample. Measured against [samples-controls](https://github.com/abap2UI5/samples-controls) (637 classes, gated, at zero): five chains here showed the reader a different tree than the one that renders, and 57 of 86 app classes carried neither `PROTECTED SECTION.` nor `PRIVATE SECTION.`. What this gate deliberately does NOT take over is the blank-line and `t_arg` continuation rules — those are pattern-lint *warnings* over there and that corpus carries 382 of them |
| `check:playground` | every complete app class on the site either carries a **Run** button or a marker on its page saying why it cannot run. The rules that offer the button fail towards *not* offering one, so without this an example nobody ever measured is indistinguishable from an example that can never run — which is exactly how the coverage ledger below went stale. What stays undecidable by CI — does a *buttoned* example actually start — is the measurement the Run-button section describes |
| `check:samples` | the **Working Samples** blocks, against [abap2UI5/samples](https://github.com/abap2UI5/samples) |

`.github/workflows/check.yml` runs the same nine, in the same order. Keep the
two in step: a step that exists only in `package.json` is a step no pull
request has to pass, which is how `npm test` — the pin added *because* the
catalogue parser broke twice in silence — went a release without CI.

`check:samples` needs an `abap2UI5/samples` checkout — set `SAMPLES_HOME`, or
clone it as a sibling. Without one it *skips* rather than fails, so verify the
output says what you think it says. CI checks out `abap2UI5/samples@main`
explicitly for this reason.

There used to be one more, `check:counts`, holding four figures on a
`resources/samples.md` page against the catalogues themselves. That page is
gone — the home page opens [the samples page](https://abap2ui5.github.io/samples/)
directly and each catalogue introduces itself — and with it the only prose
copy of a number this repository does not own. `generate-llms.mjs` still counts
the sample catalogues into `llms.txt`, which is why CI sparse-checks out
`SAMPLES.md` from `samples-controls` and `samples-stack`; both are
`continue-on-error`, because an unreachable repository must cost a figure and
not the run, and a generated line can simply leave the number out.

A count comes down a chain, first answer wins, all of it in
`scripts/lib/catalogue.mjs` and pinned by `test/catalogue.test.mjs`: a sibling
checkout's `catalogue.json` (the machine-readable catalogue each sample
repository commits at its root), then the checkout's `SAMPLES.md` through the
same parser the sample links go through, then the `catalogue.json` the
repository publishes on its default branch, fetched — then no number. The two
`catalogue.json` steps read the same file, so a build with a checkout and a
build without one publish the same figure; and every step **counts entries**
rather than repeating a `counts` field, so no path can hand `llms.txt` a claim
instead of a count. The fetch is allowed to fail — 404 before the file is
committed over there, timeout, no network — and every failure costs the
figure, never the build.

## What the site publishes for machines

`docs:build` runs `scripts/generate-llms.mjs` first, which writes three things
into `docs/public/` — generated on every build and **gitignored**, because they
are a projection of the pages next to them:

| | |
|---|---|
| [`/docs/llms.txt`](https://abap2ui5.github.io/docs/llms.txt) | the map: every page with its title and one line of what it covers, plus the repositories around it |
| [`/docs/llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) | the whole documentation as one markdown document |
| `/docs/<page>.md` | each page as raw markdown, next to its `.html` |

One more file is published for machines and — unlike the three above —
**committed**: [`/docs/api/client-api.json`](https://abap2ui5.github.io/docs/api/client-api.json),
the client API as one JSON document. It is not a projection of the pages next
to it but a claim about the framework at a given ref, which is the
samples-block case, not the llms.txt case — so it is generated by
`npm run generate:api` and held fresh by `check:api-reference`.

This is for the reader nothing else reaches: an agent that is simply *asked*
about abap2UI5, with no MCP server and no checkout. Without it, it falls back
on training data — where abap2UI5 still looks like `z2ui5_cl_xml_view`.

Nothing needs maintaining. Adding a page to the sidebar adds it here.

The reader with a checkout and no build gets the inverse problem: the three
files are gitignored, so a clone contains none of them. `llms.txt` at the
repository ROOT is the answer — a committed pointer naming the published URLs,
maintained by hand because it names URLs and nothing else. Do not "fix" it by
committing the generated files instead: they would be stale on every commit
that touches a page, and a wrong committed copy outranks a right generated one
in every tool that reads the tree.

## Things that will trip you up

- **The API gates judge the site against `main`, not against a release.**
  `check:api-names`, `check:api-reference` and `check:examples` all resolve
  `frameworkRef( )` in `scripts/lib/release.mjs`, which is `main`. This
  repository used to pin them to the release it names, and the reasoning was
  sound in isolation — a reader installs a release, main is ahead of it — but
  it coupled every documentation merge to the framework's monthly tag. When
  the `hash_*` / `app_state_*` API landed on main these pages could not be
  corrected to it: the gate rejected the new names, so the site kept teaching
  the old spellings while the samples it links to had already migrated. The
  rest of the organisation had cut the same coupling already (abap2UI5's
  `.github/shared/check-framework-pin.mjs`, "releases never gate a merge").
  What a reader on the newest release does not have yet is now a question of
  PROSE: `resources/deprecations.md` carries a *next release* column, and
  `check:version` keeps the release number in the nav bar, the deprecations
  page and the changelog honest. `A2UI5_REF` still overrides the ref — now to
  pin a run BACK to a release rather than forward to main.

- **The nav bar and the sidebar contain the same two entries.**
  `Contribution` and `Sponsor` appear in both `themeConfig.nav` and
  `themeConfig.sidebar` in `config.mjs`. The four lines now carry a `// nav` or
  `// sidebar` marker so each one is unique — match on the marker, not on the
  link. Any further line that has to exist twice gets the same treatment;
  a replace-first edit on a text that appears twice hits the wrong one and
  looks like it worked.
- **A fenced ABAP example is code, and it is checked.** `check:examples`
  compiles it and lints the view. It also refuses `z2ui5_cl_xml_view=>` — the
  frozen builder — unless the page carries the migration banner, and refuses a
  chain step calling anything but `ele` / `tag` / `a` / `end` / `stringify`,
  whether it is written mid-chain (`)->input( )`) or on its receiver
  (`page->input( )`). Examples are the most-copied ABAP in the project; that
  gate is the reason the pages could be migrated at all.
- **…and it is written in the same house style as the samples.** The layout of
  a view chain is not taste — the indentation is the only picture the reader
  has of the tree, and `check:conventions` holds it to the rules in the
  organisation's `view-chain-layout` guide: one call per line including
  attributes, four spaces per level, `end( )` in the column of the `ele( )` it
  closes. `npm run fmt:chains` reformats a drifted chain (whitespace-only, and
  verified as such — a layout fix can never change what the view builds), so
  do not re-indent one by hand. The same gate holds every app class to all
  three section blocks. What is NOT gated here, on purpose: the blank-line
  rules around `end( )` and the `t_arg` continuation column, which are
  warnings in the sample corpora and not clean there either.
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
  `check:samples` fails if a rewrite would change anything. The client API
  reference on `resources/api.md` works the same way: everything between its
  markers, plus `docs/public/api/client-api.json` next to it, comes from
  `npm run generate:api` — edit the intro around the block by hand, never the
  block, and regenerate after a release bump or `check:api-reference` fails.

## The Run button, and why its rules are hand-maintained

An example that the [playground](https://github.com/abap2UI5/playground) can
start carries a Run button; pressing it mounts the running app under the code.
The code travels in the playground's URL fragment, read out of the rendered
block at click time — so nothing is hosted here, and the example that runs is
the text on the page rather than a copy of it.

**Whether an example runs is the one question CI cannot answer** — only a
playground can, and a playground is a three-minute build of another repository.
So the rules in `docs/.vitepress/playground.mjs` are an approximation, they
fail towards *no button*, and every one of them came from an example watched
failing in a real one:

| | |
| --- | --- |
| a complete class implementing `z2ui5_if_app` | the playground compiles a whole abapGit object, and the framework starts an app |
| a name of at most 30 characters | not a playground limit — a longer class exists nowhere. Two were printed here for years, invisible to `check:examples`, which renames every example to `zcl_docs_example_NN` before compiling it |
| displays something | `configuration/authorization.md` starts, shows an empty frame and demonstrates nothing |
| no `SELECT` outside the tables the page has | the database in the page holds the framework's tables and what open-abap ships; T100 is there, VBAK is not |
| no EML, CDS or HANA SQL | same list `check-examples.mjs` already skips |
| no add-on, no on-premise SAP class, no function module | `z2ui5_if_lp_kpi`, `cl_bcs_message`, `cl_demo_output`, `describe_by_name` |
| no method declared and never implemented, no local class | neither compiles as printed, here or in a system |

`test/playground.test.mjs` pins one fixture per line of that table, **plus the
shapes a rule written one word wider would have swallowed**: a `SELECT` in a
comment, the word FROM inside a string, `INSERT VALUE #( )` into an internal
table.

**The bookkeeping half of the question, however, is decidable, and
`check:playground` decides it.** An example the rules refuse carries a marker
on its page, directly above the fence:

```md
<!-- playground: no Run button — SELECTs from VBAK, which no browser database has -->
```

A complete app class with neither a button nor a marker fails the gate: either
it can run — then measure it (below) and let it have its button — or it cannot,
and the marker records why in the page's own words, next to the code it is
about. A marker above an example that *has* a button fails just as loudly, so
intent cannot outlive a fix. `npm run check:playground -- --list` prints every
deliberate exclusion with both reasons — the engine's and the page's — which is
also the worklist for the next measurement. What the gate cannot decide, and
says so, is whether a buttoned example actually starts; that stays a
measurement.

**To redo the measurement** — after adding examples, or after the playground
changes — build the playground, serve it, and open each fenced example in an
embedded one, checking that the status line reaches `running` and that the app
frame contains something:

```sh
git clone https://github.com/abap2UI5/playground && cd playground
npm ci && npm run build && npm run serve      # the first build is a few minutes
# then, for each example: /?embed=1&view=app#<the deflated fragment>
```

The last full measurement: **61 complete app classes, 39 with a button, all 39
started and rendered.** The site has since grown, and the hand-kept copy of
those figures here went stale without anyone noticing — which is what
`check:playground` now exists to prevent. The bookkeeping today, printed by the
gate on every run: **68 complete app classes, 47 with a button, 21 excluded on
purpose**, every exclusion a marker on its page. The growth is examples whose
shapes the measured rules already covered, plus one page completed so its
example could run at all; a spot-check of six buttoned examples in a served
playground build — the newly buttoned life-cycle class driven through its whole
event roundtrip, the quickstart and About classes, tutorial Step 12, the
tables page, and the `SELECT FROM t100` example — started and rendered, every
one. The next full measurement opens all 47.

**The published playground is what readers get**, not the checkout you tested
against. A change to the rules here can ship on its own; a change that depends
on new playground behaviour has to wait for that deploy — and the button in its
first form does depend on one. It needs the loader that names the file after the
class in it (before that, `data-code` only worked for a class called
`zcl_playground`, and every button here would have shown the reader a name
error), the app-only layout fix for a column narrower than 820px, and
`frameOptions="allow"` on the app frame. **Merge and deploy
[abap2UI5/playground](https://github.com/abap2UI5/playground) first**, then this.

## Toolchain

Node 22, matching the rest of the organisation. Actions are pinned to commits
with the tag in a comment; `.github/dependabot.yml` moves both the npm packages
and the action pins monthly. The `@abap2ui5/linter` version decides what
`check:examples` catches, so a bump there is a content decision, not just a
dependency one — read what the new rules say before merging it.
