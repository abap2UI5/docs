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
| `scripts/generate-api-reference.mjs` | Generates the client API reference — the block in `docs/resources/api.md` and `docs/public/api/client-api.json` — from `z2ui5_if_client` at the pinned release; `--check` is the freshness gate |
| `scripts/lib/client-interface.mjs` | Where `z2ui5_if_client` is fetched from (the release pin, shared with `check-api-names.mjs`) and the full parser `generate-api-reference.mjs` renders from |
| `scripts/check-version.mjs` | The release number in the nav bar, the deprecations page and the changelog, against the newest release tag of the framework |
| `docs/.vitepress/playground.mjs` | Decides which fenced ABAP example gets a **Run** button, and wraps the fence; `theme/playground.js` is the browser half |
| `scripts/lib/catalogue.mjs` | Parses and counts a sample catalogue's rows, for `link-samples.mjs` and for the figures `generate-llms.mjs` writes into `llms.txt`; pinned by `test/catalogue.test.mjs`, because it has stopped matching twice and both times answered wrongly instead of failing |

## Build & verify — run before every commit

```bash
npm run check          # test + check:version + docs:build + check:examples + check:api-names + check:api-reference + check:samples
```

A documentation repository has no compiler for its prose, but seven things in
it are decidable, and all seven are decided before a merge:

| | |
|---|---|
| `test` | the sample-catalogue parser in `scripts/lib/`, against a row of every shape the three sample repositories generate |
| `check:version` | the release number in the nav bar, the deprecations page and the changelog, against the newest release tag of the framework — this one goes stale without anybody touching this repository |
| `docs:build` | a page that does not build is a page nobody can read |
| `check:examples` | the ABAP in the fenced blocks, against the real framework: does it compile, and does the view it builds name controls and properties that exist on the UI5 floor this documentation targets |
| `check:api-names` | every `client->` name on the site — method, parameter, `cs_*` constant — against `z2ui5_if_client` at the release this site names, plus every `blob/main/` link into the framework's tree. `check:examples` compiles the fenced blocks that are whole CLASSES; this is the rest of the page: the sentence, the two-line snippet, the constant block a page reproduces, the source link. Four pages taught API that 1.143.0 had deleted and nothing was red |
| `check:api-reference` | the committed client API reference — the generated block in `resources/api.md` and `docs/public/api/client-api.json` — regenerated from `z2ui5_if_client` at the release this site names and compared byte for byte. Goes stale the same way `check:version` does: a release happens over there, and the committed reference still describes the one before it. `npm run generate:api` rewrites both |
| `check:samples` | the **Working Samples** blocks, against [abap2UI5/samples](https://github.com/abap2UI5/samples) |

`.github/workflows/check.yml` runs the same seven, in the same order. Keep the
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
the three catalogues into `llms.txt`, which is why CI sparse-checks out
`SAMPLES.md` from `samples-controls` and `samples-stack`; both are
`continue-on-error`, because an unreachable repository must cost a figure and
not the run, and a generated line can simply leave the number out.

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
to it but a claim about the framework at a pinned release, which is the
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

**This is the eighth decidable thing in this repository and the only one CI
cannot decide.** Whether an example runs is a question only a playground can
answer, and a playground is a three-minute build of another repository. So the
rules in `docs/.vitepress/playground.mjs` are an approximation, they fail
towards *no button*, and every one of them came from an example watched failing
in a real one:

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

**To redo the measurement** — after adding examples, or after the playground
changes — build the playground, serve it, and open each fenced example in an
embedded one, checking that the status line reaches `running` and that the app
frame contains something:

```sh
git clone https://github.com/abap2UI5/playground && cd playground
npm ci && npm run build && npm run serve      # the first build is a few minutes
# then, for each example: /?embed=1&view=app#<the deflated fragment>
```

The last measurement: **61 complete app classes, 39 with a button, all 39
started and rendered.** The home page has since dropped its example, and
tutorial Step 12 has since reprinted Step 10's measured class unchanged, so the
figure today is 61 and 39; the 22 without a button each have a reason the module
prints.

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
