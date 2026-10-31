# AGENTS.md

The abap2UI5 documentation site, built by VitePress from this repository
alone and published as it builds (*A site of its own*, below). Every other repository in the organisation has one
of these; this one did not, which is part of why it drifted out of the
toolchain bump that reached all the others.

**This site is written for people.** Instructions aimed at AI agents were
deliberately taken out of the pages — an agent reads `llms.txt` (below), a
person reads the page. Do not put "as an AI, …" prose back into `docs/`.

## Layout

| Path | Purpose |
| --- | --- |
| `docs/` | The pages. Sidebar and nav live in `docs/.vitepress/config.mjs`. One page is not a chapter: `index.md` (`layout: home`, the front door). **The manual is for developers.** The case for the person who approves abap2UI5 - what it can build, how it compares, what it costs, who runs it - is a site of its own, [abap2UI5/why](https://github.com/abap2UI5/why) at https://abap2ui5.github.io/why/; link it, never argue it here a second time |
| `docs/public/` | Static assets — **and** the generated `llms.txt`, `llms-full.txt` and per-page `.md`, which are gitignored |
| `scripts/check-examples.mjs` | Extracts every fenced ABAP block that builds a view, compiles it against the real framework and lints the view it produces |
| `scripts/link-samples.mjs` | Generates the *Working Samples* block on a page from its `samples:` frontmatter plus the catalogue of the repository each class belongs to — the prefix decides: `z2ui5_cl_smp_app_*` is `abap2UI5/samples` (read from its `SAMPLES.md` in a checkout), `z2ui5_cl_smpc_*` samples-controls and `z2ui5_cl_smps_*` samples-stack (read from their `catalogue.json`, a checkout or the published copy). Checks the link in both directions for the samples repository (the `" @docs` line), the class's existence for the other two, and resolves the source links a page writes by hand against whichever checkout is here |
| `scripts/generate-llms.mjs` | Builds `llms.txt` / `llms-full.txt` / per-page markdown from the sidebar. Runs inside `docs:build`, so the deploy publishes them |
| `scripts/generate-api-reference.mjs` | Generates the client API reference — the block in `docs/resources/api.md` and `docs/public/api/client-api.json` — from `z2ui5_if_client` on the framework branch this site tracks (`main`); `--check` is the freshness gate |
| `scripts/lib/client-interface.mjs` | Where `z2ui5_if_client` is fetched from (the ref comes from `lib/release.mjs`, shared with `check-api-names.mjs`; `A2UI5_HOME` reads the same ref out of a local clone of the framework instead, with `git show`, never its working tree) and the full parser `generate-api-reference.mjs` renders from; pinned by `test/client-interface.test.mjs` |
| `scripts/check-version.mjs` | The release number in the bar's version dropdown, the deprecations page and the changelog, against the newest release tag of the framework |
| `scripts/generate-search.mjs` | Builds `docs/public/search-index.json` — the pages of this site plus every entry in the three sample catalogues. This site's own bar searches with VitePress's local search; the index is published for the playground's search box, which fetches it from here. `scripts/lib/search-engine.js` is the matcher the playground carries a copy of, pinned with the index by `test/search.test.mjs`. Runs inside `docs:build`, so the deploy publishes it; `scripts/lib/search-index.mjs` is what goes in |
| `scripts/lib/pages.mjs` | What a page of this site IS: the sidebar walk, its title, its opening sentences, its headings, its words. Shared by `generate-llms.mjs` and `generate-search.mjs` so a page added to the sidebar reaches both; `summarise( )` — the description of 150 pages and the note in `llms.txt`, which nobody writes and nobody proofreads — is pinned by `test/summarise.test.mjs` |
| `docs/.vitepress/playground.mjs` | Decides which fenced ABAP example gets a **Run** button, and wraps the fence; `theme/playground.js` is the browser half |
| `scripts/list-runnable.mjs` | The measurement's worklist: every fenced example that carries a Run button, out of the same `playground.mjs` that decides the button. `--json` adds each example's ABAP verbatim - what the button sends - so the measurement below can be driven rather than clicked |
| `scripts/check-playground.mjs` | The Run-button bookkeeping: every complete app class either gets a button from `playground.mjs` or carries a `<!-- playground: no Run button — … -->` marker above its fence saying why it cannot run; a stale marker fails as loudly as a missing one. `--list` prints the deliberate exclusions with both reasons |
| `scripts/check-vocabulary.mjs` | Every prose word against a dictionary and against `scripts/lib/vocabulary.txt` (grouped and commented, one word per line). `scripts/lib/vocabulary.mjs` is the tokenizer - what is a word here and what is an identifier - and is pinned by `test/vocabulary.test.mjs`, whose first case is the four typos that reached the published site |
| `scripts/check-line-length.mjs` | The wrapped-page rule and `--fix` (`npm run fix:line-length`), which rewraps a drifted paragraph. `scripts/lib/line-length.mjs` is what a prose line IS here and what a rewrap may touch; pinned by `test/line-length.test.mjs`, which holds the manual to the rule as its last case |
| `scripts/check-conventions.mjs` | The two house conventions the sample corpora gate and this one did not: the view-chain layout in every fenced chain (the linter's `chain-house-layout`, which is opt-in — `check-examples.mjs` writes its config without a `rules` block, so the rule was never emitted), and the three class section blocks in every fenced app class. `--fix` (`npm run fmt:chains`) reformats a drifted chain; the sections are a judgement and stay by hand |
| `scripts/lib/catalogue.mjs` | Parses and counts a sample catalogue, for `link-samples.mjs` and for the figures `generate-llms.mjs` writes into `llms.txt` — from a sibling checkout when one is here, else from the `catalogue.json` each sample repository commits at its root; pinned by `test/catalogue.test.mjs`, because it has stopped matching twice and both times answered wrongly instead of failing |
| `docs/.vitepress/theme/index.js` | **The default theme, as it comes.** The bar, the search (VitePress's local search), the sidebar, the outline and the home page are VitePress's own, so the manual reads like every other VitePress site a developer knows. Do not build a bar, a search box, a crumb trail or a menu of our own again: that is what this site had, 2,700 lines of CSS and five components arguing the theme's parts out of the way, and it read as a site nobody had seen before. The theme adds exactly one thing, the Run button (`playground.js`) |
| `docs/.vitepress/theme/style.css` | The family's red as the theme's brand colour (`--vp-c-brand-*`, CONVENTION.md in abap2UI5/abap2UI5.github.io), the self-hosted Inter, and the few classes the theme has no rule for: the Run button, the filename over a block, the scroll wrapper round a wide table, an inline brand mark. Nothing that restyles the theme's own parts |
| `scripts/check-images.mjs` | Every image under `docs/public`: a screenshot is WebP, a deliverable is one of the PNGs the logo page hands out, none is over its budget, and the build can measure each of them - `scripts/lib/images.mjs` is the reader |
| `scripts/lib/prose.mjs` | What on a page is PROSE - not fenced or inline code, not a link target, a tag, a comment, a URL, a generated block or a frontmatter setting - and the house spelling of it, which is American: the British forms as patterns, each with the American form it becomes. `test/spelling.test.mjs` is the gate; `scripts/fix-spelling.mjs` (`npm run fix:spelling`) rewrites exactly the words the gate names |
| `scripts/check-cross-site.mjs` | Every link out of this deployment and into a neighbouring one on the same origin carries a `target`, or VitePress's router swallows it and shows this site's 404 at the other site's URL. Reads the BUILT html, so it runs after `docs:build`; the rule and the reasoning are in `scripts/lib/cross-site.mjs` |
| `scripts/generate-controls-reference.mjs` | Generates the custom-control reference — the block in `docs/resources/controls.md` — from `app/webapp/cc/*.js` on the framework branch this site tracks; `scripts/lib/controls.mjs` runs each module in an empty vm and records what it hands `extend( )`; `--check` is the freshness gate |
| `scripts/check-audience.mjs` | Every prose line against the phrases of the decision-maker site (`scripts/lib/audience.mjs`): the manual is for developers and links https://abap2ui5.github.io/why/ instead of arguing it |
| `scripts/lib/lifecycle.mjs` | The third half of `check-conventions.mjs`: in a runnable app class, `view_display( )` only under `check_on_init( )` / `check_on_navigated( )`; the `<!-- lifecycle: allow unconditional display — … -->` marker is the exception, and a stale one fails |
| `scripts/lib/user-exit.mjs` | Reads `z2ui5_if_ui5_exit` and the defaults `z2ui5_cl_ui5_user_exit` assigns, for the generated block on `advanced/extensibility/user_exits.md` (written by `generate-api-reference.mjs`) |

## Build & verify — run before every commit

```bash
npm run check          # test + check:version + docs:build + check:cross-site + check:images + check:examples + check:conventions + check:line-length + check:vocabulary + check:audience + check:playground + check:api-names + check:api-reference + check:controls + check:samples
```

A documentation repository has no compiler for its prose, but fifteen things
in it are decidable, and all fifteen are decided before a merge:

| | |
|---|---|
| `test` | the sample-catalogue parser in `scripts/lib/`, against a row of every shape the three sample repositories generate; and **the sidebar against the pages** (`test/sidebar-titles.test.mjs`): every entry's label is its page's own H1 (a search result shows the H1, the menu the label, and fourteen pages had two names a reader could not connect), every section's link is either its own page or the first page in its list (Configuration opened Setup while Installation stood first), and every page under `docs/` has an entry |
| `check:version` | the release number in the bar's version dropdown (the nav item in `config.mjs` whose text is the number), the deprecations page and the changelog, against the newest release tag of the framework — this one goes stale without anybody touching this repository |
| `docs:build` | a page that does not build is a page nobody can read |
| `check:examples` | the ABAP in the fenced blocks, against the real framework: does it compile, and does the view it builds name controls and properties that exist on the UI5 floor this documentation targets. **It ran no abaplint rules at all until 2026-09-04** — the generated config had no `rules` block, so abaplint walked 139 files, ran nothing, and printed `0 issue(s) found` for years. Ten examples with an unbalanced parenthesis were sitting behind that. The three compile rules it runs now (`parser_error`, `check_syntax`, `unknown_types`) are the sample repositories' own; the reasoning for stopping there, measured against the full 188-rule set, is in the file. A fragment cannot be compiled, so every fence is also read for the two things decidable without a system: a call to a method of the frozen builder, and a `|…|` string template whose literal text runs over a line break (only a `{ … }` may) - nine binding strings were written that way and none compiled |
| `check:api-names` | every `client->` name on the site — method, parameter, `cs_*` constant — against `z2ui5_if_client` on `main`, plus every `blob/main/` link into the framework's tree, plus **no name from the frozen package** (`src/99`: `z2ui5_cl_util*`, `z2ui5_cl_pop_*`, `z2ui5_cl_xml_view*`, `z2ui5_if_exit`, `z2ui5_if_types`, …) anywhere but on the deprecations page and in the changelog. `check:examples` compiles the fenced blocks that are whole CLASSES; this is the rest of the page: the sentence, the two-line snippet, the constant block a page reproduces, the source link. Four pages taught API that 1.143.0 had deleted and nothing was red |
| `check:api-reference` | the committed client API reference — the generated block in `resources/api.md` and `docs/public/api/client-api.json` — regenerated from `z2ui5_if_client` on `main` and compared byte for byte. Goes stale whenever the interface changes over there and the committed reference still describes the shape before it. `npm run generate:api` rewrites both. The same run renders the user exit — `z2ui5_if_ui5_exit` plus the defaults `z2ui5_cl_ui5_user_exit` seeds, read out of the class, not retyped (`scripts/lib/user-exit.mjs`, pinned by `test/user-exit.test.mjs`) — into the block between the same `api:` markers on `advanced/extensibility/user_exits.md`, and `--check` holds that block too. Three artifacts, not two |
| `check:controls` | the committed custom-control reference — the generated block in `resources/controls.md` — regenerated from `app/webapp/cc/*.js` on `main` and compared byte for byte. The metadata (properties with type and default, events with parameters, aggregations) is read by RUNNING each module in an empty vm with stand-in dependencies (`scripts/lib/controls.mjs` says why a regex is the wrong tool: a default is sometimes an expression, and one control builds its definition first and hands it to `extend( )` by name). Each section links the pages of this site that use the control, found by walking them — so a cookbook page that starts using one changes the block, and the gate says to run `npm run generate:controls`. Lists the directory through the GitHub contents API (with `GITHUB_TOKEN` in CI) or `git ls-tree` with `A2UI5_HOME`; unreachable means SKIPPED, never a false OK. Pinned by `test/controls.test.mjs` |
| `check:audience` | every prose line of the manual against the phrases of the site for decision makers — `license fee`, `per-user fee`, `subscription`, `ROI`, `total cost`, `vendor lock`, `SLA`, `decision maker`, `IT management`, `business case` (whole words, any case, plural allowed; `scripts/lib/audience.mjs` is the list) — on every page but the four whose subject they are: `resources/license.md`, `resources/support.md`, `resources/sponsor.md` and `get_started/about.md` (the hand-off sentence). **The manual is for developers and links https://abap2ui5.github.io/why/**; both AGENTS.md files said so and nothing held it, and the business case had grown back on a dozen pages (two whole essays, the comparison tables of two more, the openings of Performance and Security). A page that argues the case is a second, unmaintained copy of that site. The prose is `scripts/lib/prose.mjs`'s, so a fence, a link target or a comment is never a hit; plain "per user" is not on the list because a work process per user is a technical fact. Floor: exit 1 when no page was walked. Pinned by `test/audience.test.mjs` |
| `check:conventions` | the fenced ABAP against the house style the reader meets next: the view-chain layout, and the three section blocks of an app class. `check:examples` asks whether an example compiles and names real API — both questions about the framework; neither can see that a snippet is written in a different style from every sample. Measured against [samples-controls](https://github.com/abap2UI5/samples-controls) (637 classes, gated, at zero): five chains here showed the reader a different tree than the one that renders, and 57 of 86 app classes carried neither `PROTECTED SECTION.` nor `PRIVATE SECTION.`. What this gate deliberately does NOT take over is the blank-line and `t_arg` continuation rules — those are pattern-lint *warnings* over there and that corpus carries 382 of them. And, third, the lifecycle: in every runnable app class (the Run button's own decision, so this and `check:playground` never disagree) a `client->view_display( )` in `main( )` must sit under an `IF`/`ELSEIF`/`WHEN` on `client->check_on_init( )` or `check_on_navigated( )` — a nested IF inherits the branch; `ELSE`, an event `WHEN` and a negated check (`= abap_false`, `NOT`) do not. Only `main( )` is read: the canonical shape calls a `view_display( )` method of the class from the branch, and following that call would be a compiler. Twenty cookbook classes sent their view on every roundtrip, the one thing Life Cycle, Common Failures and the tutorial tell the reader not to do, and nothing was red. A page that means the exception says so directly above the fence, `<!-- lifecycle: allow unconditional display — reason -->` (Step 2 of the tutorial, the first screen before events exist), and a marker whose example no longer needs it fails as loudly as a missing one. `scripts/lib/lifecycle.mjs`, pinned by `test/lifecycle.test.mjs` |
| `check:line-length` | **a page that is wrapped has to stay wrapped.** The manual is not written to one column and should not be: 39 pages are wrapped, 59 are one line per paragraph, 64 are in between, so a site-wide column would be a reformat of two thirds of the pages rather than a gate. The drift is the decidable part - a page whose prose already sits inside 80 characters may not acquire a line outside it. That is what an editor does to a paragraph it rewrites: it hands it back as ONE long line, invisible in the editor, and from then on every diff of that paragraph is one changed line instead of three, so each later correction reads as a rewrite. It happened twice in two days on the same page. Held: 66 pages; the other 97 are left alone and start being held the day somebody wraps them. Not prose, and the reason each one is not: fenced code, frontmatter, tables (they wrap at the cell), HTML, headings and image alt text (one line by construction), the caption under an image when it repeats that alt text word for word (the two are meant to be comparable at a glance), a line that is only long because of one unbreakable URL, and the generated `samples:` and `api:` blocks (wrapping one by hand is a change the next regeneration undoes). `npm run fix:line-length` rewraps a drifted paragraph - whitespace only, and verified as such: the words are identical and the rendered article is identical, which is the check that caught the two bugs in the rewrapper (it tore the punctuation off a code span, and it swallowed a `:::` container into the paragraph) |
| `check:vocabulary` | every prose word of the manual, against a dictionary (`dictionary-en`, the Hunspell en_US) and then against `scripts/lib/vocabulary.txt`, the 215 words this project uses that a dictionary does not carry. **`test/spelling.test.mjs` answers which SPELLING of a word; this one answers whether it is a word at all**, and nothing did: that test names British forms from a list, so a word that is neither spelling reads as prose it has no opinion about. `thant`, and then `havte`, `fullfll` and `requoirememnts`, went in through the browser editor, past all fourteen other gates and out to the published site in two days - and the second set stood in the FIRST SENTENCE of a page, which is where `scripts/lib/pages.mjs` takes the description from, so it read wrong in the search index, in `llms.txt` and under the card as well. The prose is `scripts/lib/prose.mjs`'s, the same reader the spelling test uses, so an ABAP keyword, a CSS property or a path is never a word here. Not a word either, each for a reason: a token carrying a digit or an underscore (`abap2UI5`, `z2ui5_if_app`), an acronym (`ICF`, `APIs`), camelCase (`liveChange`) - all identifiers, not English; a hyphenated compound is judged part by part, and emphasis inside a word (`**f**rontend`) is joined back up first. A word goes on the list when it is right and the dictionary simply does not have it; a word you are unsure about does not, because a list that absorbs doubt is how a gate like this stops catching anything. `--unused` names entries no page uses any more |
| `check:playground` | every complete app class on the site either carries a **Run** button or a marker on its page saying why it cannot run. The rules that offer the button fail towards *not* offering one, so without this an example nobody ever measured is indistinguishable from an example that can never run — which is exactly how the coverage ledger below went stale. What stays undecidable by CI — does a *buttoned* example actually start — is the measurement the Run-button section describes |
| `check:cross-site` | every link that leaves this deployment for a neighbouring one on the same origin — the playground, the catalogue, the linter's rule pages — carries a `target`. Without it VitePress's router treats the link as a route of THIS site, finds no page behind `/playground/` and renders the 404 *at that URL*, which reads as the other site being broken. Every way out of the manual was in that state at once: both bar items, the Linter rules row in the menu and the Run bar's link. Judges the built HTML, so it runs straight after `docs:build`. What it cannot see is the Run bar's link — built in a browser, in no built page — which is why `test/cross-site.test.mjs` pins that one as source |
| `check:images` | every image under `docs/public`, against the three things a page can afford and the one it cannot: a screenshot is WebP (the PNG captures were 200 to 335 kB each, 2.9 MB across the manual, on pages of 20 kB of text; the same captures as WebP are a fifth of that), a deliverable is one of the PNGs the logo page hands out, nothing is over its budget, and the build can measure every one - an image it cannot size gets no width and height and moves the page when it lands |
| `check:samples` | the **Working Samples** blocks and the source links a page writes by hand, against [abap2UI5/samples](https://github.com/abap2UI5/samples), [samples-controls](https://github.com/abap2UI5/samples-controls) and [samples-stack](https://github.com/abap2UI5/samples-stack) — a page may declare a class of any of the three |

**All eight walking gates carry a floor.** A gate that checked nothing reports
the same shape as a gate that found nothing wrong — which is precisely how
`check:examples` passed for years on an abaplint config with no rules in it. So
`check:examples`, `check:conventions` and `check:playground` each exit 1 when
their walk finds no example at all (`check:conventions` also when it judged no
runnable class for the lifecycle), `check:audience` when it walked no page, and say which of the fence language, the
page layout or the builder name is the likely cause - `check:conventions` when
EITHER of its halves finds nothing (no chain, or no app class), and also when
`chain-house-layout` stays silent on a canary chain built to break it, which is
what a linter bump that renamed the rule would look like. `check:line-length` exits 1
when no page of the site counts as wrapped, which would mean its fence handling
or its glob stopped matching rather than that the manual went loose, and
`check:vocabulary` when it walked no page or read an empty word list. `check:cross-site` carries
the same floor twice over: no HTML in `dist` at all, and no cross-site link on
a site whose bar carries three of them on every page.

The fifteen are written out in **three** places, and all three have to name the
same set: `package.json`'s `check` script, `.github/workflows/check.yml` for a
pull request, and `.github/workflows/deploy.yml` before the site is published.
`check.yml` runs them in the script's order; `deploy.yml` cannot, because it
has to build the site it publishes, so `docs:build` and `check:cross-site` —
which judges the built HTML — move to the end there. The SET is what has to
stay in step, and it is the thing that quietly does not: a step that exists
only in `package.json` is a step no pull request has to pass, which is how
`npm test` — the pin added *because* the catalogue parser broke twice in
silence — went a release without CI, and how `check:conventions` sat in
`check.yml` and not in `deploy.yml` from the day it was added, which is a gate
`main` could be published past.

`check:samples` needs an `abap2UI5/samples` checkout — set `SAMPLES_HOME`, or
clone it as a sibling. Without one it *skips* rather than fails, so verify the
output says what you think it says. CI checks out `abap2UI5/samples@main`
explicitly for this reason, and that checkout may not fail - not in
`check.yml` and not in `deploy.yml`, where a `continue-on-error` on it once let
a deploy publish sample links nothing had checked. The other two repositories are found the same way
(`SAMPLES_CONTROLS_HOME` / `SAMPLES_STACK_HOME`, or `.samples-controls` /
`../samples-controls` and the stack equivalents) and fall back to the
`catalogue.json` each publishes; CI checks both out with `catalogue.json` and
`src`, so a class a page declares from them is held to the tree, not only to
the catalogue.

There used to be one more, `check:counts`, holding four figures on a
`resources/samples.md` page against the catalogues themselves. That page is
gone — the cookbook links [the sample catalogue](https://abap2ui5.github.io/samples-site/),
which introduces itself and counts itself — and with it the only prose
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
| `/docs/<page>.md` | each page as raw markdown, next to its `.html` — with every root-relative link outside a fence made absolute (a page as its `.md` twin, an asset as itself), because `/cookbook/…` served under `/docs/` resolves to the origin root, which is another repository |

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

## A site of its own

This documentation is built and deployed from this repository alone:
`npm run docs:build` is VitePress, and its `docs/.vitepress/dist` is what
`deploy.yml` publishes at /docs/. Nothing is borrowed from another site at
build time, nothing is fetched from one at run time except the playground
frame a reader asks for, and nothing is shared in the browser's storage - the
theme choice and the search query are this site's own keys.

**The playground is a URL, not a dependency.** The Run button builds the
frame itself (`theme/embed.js`): `https://abap2ui5.github.io/playground/`
with `embed=1`, `view=app` for an app alone, and the code in the fragment -
one version character, then the base64url of the deflate-raw of a JSON array
of `{ name, source }` files. That contract is the playground's
(`src/shell/share.mjs` and `src/embed/abap2ui5-embed.js` over there); the
frame's height comes back as a `postMessage` from `abap2ui5-playground`.
Nothing else of the playground is known here.

**The neighbours are links.** Samples and Playground in the bar
(`theme/SiteNav.vue`) are plain links to the other sites. They share this
origin, so each carries a `target` - without it VitePress's router takes the
link over and draws this site's 404 at the other site's URL, which is what
`check:cross-site` holds.

**The look is the family's convention, copied.** The palette, Inter and the
bar's shape follow `CONVENTION.md` in
[abap2UI5/abap2UI5.github.io](https://github.com/abap2UI5/abap2UI5.github.io),
written into `theme/style.css` by hand. No gate compares them with another
repository: each site keeps its own copy, and a change to the convention is
made there first and then here.

## Addresses that have moved

**A URL this project published is a URL it keeps answering.** VitePress has no
redirect mechanism; what it does is copy
`docs/public/` into the build verbatim, so a moved page leaves a small
hand-written HTML file behind at its old address — a canonical link, a
`meta refresh`, `noindex`, and one sentence with a link for a reader whose
browser does not follow it. `docs/public/configuration/troubleshooting.html`
is the worked example.

There are seventy-nine of them. Eight are `/technical/*`: that whole section became
the Insights series under `/advanced/insights/` in one commit, and eight
addresses that had been public for years stopped existing with it — one of
them, `/technical/concept`, is linked from the framework's README, which is
the project's front page on GitHub. The ninth is the troubleshooting page,
renamed in 2023. The other nine are the pages folded into a neighbour on
2026-09-09, each pointing at the section that now carries it: `get_started/next`
(the end of Hello World), `get_started/tooling` (moved under `advanced/`, where
its sidebar entry already was), the three one-paragraph toolchain pages and
`advanced/local` (sections of `technical/tools/`), `configuration/btp_abap_env`
(S/4 Public Cloud, which says it covers both), `configuration/transport`
(Productive Usage) and `resources/contact` (Support).
The last thirteen are the Tutorial's, moved on 2026-10-08 when its single
"Walkthrough" group was dissolved: `tutorials/walkthrough/index` and
`step-1` to `step-12` each point at the same page one level up, under
`/tutorials/`. Those carry a one-line script on top of the `meta refresh`
that keeps the `#fragment`, because links into a step's sections exist.
The last five point out of this site, at the page for decision makers
(https://abap2ui5.github.io/why/), which took over what the manual
carried for them on 2026-10-24: `evaluate` and `get_started/evaluate` (its
address for its first day) keep the `#fragment`, because the framework's README
links sections of it and the new page keeps every section id;
`resources/cost_calculator`, `resources/who_uses` and `resources/references`
point at the section that carries each now.
The largest group, forty-four, is `/advanced/insights/`: the series left the
manual for a site of its own on 2026-10-31,
https://abap2ui5.github.io/insights/ (abap2UI5/insights), where every essay
kept its slug. One stub per essay and one for the index point at the same
essay there, carrying the `#fragment`; the five older stubs of the series
(three renumberings, and #35 and the first #37, which went to the page for
decision makers) and the `/technical/*` ones point at the new site directly,
so no old address takes two hops. The essays, their SVGs and their sidebar
group are gone from this repository; the sidebar keeps one row, `Insights`,
that leads there.

Two things make this safe rather than a second set of pages to maintain:

- **A stub is plain HTML in `docs/public/`**, copied as it is. VitePress's
  dead-link check reads the markdown pages, not these files, so a stub whose
  target is renamed later has to be found by hand: write the `<a href>` and
  the `meta refresh` with the same URL, and check both when a page moves.
- **The 404 is the net under the rest.** Not every renamed address gets a
  stub: the bar is "public, and linked from outside this organisation, where
  we cannot fix it". Everything else lands on `404.html`, which carries the
  whole page index and guesses from the words of the address — see the
  scoring note there, and in particular why a word is worth what it narrows
  down rather than one point per hit.

Delete a stub when the old URL has stopped receiving traffic, not before.

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
  `check:version` keeps the release number in the bar's version dropdown, the deprecations
  page and the changelog honest. `A2UI5_REF` still overrides the ref — now to
  pin a run BACK to a release rather than forward to main. `A2UI5_HOME` points
  the same three at a local clone of the framework: the ref is then read out
  of it with `git show` (so `A2UI5_HOME=../abap2UI5 npm run generate:api`
  regenerates the reference on a machine that cannot reach
  raw.githubusercontent.com, and `A2UI5_REF=my-branch` on top regenerates it
  against a branch that has not merged). It is deliberately not a
  sibling-checkout convenience like the sample catalogues have: the clone's
  working tree is never read, because the branch a neighbouring checkout
  happens to be on must not change a gate's verdict. Every run prints where
  the interface came from — read the line before trusting an OK.

- **A link to a neighbouring site needs `target="_self"`, and looks fine
  without it.** The playground, the catalogue and the linter's rule pages are
  on this origin, so VitePress's router takes an ordinary link to them over,
  finds no page of this site there and renders the 404 at their URL. Nothing
  is red: the markup is valid, the URL is right, and a reload lands on the
  real page — so it reads as the other site being down. Adding a row to the
  bar or to the menu behind its last button means adding the attribute;
  `npm run check:cross-site` (after a build) says so if you forget. A link to
  another host needs nothing.

- **`themeConfig.nav` is empty, and it stays empty.** The bar carries the four
  sections of the project (`theme/SiteNav.vue`), the search, and everything
  else behind the menu at its right-hand end. An entry added back to `nav`
  lands between the sections and the search box and the row stops reading left
  to right. What used to be there — Guide, Links, the version number — is
  written out in `config.mjs` where the array was, including where each one
  went. The `// nav` / `// sidebar` markers left in the sidebar are the scar of
  the duplication that dropdown cost: two of its entries stood twice in one
  file, and a replace-first edit hit the wrong one and looked like it worked.
- **The prose is American English, and a test holds it to that.** Behavior,
  color, license, catalog, optimize - because SAP writes "authorization",
  "Customizing" and "behavior definition", and a manual spelled the other way
  teaches a reader to search for words their system does not use. The pages
  were mixed for years; `test/spelling.test.mjs` now names every British form
  in the PROSE of every page, and `npm run fix:spelling` rewrites exactly
  those words. Prose only: fenced and inline code, link targets, tags,
  comments, URLs, the generated blocks and the frontmatter's settings are
  invisible to both (`scripts/lib/prose.mjs`), so `src/catalogue/` stays the
  path it is while "the sample catalog" is the words. A word that is `-ise`
  in American English too and is not yet on the list there - the test will
  name it - goes on the list, not in the page. This file and the code are not
  the manual, and stay as they are written.
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
- **What is obsolete is on `resources/deprecations.md` and nowhere else.**
  Every other page is written as if the superseded name had never existed: no
  "formerly called", no "the older spelling still compiles", no obsolete
  constant in a `cs_event` listing — a page that has to mention what went
  away links to the deprecations page instead. The generated client API
  reference follows the same rule mechanically: `generate-api-reference.mjs`
  drops every method whose ABAP-Doc opens with "obsolete", every parameter
  whose plain comment does and every constant under a label that does, from
  the page and from `client-api.json` alike. The interface marks them that
  way (its comments say so), so a new compatibility-only member over there
  needs its label, and a sentence in a live method's doc that names an old
  name is a documentation bug on that side. `check:api-names` exempts the
  deprecations page and the changelog for the same reason: they are the pages
  whose subject is the old names, and refuses every name from the framework's
  frozen package (`src/99`) on any other page — the util classes, the built-in
  popups, the predecessor view builder, the superseded exit and types
  interfaces still ship, so an example using one compiles and nothing else
  would notice. The changelog stays a dated record — a rename that happened
  is history there, not a mention to remove.
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
  `check:samples` fails if a rewrite would change anything. It also resolves
  every source link a page writes **by hand** — `github.com/abap2UI5/samples/blob/main/…`
  in the prose rather than in a generated block, and the same for the two
  sibling repositories — against that same checkout.
  The Insights essays used to be the main case of a hand-written sample link
  (they have their own site now, abap2UI5/insights); the samples repository
  renumbers, so such a link is one renumber away from a 404 with nothing
  watching it. Now something is, and it costs no request: CI clones
  `abap2UI5/samples` in full for the catalogue, so the answer is a file lookup. The client API
  reference on `resources/api.md` works the same way: everything between its
  markers, plus `docs/public/api/client-api.json` next to it, comes from
  `npm run generate:api` — edit the intro around the block by hand, never the
  block, and regenerate after a release bump or `check:api-reference` fails.
  The user-exit block on `advanced/extensibility/user_exits.md` comes from the
  same run, and the custom-control block on `resources/controls.md` from
  `npm run generate:controls` (`check:controls`); `scripts/lib/line-length.mjs`
  skips `samples:`, `api:` and `controls:` blocks alike.

## The Run button, and why its rules are hand-maintained

An example that the [playground](https://github.com/abap2UI5/playground) can
start carries a Run button; pressing it mounts the running app under the code.
The code travels in the playground's URL fragment, read out of the rendered
block at click time — so nothing is hosted here, and the example that runs is
the text on the page rather than a copy of it.

**The front door's example starts itself.** Everywhere else nothing is fetched
until a click — a chapter with seven Run buttons must not pull seven ABAP
runtimes. The one marked `edit` is watched by an `IntersectionObserver` and
mounts when it scrolls within 400px of the viewport, because "one ABAP class is
one UI5 app" is a claim while it is printed and a fact once it runs beside its
own source. A reader who never scrolls past the tiles still fetches nothing; one
who asks for less data (`prefers-reduced-data`, or `navigator.connection
.saveData`) is never sent it and keeps the button. Nor is a reader on a phone
(`pointer: coarse`, or a column under 860px): the start was measured at 33 MB
before compression, and `saveData` is a flag most phones never set. There the
button stays, and the 137-line listing folds to its first screen with a *Show
all* button under it (`site.js`, `foldOnPhone`) — it was 4,000 of the front
door's 6,900px on a 390px screen. **A self-start that fails is
silent** — the button comes back and no error is printed, because a message
about something nobody asked for reads as a broken front door when what is
broken is the network.

**One example is editable, and it is marked in the fence.** ```abap edit gives
the container `data-play="edit"`, and `theme/playground.js` then mounts the
playground WITHOUT `view=app` — the editor and the running app side by side —
and hides the printed listing while the frame is open, so the reader sees one
copy of the code and it is the one they can type in. Close puts the listing
back. Only the front door carries it: everywhere else the code is printed right
above the frame and an editor beside that would be the same text twice. The
flag never decides WHETHER there is a button, only what kind of frame it opens;
an example the playground would refuse gains nothing by asking to be edited.
`test/playground.test.mjs` pins all four cases.

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
| no `SELECT` outside the tables the page has, and no write to one | the database in the page holds the framework's tables and what open-abap ships; T100 is there, VBAK is not. A namespaced `/dmo/flight` and the table after a `JOIN` count, and so do `INSERT INTO`, `UPDATE`, `MODIFY` and `DELETE` |
| no EML, CDS or HANA SQL | the very list `check-examples.mjs` skips: one regex, `NEEDS_A_SYSTEM`, exported from `playground.mjs` |
| one object per fence | a playground file is one abapGit object: a test class (any name), a second class or an interface beside the app has nowhere to go |
| no add-on, no on-premise SAP class, no function module | `z2ui5_if_lp_kpi`, `cl_bcs_message`, `cl_demo_output`, `describe_by_name` |
| no method declared and never implemented, no local class | neither compiles as printed, here or in a system |

`test/playground.test.mjs` pins one fixture per line of that table, **plus the
shapes a rule written one word wider would have swallowed**: a `SELECT` in a
comment, the word FROM inside a string (an escaped `\|` in a template
included), `INSERT VALUE #( )` into an internal table, `DELETE` on one.

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
changes — take the worklist from `npm run runnable`, build the playground,
serve it, and open each example in it, checking that the status line reaches
`running` and that the app frame contains something:

```sh
npm run runnable -- --json > /tmp/runnable.json   # 64 examples, ABAP included
git clone https://github.com/abap2UI5/playground && cd playground
npm ci && npm run build                       # the first build is a few minutes
RUNNABLE_JSON=/tmp/runnable.json npm test -- docs-examples
```

The driving half lives THERE, in `tests/docs-examples.spec.js`, because that is
where the browser harness already is — a documentation site should not need
Playwright to run something once a quarter. It is not part of that repository's
`npm test`: with no worklist it contributes no tests. One example is one test,
named for its page and its class, so a failure names the page to open.

**Last measured 2026-09-07: 64 examples, 61 starting.** The three that did not
were `url_handling`'s `z2ui5_cl_sample_url` and both classes on
`navigation/app_state`, and they had one cause between them: all three teach the
`hash_*` / `app_state_*` API, which landed in abap2UI5#2693 on 2026-08-31, one
day after the commit the playground pinned. abaplint refused the method and the
run stopped before the app started — "1 error in the ABAP". Nothing here was
wrong: `check:api-names` resolves every `client->` name against abap2UI5 main,
where the method exists. It is exactly the case named above, the pin moved in
abap2UI5/playground, and the measurement is **64 of 64** on the pin that carries
it. Read a failure here in that order: this site first, then the pin.

The class in the example has to match the file it goes into: the playground
refuses the pair when they disagree, exactly as a system does. Renaming the
class to the open file's is enough — nothing in these examples depends on
its name.

The bookkeeping, printed by the gate on every run: **82 complete app classes,
63 with a button, 19 excluded on purpose**, every exclusion a marker on its
page.

**The last full measurement — 2026-09-04, all 63 opened in a served playground
build: 58 started and rendered.** The five that did not, and why, because that
is the half worth keeping:

| | |
| --- | --- |
| `expert_more/value_help.md` | started, and showed **nothing**: a `Page` with no title holding one `Input` with no label draws no text at all. Fixed here — the page has a title and the input a placeholder |
| `device_capabilities/geolocation.md` | same shape, worse: the `z2ui5:Geolocation` control is invisible by design, so the whole demo was an empty frame. Fixed here — the example now lists the values it reads, which is what the page is about |
| `browser_interaction/url_handling.md` | `client->hash_set( )` |
| `navigation/app_state.md` (both examples) | `client->app_state_set_active( )`, `client->app_state_get_href( )` |

Both fixes were re-measured the same way: `value_help` now puts "Value help" on
screen, `geolocation` "Device position / Latitude / Longitude / Altitude /
Accuracy" — so **60 of the 63 render**, and the three that do not are all one
thing.

The last three are **not** the documentation's: all three methods exist in
`z2ui5_if_client` on abap2UI5 `main`, which is what `check:examples` compiles
these pages against, and the playground pins an older framework commit
(`tools/fetch-deps.mjs`). The playground is right to refuse them and the pin is
what moves; the pages stay as they are.

The measurement before this one read *61 complete app classes, 39 with a
button, all 39 started* and had gone stale without anyone noticing — which is
what `check:playground` now exists to prevent for the bookkeeping half, and
what the dated table above is for on the half a gate cannot decide.

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
