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
| `scripts/check-version.mjs` | The release number in the bar's menu, the deprecations page and the changelog, against the newest release tag of the framework |
| `scripts/generate-search.mjs` | Builds `docs/public/search-index.json` — the pages of this site plus every entry in the three sample catalogues, which is what the box in the middle of the bar searches. Runs inside `docs:build`, so the deploy publishes it; `scripts/lib/search-index.mjs` is what goes in |
| `scripts/lib/pages.mjs` | What a page of this site IS: the sidebar walk, its title, its first sentence, its headings, its words. Shared by `generate-llms.mjs` and `generate-search.mjs` so a page added to the sidebar reaches both |
| `docs/.vitepress/playground.mjs` | Decides which fenced ABAP example gets a **Run** button, and wraps the fence; `theme/playground.js` is the browser half |
| `scripts/list-runnable.mjs` | The measurement's worklist: every fenced example that carries a Run button, out of the same `playground.mjs` that decides the button. `--json` adds each example's ABAP verbatim - what the button sends - so the measurement below can be driven rather than clicked |
| `scripts/check-playground.mjs` | The Run-button bookkeeping: every complete app class either gets a button from `playground.mjs` or carries a `<!-- playground: no Run button — … -->` marker above its fence saying why it cannot run; a stale marker fails as loudly as a missing one. `--list` prints the deliberate exclusions with both reasons |
| `scripts/check-conventions.mjs` | The two house conventions the sample corpora gate and this one did not: the view-chain layout in every fenced chain (the linter's `chain-house-layout`, which is opt-in — `check-examples.mjs` writes its config without a `rules` block, so the rule was never emitted), and the three class section blocks in every fenced app class. `--fix` (`npm run fmt:chains`) reformats a drifted chain; the sections are a judgement and stay by hand |
| `scripts/lib/catalogue.mjs` | Parses and counts a sample catalogue, for `link-samples.mjs` and for the figures `generate-llms.mjs` writes into `llms.txt` — from a sibling checkout when one is here, else from the `catalogue.json` each sample repository commits at its root; pinned by `test/catalogue.test.mjs`, because it has stopped matching twice and both times answered wrongly instead of failing |
| `docs/.vitepress/theme/style.css` | Everything this site looks like. Its palette is the playground's, copied — see *One site in three places* below |
| `docs/.vitepress/theme/site-memory.js` | Where the reader was, on each site, so the bar comes back to it; pinned by `test/site-memory.test.mjs` |
| `docs/.vitepress/theme/TheBar.vue` | The bar, as one element this repository owns: the brand, `SiteNav.vue` (the four sections), `SearchBox.vue`, the two marks, `SiteMenu.vue` (the menu behind the last button, and the version number `check:version` reads). It used to be the theme's bar with our parts in its slots and 85 override lines arguing its own parts out of the way; the theme now keeps two things, the hamburger and the screen it opens on a phone |
| `docs/.vitepress/theme/SiteNav.vue` | The four sections, in the bar and again in the phone screen the theme's hamburger opens. Every item that leaves this deployment carries a `target` (`check:cross-site`), and three of the four are lifted to where the reader last was (`site-memory.js`) |
| `docs/.vitepress/theme/SiteMenu.vue` | The menu behind the bar's last button: the light/dark switch, the project's tools and its repositories, and the version number `check:version` reads (`VERSION`) |
| `docs/.vitepress/theme/SearchBox.vue` | The box in the middle of the bar and what it opens. `theme/search-engine.js` is the matching, framework-free because the other three bars carry a copy of it; both are pinned by `test/search.test.mjs` |
| `scripts/check-design.mjs` | The palette, the type and the radii the four bars share, against `abap2UI5/playground`'s copy of them — needs a checkout (`PLAYGROUND_HOME`, `.playground`, `../playground`) or the network, and fails rather than passing without one. The table of what is shared, and both spellings of each value, is `scripts/lib/design.mjs` |
| `scripts/check-cross-site.mjs` | Every link out of this deployment and into a neighbouring one on the same origin carries a `target`, or VitePress's router swallows it and shows this site's 404 at the other site's URL. Reads the BUILT html, so it runs after `docs:build`; the rule and the reasoning are in `scripts/lib/cross-site.mjs` |

## Build & verify — run before every commit

```bash
npm run check          # test + check:version + docs:build + check:cross-site + check:design + check:examples + check:conventions + check:playground + check:api-names + check:api-reference + check:samples
```

A documentation repository has no compiler for its prose, but eleven things in
it are decidable, and all eleven are decided before a merge:

| | |
|---|---|
| `test` | the sample-catalogue parser in `scripts/lib/`, against a row of every shape the three sample repositories generate — and the cross-site position memory, specifically which stored values `theme/site-memory.js` may follow |
| `check:version` | the release number in the bar's menu (`VERSION` in `theme/SiteMenu.vue` — it was a nav dropdown's label in `config.mjs` until the bar was rebuilt, and in `SiteBar.vue` until the bar was split), the deprecations page and the changelog, against the newest release tag of the framework — this one goes stale without anybody touching this repository |
| `docs:build` | a page that does not build is a page nobody can read |
| `check:examples` | the ABAP in the fenced blocks, against the real framework: does it compile, and does the view it builds name controls and properties that exist on the UI5 floor this documentation targets. **It ran no abaplint rules at all until 2026-09-04** — the generated config had no `rules` block, so abaplint walked 139 files, ran nothing, and printed `0 issue(s) found` for years. Ten examples with an unbalanced parenthesis were sitting behind that. The three compile rules it runs now (`parser_error`, `check_syntax`, `unknown_types`) are the sample repositories' own; the reasoning for stopping there, measured against the full 188-rule set, is in the file |
| `check:api-names` | every `client->` name on the site — method, parameter, `cs_*` constant — against `z2ui5_if_client` on `main`, plus every `blob/main/` link into the framework's tree, plus **no name from the frozen package** (`src/99`: `z2ui5_cl_util*`, `z2ui5_cl_pop_*`, `z2ui5_cl_xml_view*`, `z2ui5_if_exit`, `z2ui5_if_types`, …) anywhere but on the deprecations page and in the changelog. `check:examples` compiles the fenced blocks that are whole CLASSES; this is the rest of the page: the sentence, the two-line snippet, the constant block a page reproduces, the source link. Four pages taught API that 1.143.0 had deleted and nothing was red |
| `check:api-reference` | the committed client API reference — the generated block in `resources/api.md` and `docs/public/api/client-api.json` — regenerated from `z2ui5_if_client` on `main` and compared byte for byte. Goes stale whenever the interface changes over there and the committed reference still describes the shape before it. `npm run generate:api` rewrites both |
| `check:conventions` | the fenced ABAP against the house style the reader meets next: the view-chain layout, and the three section blocks of an app class. `check:examples` asks whether an example compiles and names real API — both questions about the framework; neither can see that a snippet is written in a different style from every sample. Measured against [samples-controls](https://github.com/abap2UI5/samples-controls) (637 classes, gated, at zero): five chains here showed the reader a different tree than the one that renders, and 57 of 86 app classes carried neither `PROTECTED SECTION.` nor `PRIVATE SECTION.`. What this gate deliberately does NOT take over is the blank-line and `t_arg` continuation rules — those are pattern-lint *warnings* over there and that corpus carries 382 of them |
| `check:playground` | every complete app class on the site either carries a **Run** button or a marker on its page saying why it cannot run. The rules that offer the button fail towards *not* offering one, so without this an example nobody ever measured is indistinguishable from an example that can never run — which is exactly how the coverage ledger below went stale. What stays undecidable by CI — does a *buttoned* example actually start — is the measurement the Run-button section describes |
| `check:cross-site` | every link that leaves this deployment for a neighbouring one on the same origin — the playground, the catalogue, the linter's rule pages — carries a `target`. Without it VitePress's router treats the link as a route of THIS site, finds no page behind `/playground/` and renders the 404 *at that URL*, which reads as the other site being broken. Every way out of the manual was in that state at once: both bar items, the Linter rules row in the menu and the Run bar's link. Judges the built HTML, so it runs straight after `docs:build`. What it cannot see is the Run bar's link — built in a browser, in no built page — which is why `test/cross-site.test.mjs` pins that one as source |
| `check:design` | the values the four bars are made of — the seven palette colours, the two type stacks, the two radii — against the copy [abap2UI5/playground](https://github.com/abap2UI5/playground) keeps, in **both** schemes. They are copied by hand on purpose (a stylesheet fetched across two deployments is a request in front of the first paint), and until this gate nothing compared the copies: they agreed because whoever touched one remembered the other. One had already drifted — two font stacks leading with different families, which is the same face on macOS and Windows and two different ones on Linux, so the same four words in the same bar measured 59/122/78/97px here and 65/141/87/110 there. It compares the EFFECTIVE value (a property the dark block does not redeclare keeps its light one), because the two sides switch schemes differently: `.dark` here, `[data-theme]` over there |
| `check:samples` | the **Working Samples** blocks, against [abap2UI5/samples](https://github.com/abap2UI5/samples) |

**All four walking gates carry a floor.** A gate that checked nothing reports
the same shape as a gate that found nothing wrong — which is precisely how
`check:examples` passed for years on an abaplint config with no rules in it. So
`check:examples`, `check:conventions` and `check:playground` each exit 1 when
their walk finds no example at all, and say which of the fence language, the
page layout or the builder name is the likely cause. `check:cross-site` carries
the same floor twice over: no HTML in `dist` at all, and no cross-site link on
a site whose bar carries three of them on every page.

The eleven are written out in **three** places, and all three have to name the
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
explicitly for this reason.

There used to be one more, `check:counts`, holding four figures on a
`resources/samples.md` page against the catalogues themselves. That page is
gone — the cookbook links [the samples page](https://abap2ui5.github.io/samples/)
and each catalogue introduces itself — and with it the only prose
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

## One site in three places

This documentation, the [playground](https://abap2ui5.github.io/playground/)
and the [sample catalogue](https://abap2ui5.github.io/playground/samples/) are
three deployments on **one origin**, and from 2026-09-04 they are meant to be
read as one site: the same bar, the same palette, the same measure. The other
two live in [abap2UI5/playground](https://github.com/abap2UI5/playground)
(`src/shell/`, `src/catalogue/` and the per-sample pages
`tools/sample-pages.mjs` writes).

**The palette is copied, not imported.** The seven values at the top of
`theme/style.css` are `src/catalogue/catalogue.css`'s, written out. Three
repositories deploy separately, and a stylesheet fetched across them would be a
request in front of the first paint. Change them together — the same applies to
the bar's markup, which exists four times by hand for the same reason, and to
the bar's **measure**: 20px from either edge at every width, 12px on a phone,
over the whole width and never centred, which is the catalogue's bar and which
`style.css` (*the measure*) holds the theme's two nav layouts to.

**The accent is SAP blue, and the mark is still red.** `#0a6ed1` / `#4aa3ff` is
what a link, a button and the hero name are set in; `#d03c4a` is the circle in
the wordmark and belongs to the mark alone. `resources/logo.md` is the page
that says so and the one place either value is quoted to a reader — if you move
an accent, move that table with it.

**What the bar carries, left to right.** The mark and the name, then the four
sections of the project — **Home**, **Documentation**, **Samples**,
**Playground** — then the search, then the two marks and the menu behind the
last button. One row at every width, which is the whole bar: the sections are
hard against the brand because that is where a reader's eye already is, and the
search sits on auto margins so it is centred in whatever the row has left
rather than at a number that is right on one screen.

Two of the four sections are pages of THIS deployment (Home is
`docs/index.md`, which the brand alone used to open and nothing named;
Documentation opens the manual) and two are the neighbouring sites. The one
you are on is marked, which for this site means Home *or* Documentation
depending on the page — `relativePath` decides it, server-side, so the bar is
right in the HTML rather than after hydration.

**The search is one box for the whole project.** It was VitePress's own local
search, which indexes the pages of this site and nothing else — and half of
what a reader wants is a sample, in another repository, on another deployment.
`theme/SearchBox.vue` searches one index instead
(`/docs/search-index.json`, built by `generate-search.mjs`): every page of the
manual with its headings and its words, and all ~770 entries of the three
sample catalogues with the titles, summaries and keywords those repositories
maintain. Results are grouped by area, documentation first, and a sample hit
opens that sample's own page in the catalogue.

The index is generated, gitignored and fetched lazily — nothing is loaded until
somebody types. The matching is `theme/search-engine.js`, deliberately
framework-free: the other three bars are static HTML and carry a copy of it,
the same arrangement as the palette and the position memory. **The index is not
copied.** It is one document on the shared origin, fetched by whichever site
the reader is on, because two copies of the data would be two answers to the
same query.

**One origin is also what breaks a plain link between them, and every link out
of this site carries `target="_self"` because of it.** This site is a single
page application: VitePress's router listens on the window and takes over any
link that is same-origin and looks like a page
(`origin === currentUrl.origin && treatAsHtml(pathname)`, in
`vitepress/dist/client/app/router.js`). `/playground/` passes both tests — so
the router pushed the URL, went looking for a page of THIS site to render
there, found none, and drew this site's own **404** in its place. The address
bar said `/playground/`, the document said PAGE NOT FOUND, and a reload then
loaded the real playground, which is what a failed SPA route change looks like
from the outside. Every way out of the manual was in that state at once: both
bar items, the *Linter rules* row in the menu, and the Run bar's *Switch to
Playground with this code*.

The router's own escape hatch is the line above those two tests — a link with a
`target` attribute is left alone, whatever the value. `_self` is the value,
because these three are one site and open in one tab. It is needed for
**neighbours on this origin only**: a link to github.com is another origin, the
router never looks at it, and VitePress gives external links in a page a
`target="_blank"` of their own — which is why only the hand-written bar and
menu ever got this wrong. `check:cross-site` now holds the whole built site to
it, and the direction back needs nothing: the other three documents are static
pages with no router in front of them.

**One origin means one localStorage**, which is what four things here rely on:

| | |
|---|---|
| the theme | The key is the playground's (`abap2ui5-playground:theme`). A head script in `config.mjs` reads it before the first paint and hands it to VitePress's own appearance handling; `SiteMenu.vue` writes it back when the button is pressed. A reader crossing from a dark playground gets a dark page, with no flash |
| where you were | `theme/site-memory.js`. Every page writes its own path down; the Samples item is lifted to whatever the catalogue last wrote. A stored value is **checked, not followed** — resolved against this origin and kept only if it is still inside the section the markup declares: the href it carries, or a wider `scope` the caller names for a link written deeper than what it restores (the other three bars point Documentation at the first page of the manual and still come back to wherever the reader was in it). A poisoned or stale key costs a restored position and nothing else. The cases are `test/site-memory.test.mjs` |
| where **on** the page you were | `theme/site-memory.js` again, keys `:scroll` (a small map of path → offset, the twelve most recent) and `:returning`. Restored **on arrival by the bar and nowhere else**: a bar item writes one record saying where it is sending the reader, and the page that *is* that, arriving within half a minute and with no hash of its own, honours it. Restoring on every load would fight the browser's own back-and-forward restoration and would drop a reader who followed an ordinary link into the middle of a page with nothing to explain it. A stored offset is checked the same way a stored path is — `scrollTo` takes whatever it is given. `test/scroll-memory.test.mjs` |
| the last thing you searched for | `theme/search-engine.js`, key `:search`. A hit opens another page, often another deployment, and the box that opens there was empty; the query is written down as a hit is opened and the next box starts with it, selected, so the first keystroke replaces it. Checked (a string, short, and less than half an hour old) rather than used. `test/search.test.mjs` |

The playground is consulted by both and remembered by neither: its URL carries
the code in the editor, so an item that reopened yesterday's sample would be a
different promise from the one the word makes.

**What a browser test in this repository cannot reach.** `vitepress preview`
serves this site on localhost while the catalogue link points at
abap2ui5.github.io, so every path through `lastVisited( )` takes the
different-origin branch and falls back. That is why the same-origin cases are a
unit test with a stubbed `location` rather than an end-to-end one. The round
trip itself is held over there, in `tests/site-memory.spec.js`.

**Why this is not one deployment, and why merging them would not have helped.**
The question comes up whenever the bar misbehaves: put the documentation, the
playground and the catalogue in one GitHub Pages site and the seams go away.
They are already on one origin — that is what a project page per repository
under `abap2ui5.github.io` gives you — so a merge would buy no origin that is
not already shared, and the 404 above was **not** a cross-origin problem: it
was a same-origin one, caused by exactly the sharing a merge would deepen. One
deployment would still serve the playground at a path VitePress does not own,
the router would still swallow the link, and the fix would still be the
attribute.

What it would cost is concrete: one repository publishing everything means the
documentation waits on the playground's build (a pinned framework, a transpile,
UI5, seven hundred catalogue pages) to publish a typo fix, one `pages` queue
for both, and the gates of two very different projects in one run. What it
would buy — one bar instead of four, one palette — is real, and is the price
deliberately paid above for a first paint that fetches nothing across a
deployment. If that trade is ever re-opened, re-open it for the four copies of
the bar, not for the links between the sites; those are one attribute.

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
  `check:version` keeps the release number in the bar's menu, the deprecations
  page and the changelog honest. `A2UI5_REF` still overrides the ref — now to
  pin a run BACK to a release rather than forward to main.

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
changes — take the worklist from `npm run runnable`, build the playground,
serve it, and open each example in it, checking that the status line reaches
`running` and that the app frame contains something:

```sh
npm run runnable -- --json > /tmp/runnable.json   # 63 examples, ABAP included
git clone https://github.com/abap2UI5/playground && cd playground
npm ci && npm run build                       # the first build is a few minutes
RUNNABLE_JSON=/tmp/runnable.json npm test -- docs-examples
```

The driving half lives THERE, in `tests/docs-examples.spec.js`, because that is
where the browser harness already is — a documentation site should not need
Playwright to run something once a quarter. It is not part of that repository's
`npm test`: with no worklist it contributes no tests. One example is one test,
named for its page and its class, so a failure names the page to open.

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
