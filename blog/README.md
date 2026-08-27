# abap2UI5 Know-How — article series

Drafts for a short series explaining abap2UI5 concepts, one idea per article.

**These files are drafts, not documentation.** They live at the repository
root, deliberately outside `docs/`, so they are not built as site pages, not
picked up by `generate-llms.mjs`, and their fenced ABAP blocks are not compiled
by `check:examples`. Nothing here is published by the site. If a draft later
earns a place in the documentation, it moves into `docs/` — and then it has to
pass the gates like every other page.

## Publishing plan

| Date | Article | State |
|---|---|---|
| Tue, 25 Aug 2026 | #1 Somewhere on the Way to UI5, We Lost RTTS | **posted** |
| tbd | #2 abap2UI5 is not a Programming Model | draft |
| tbd | #3 The Cost of a Screen | draft |

**#2 and #3 were one article until they were split.** They had to be: the
draft opened on the cost of a screen and then spent half its length explaining
the interface, which is a different argument with a different audience. Kept
apart, each is short and each is answerable on its own.

| | asks | evidence | ends on |
|---|---|---|---|
| **#2** Not a Programming Model | what does abap2UI5 ask of my architecture? | the `z2ui5_if_app` interface in full, and an EML call from an event handler | nothing — so it composes |
| **#3** The Cost of a Screen | why do small screens never get built? | a complete job monitor, and the list of steps it skips | the price scales down |

**The boundary is load-bearing, so hold it.** #3 must not re-explain the
contract and #2 must not argue the economics; each links the other once and
moves on. Both articles open with a box saying which question they answer,
because a reader arriving from the wrong one should find that out in a
sentence rather than a paragraph.

The shared temptation is "one class, no service" — which #2 states as a
*contract* ("that is all it asks") and #3 as a *price* ("that is all it
costs"). Same fact, two arguments; if either starts making the other's, they
have merged again.

## Format

**The article is the artefact.** Each piece is written to be read in full and
is not cut down to fit a feed. The LinkedIn post at the bottom of each file
exists only to introduce the article and ask one question — it is an
announcement, not a second version of the content.

**Short, though.** Not feed-short, but no longer than the idea needs: around
**600–900 words**, counting the article's prose only — not the code blocks and
not the teaser post, which is measured in characters and belongs under ~1,200.
Count those three separately or the number means nothing; conflating article
and teaser is how article 1 was twice reported at a length it did not have.
The failure mode in these drafts has been restating a point in a second, more
careful paragraph — the careful one usually replaces the first rather than
following it. Article 1 is at 887: it carries the design-time/runtime split, a
mechanism section and a coexistence close, and each of those earns its words.
Article 2 is at 910 and has not earned its extra 300 — it wants the pass.

Which means the usual reach mechanics are deliberately not applied here: no
hook engineered to a character count, no carousel, no "link in the first
comment", no posting-time optimisation. If an article is worth reading, the
introduction can simply say what it is about.

What the teaser post does need: enough substance that someone can decide
whether to click, a question that is specific enough to actually answer, and
the link. Three hashtags, not fifteen.

Language is English — the abap2UI5 community is international.

## The frame

The series exists to explain **a problem and how abap2UI5 addresses it**. It is
not a comparison, and the way to keep it from becoming one is not to soften the
comparison but to leave it out. Three rules, and they are load-bearing:

1. **Never compare on merit — but do state how things coexist.** The two are
   not the same sentence, and the difference is the whole series.
   *Comparison* is what to leave out: which approach is better, faster or more
   modern, and equally the even-handed version of it, because a paragraph
   defending RAP is still a paragraph about RAP and invites the argument the
   series exists to avoid. Where the incumbent approach appears only as the
   backdrop to a problem, name the *mechanism*, not the product: "the standard
   path goes through a typed service".
   *Coexistence* is the point and has to be explicit, because a reader does
   not infer it. Say plainly what runs next to what, what calls what, and what
   does not have to change — and there, name the thing: an abap2UI5 screen
   calls a RAP business object through EML, a freestyle UI5 team gets the same
   `sap.m` controls and no second frontend stack. That is a fact about
   installation and call direction, not a claim about merit.
   Article 1 names RAP and OData once each, both in its coexistence section.
2. **No authorial first person.** No "I think", no "the point I want to make",
   in the articles or the teaser posts. An argument that stands on its own does
   not need an author vouching for it, and the same text then works as
   documentation later.
   The collective "we" is a different thing and is allowed where it means the
   ABAP community rather than the writer — "somewhere on the way to UI5, we
   lost RTTI" is a shared observation, and rewriting it into the passive would
   cost the sentence its point.
3. **State the limits in the article.** Article 1 says abap2UI5 has no data
   model, no transactional buffer and no generated UI; article 2 says a generic
   RTTI table has no contract and spells out what that costs. Naming the cost
   is what makes the rest credible, and it does the work a comparison would
   have done without mentioning anyone.

4. **Write for the old hands.** The reader has built field catalogs by hand,
   knows what `REUSE_ALV_GRID_DISPLAY` cost, and has a `Z` package full of
   tools nobody has touched since. Use that vocabulary and skip the
   explanations they do not need — the recognition is what earns the rest of
   the argument. It also decides what an example may assume: article 1's data
   browser needs no introduction to what SE16N is for.

**Free and MIT** gets mentioned once, in passing, never as a pitch.

### On titles

"The Cost of a Screen" was called *"abap2UI5 is not a Programming Model"* while
it was being written, and the body outgrew it. Negation implies a thing being negated even
when nothing is named, so the title kept inviting the comparison the text had
stopped making. *"The Cost of a Screen"* names the problem instead, which is
what the article is actually about — and it let the phrase "programming model"
disappear from the piece entirely.

Worth applying to the rest of the roadmap: title the problem, not the contrast.

## Articles

| # | File | Title | Status |
|---|---|---|---|
| 1 | [`01-somewhere-on-the-way-to-ui5.md`](01-somewhere-on-the-way-to-ui5.md) | Somewhere on the Way to UI5, We Lost RTTI | ready |
| 2 | [`02-the-cost-of-a-screen.md`](02-the-cost-of-a-screen.md) | The Cost of a Screen | draft |

Each file contains the full article, then the teaser post under a heading at
the bottom.

## The ABAP in a draft

Two rules, both learned the hard way in this repository.

**Check it before it ships.** A published snippet is the most-copied ABAP the
project produces, and a wrong one is uncorrectable once it is on LinkedIn. Run
the same two gates `check:examples` runs — the class in article 1 passes both:

```sh
# does it compile against the real framework and the released API mirror
npx --yes @abaplint/cli abaplint.json     # deps: abap2UI5/abap2UI5 + abapedia/steampunk-2305-api
# does the view name controls and properties that exist on UI5 1.71,
# and is the chain laid out the way the house style says
npx --yes @abap2ui5/linter --config <cfg> # rules: { "chain-house-layout": true }
```

**Never teach a frozen class.** `src/99/` is legacy that ships only so existing
installations keep compiling — `z2ui5_cl_util*` and the built-in
`z2ui5_cl_pop_*` popups are in it, and `z2ui5_cl_xml_view` is the reason this
repository has a `check:examples` gate at all. Both drafts pointed at `src/99`
before this was caught: an app uses plain `cl_abap_*` RTTI, `z2ui5_cl_ui5_view_builder`,
and the addons for the rest. When a snippet wants a ready-made tool, link the
[addon](https://github.com/abap2UI5-addons) rather than a frozen built-in.

## Code that appears in an article

A snippet printed in an article is a fragment, and neither gate can check a
fragment. **So the complete class belongs in `abap2UI5/samples`, not in
`assets/`** — a sample is compiled, linted and rendered on every commit there,
while a copy here is a copy somebody has to keep in step, which is the failure
this whole repository already has a `check:examples` gate for.

Article 1 cuts its snippet from
[sample 497](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_497.clas.abap)
and links it. `assets/zcl_rtti_table_view.clas.abap` was that class living here
first; it is gone now that the sample is merged.

While drafting, keep the class in a scratch directory and run the two gates the
samples repository runs — `npm run lint` / `check:cloud` / `check:abap2ui5`
there — then open the sample PR and link it from the article.

## Images

`assets/make-header.py` builds the header image for article 1 and prints where
it wrote it. Run it from the repository root; it needs `pillow` and the
Chromium that ships with this environment.

The three mascots are the project's own brand assets, embedded from
`docs/public/mascots/` and only scaled — `docs/resources/logo.md` is explicit
that brand art is scaled and nothing else, and a hand-traced dinosaur would be
both off-brand and worse than the original. The ALV grid and the UI5 table are
drawn in the script, because the whole point of the picture is that they are
the same data behind two different faces.

Only the PNG is committed. The intermediate SVG is not: it carries the three
mascots as ~480 KB of inline base64, so it is a build artefact of the script
rather than an editable source. **The script is the editable source** — change
the layout there and re-run.

## Roadmap

Rough arc: **1–2 positioning**, **3–5 mechanics**, **6–8 practice and
enterprise**, **9–12 depth and outlook**.

| # | Working title | Core idea |
|---|---|---|
| 3 | The Roundtrip — how a stateful ABAP SPA works | GET loads the shell once, everything after it is POST/JSON; a draft table instead of a session. The diagram in the framework's `AGENTS.md` carries the whole article |
| 4 | Zero Deployment | No BSP per app, no frontend transport. Activate the class, call `?app_start=zcl_my_app` |
| 5 | PUBLIC means persisted | Why `PUBLIC SECTION` is serialized state that travels every roundtrip — the convention people follow without knowing why |
| 6 | "No JavaScript" — what that really means | Honest about the boundary: standard UI5 fully from ABAP, custom controls are JS |
| 7 | One codebase, 7.02 to ABAP Cloud | The downporting pipeline. Highly relevant to everyone still on an older stack, and under-covered in the community |
| 8 | abap2UI5 in the Fiori Launchpad | The hash split and the stripped `value` envelope — the two things that actually bite |
| 9 | Two-way binding without OData | `_bind( )`, model deltas, why the data is already current in the event handler |
| 10 | What CI looks like in an open source ABAP project | abaplint, the gates, generated artefacts. Aimed at developers who assume ABAP cannot have this |
| 11 | Building UI5 with an AI agent, without an SAP system | MCP server, headless render, screenshot. Keep it concrete so it does not read as AI hype |
| 12 | From an SE80 report to UI5 in an afternoon | One concrete ALV migration. A good closing article, and the natural sequel to #1 |

## Where the articles are published

Undecided. A LinkedIn article keeps everything in one place but is hard to link
to later and impossible to correct cleanly. A page under `docs/` — or a post on
a blog of your own — survives, can be fixed, and can be linked from the
repository; the LinkedIn post then introduces that instead. If the articles end
up in `docs/`, remember that the ABAP in them becomes gated code: `check:examples`
compiles complete view-building classes, so the fragments here would need to
grow into full classes or stay fragments on purpose.
