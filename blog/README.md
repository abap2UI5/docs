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
| Tue, 25 Aug 2026 | #1 Somewhere on the Way to UI5, We Lost RTTI | ready, needs a URL |
| tbd | #2 The Cost of a Screen | draft |

**The one thing blocking Tuesday is the `[link]` in the teaser post** — see
*Where the articles are published* at the bottom. The post cannot go out
without a destination, and the destination decides whether the article can be
corrected afterwards.

The RTTI article leads because it opens on a loss the audience recognises
before it mentions abap2UI5 at all. "The Cost of a Screen" is the positioning
piece and reads better once a reader has seen the framework do something
concrete. Swap the numbers if that ordering turns out to be wrong — nothing
else depends on it.

## Format

**The article is the artefact.** Each piece is written to be read in full, at
whatever length the idea needs, and it is not cut down to fit a feed. The
LinkedIn post at the bottom of each file exists only to introduce the article
and ask one question — it is an announcement, not a second version of the
content.

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

1. **Do not name other frameworks.** Neither to criticise them nor to be fair
   to them — a paragraph defending RAP is still a paragraph about RAP, and it
   invites exactly the argument the series is trying not to have. Where the
   incumbent approach has to appear at all, describe the *mechanism* rather
   than the product: "the standard path goes through a typed service", not a
   brand name. Both current drafts name RAP zero times.
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
