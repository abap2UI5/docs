# abap2UI5 Know-How — LinkedIn series

Drafts for a short LinkedIn series explaining abap2UI5 concepts, one idea per
post.

**These files are drafts, not documentation.** They live at the repository
root, deliberately outside `docs/`, so they are not built as site pages, not
picked up by `generate-llms.mjs`, and their fenced ABAP blocks are not compiled
by `check:examples`. Nothing here is published by the site. If a draft later
earns a place in the documentation, it moves into `docs/` — and then it has to
pass the gates like every other page.

## The frame

The series exists to show that **abap2UI5 extends what you already run** — it
is not a competitor to RAP, and no post may read as one. Two rules for every
draft:

1. **No comparison, and say so in the post.** The line "this is not a
   comparison" belongs in the visible text, not in a footnote. Better still:
   praise RAP where it is genuinely the right answer, and show the two
   composing (post 1 does this with an EML call from an abap2UI5 event
   handler — the strongest argument available, because it is code rather than
   opinion).
2. **Free and MIT, mentioned once, in passing.** Never as a sales pitch.

Language is English: the abap2UI5 community is international and the reach in
the SAP audience on LinkedIn is far larger.

## Posts

| # | File | Title | Status |
|---|---|---|---|
| 1 | [`01-not-a-programming-model.md`](01-not-a-programming-model.md) | abap2UI5 is not a Programming Model | draft |
| 2 | [`02-abap2ui5-and-rtti.md`](02-abap2ui5-and-rtti.md) | Whatever Happened to RTTI? | draft |

## Roadmap

Rough arc: **1–2 positioning**, **3–5 mechanics**, **6–8 practice and
enterprise**, **9–12 depth and outlook**.

| # | Working title | Core idea |
|---|---|---|
| 3 | The Roundtrip — how a stateful ABAP SPA works | GET loads the shell once, everything after it is POST/JSON; a draft table instead of a session. The diagram in the framework's `AGENTS.md` carries the whole post |
| 4 | Zero Deployment | No BSP per app, no frontend transport. Activate the class, call `?app_start=zcl_my_app` |
| 5 | PUBLIC means persisted | Why `PUBLIC SECTION` is serialized state that travels every roundtrip — the convention people follow without knowing why |
| 6 | "No JavaScript" — what that really means | Honest about the boundary: standard UI5 fully from ABAP, custom controls are JS. Being straight about limits reads better than marketing |
| 7 | One codebase, 7.02 to ABAP Cloud | The downporting pipeline. Highly relevant to everyone still on an older stack, and under-covered in the community |
| 8 | abap2UI5 in the Fiori Launchpad | The hash split and the stripped `value` envelope — the two things that actually bite |
| 9 | Two-way binding without OData | `_bind( )`, model deltas, why the data is already current in the event handler |
| 10 | What CI looks like in an open source ABAP project | abaplint, the gates, generated artefacts. Aimed at developers who assume ABAP cannot have this |
| 11 | Building UI5 with an AI agent, without an SAP system | MCP server, headless render, screenshot. Highest reach potential — keep it concrete so it does not read as AI hype |
| 12 | From an SE80 report to UI5 in an afternoon | One concrete ALV migration. A good closing post |

## LinkedIn playbook

**Format**

- **Native post, not a LinkedIn article.** Articles get a fraction of the
  reach. Target 1,300–2,000 characters. The drafts here are longer than that on
  purpose — cut to hook + three paragraphs + one snippet, and link the rest.
- **Code as an image, never as text.** LinkedIn destroys indentation. Use a
  Carbon/Ray screenshot, or a **PDF carousel** of 3–6 pages. Carousels have the
  highest dwell time in the feed and suit a series: same look, number in the
  corner, instant recognition.
- **One idea per post.** Draft 2 currently carries two (the RTTI history and
  the generic view) — split it if it does not compress.

**Hook**

- Only ~200 characters are visible before "…see more". They have to carry the
  tension.
- What works: a claim with friction ("abap2UI5 is not a programming model"), a
  number ("the entire contract is one interface with one method"), a lost
  capability ("somewhere on the way to UI5, we lost RTTI").
- What does not: "In this post I would like to explain…".

**Reach**

- Tuesday–Thursday, 07:00–09:00 CET. Friday and the weekend are dead in this
  audience.
- External links suppress reach — put the GitHub link in the **first comment**
  and write "link in the comments".
- Three to five hashtags, not fifteen: `#ABAP #SAP #UI5 #Fiori #OpenSource`.
- Answer comments actively in the first 60–90 minutes; replies weigh more than
  likes.
- Close with a **specific** question — not "what do you think?" but "where do
  you still use RTTI-driven tooling today?".
- One post every one to two weeks, numbered, each one naming the next.

**Series mechanics**

- Two closing lines on every post:
  `abap2UI5 Know-How — #1 Not a Programming Model · #2 RTTI · #3 The Roundtrip (next)`.
  That is what turns separate posts into a series.
- After four or five posts, collect them on one page and link there instead of
  to individual posts.
