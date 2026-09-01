# abap2UI5 Know-How — article series

Drafts for a short series explaining abap2UI5 concepts, one idea per article.

**These files are drafts, not documentation.** They live at the repository
root, deliberately outside `docs/`, so they are not built as site pages, not
picked up by `generate-llms.mjs`, and their fenced ABAP blocks are not compiled
by `check:examples`. Nothing here is published by the site. If a draft later
earns a place in the documentation, it moves into `docs/` — and then it has to
pass the gates like every other page.

## Publishing plan

| # | Article | State |
|---|---|---|
| 1 | Somewhere on the Way to UI5, We Lost RTTS | **posted** |
| 2 | abap2UI5 is not a Programming Model | draft, 402 words |
| 3 | The Cost of a Screen | draft, 330 words |
| 4 | The Roundtrip | idea |
| 5 | PUBLIC Means Persisted | idea |
| 6 | One Codebase, 7.02 to ABAP Cloud | idea |

**Every article answers one question**, and the arc is 2–3 positioning, 4–5
mechanics, 6 practice. Someone arriving at #4 first should not need #2.

| # | asks | evidence |
|---|---|---|
| **2** | what does it want from my architecture? | the `z2ui5_if_app` interface in full, then one app with three save handlers — EML, `MODIFY` on a table, a BAPI |
| **3** | why do small screens never get built? | a whole job monitor in one class |
| **4** | how does it actually work? | GET loads the shell once, then only POST/JSON; a draft table instead of a session |
| **5** | which convention will bite me? | why `PUBLIC SECTION` is state that travels every roundtrip |
| **6** | does it run on my release? | the downport pipeline |

### What the splitting exercise settled

#2 was split into a contract article and an EML article, then merged back. The
merge was right and the reason is worth keeping: **"the contract is tiny,
therefore nothing below it changes" is one causal chain, not two claims.** Split
apart, the EML half was 210 words — a code block with a wrapper, not an article.

The test that survives: two claims are separable when each has its own *reader*.
The contract and its consequence have the same one.

#3 was joined to #2 before that, and stays out — "why screens never get built"
is an economic argument for someone weighing effort, not a structural one.
**"One class, no service" belongs to #2 as a contract and to #3 as a price**; if
either starts making the other's argument they have merged again.

**Two ideas deliberately held back.** "No JavaScript — where the line is" is a
limits article and lands better once the series has credit. "Building UI5 with
an AI agent" has the most reach potential and would read as hype this early.

**The numbering has now moved twice** — RTTS from #2 to #1, and this merge
pulling everything up one. That is free while only #1 is published and stops
being free the moment #2 goes out.


## Format

**The article is the artefact.** Each piece is written to be read in full and
is not cut down to fit a feed. The LinkedIn post at the bottom of each file
exists only to introduce the article and ask one question — it is an
announcement, not a second version of the content.

**Short.** Around **300–400 words of article prose** — not the code blocks and
not the teaser post, which is measured in characters and belongs under ~1,000.
Count the three separately or the number means nothing; conflating article and
teaser is how article 1 was twice reported at a length it did not have.

That target is half of what article 1 shipped at (917), and it is deliberate:
at this size an article can only carry one claim, which is the constraint that
made the splits obvious. When a draft will not come under 400, the usual reason
is not that the topic is big — it is that there are two topics in it.

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

| # | File | Title |
|---|---|---|
| 1 | [`01-somewhere-on-the-way-to-ui5.md`](01-somewhere-on-the-way-to-ui5.md) | Somewhere on the Way to UI5, We Lost RTTS |
| 2 | [`02-not-a-programming-model.md`](02-not-a-programming-model.md) | abap2UI5 is not a Programming Model |
| 3 | [`03-the-cost-of-a-screen.md`](03-the-cost-of-a-screen.md) | The Cost of a Screen |

Each file holds the article, then the teaser post under a heading at the
bottom.


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

**The abaplint rules are `parser_error` and `check_syntax`.** A config that says
`"syntax_check": true` is not a stricter spelling of them, it is a rule that
does not exist — abaplint ignores it, reports `0 issue(s) found`, and the gate
passes on anything, garbage included. That is how three drafts came to carry a
view chain with one closing parenthesis too many: the `DATA(view) = ...`
statement never parsed, and nothing said so. A snippet a gate has never
actually rejected is a snippet nobody has checked. Break it on purpose once and
watch the run go red before trusting a green one.

Chains only balance if the last line closes exactly what it opened. Every line
after the first begins with `)`, which closes the call the line above left
open, so the final line ends `... v = \`x\` ).` — one parenthesis, never `) ).`

EML needs `"version": "v755"`; at `v750` the `MODIFY ENTITIES` and
`COMMIT ENTITIES` statements are reported as parser errors that are not there.

**What `errorNamespace` does and does not check.** With `"^(Z|Y)"`, an unknown
name outside the Z namespace is accepted in silence — `bapisdh1_NOPE` passes,
`ztravel_NOPE` does not. So in article 2 the `MODIFY ztravel` handler is really
checked (against a stub `ztravel.tabl.xml` with `TRAVEL_ID` and `DESCRIPTION`,
kept in the scratch gate, not here), while `BAPI_SALESORDER_CHANGE`, `bapisdh1`
and `bapisdh1x` parse without being resolved at all, and the EML entity
`z_i_travel` is not resolved either. A standard-SAP name in a snippet is
therefore checked by reading SAP's documentation, never by the gate — the
BAPI's parameters were confirmed against se80.co.uk and SAP Community before
publishing.

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

Two generators, both run from the repository root; each needs `pillow` and the
Chromium that ships with this environment.

| | |
|---|---|
| `assets/make-series-cover.py` → `series-cover.png` | **the reusable one.** A reading scene, and almost no words: the dinosaur deep in a book with a thought over its head, the sheep reading on a stack beside it, the sloth already at the keyboard. Learn, think, build — which reads for any article, and that is what a series cover has to do. Use it for anything in the series without its own picture |
| `assets/make-header.py` → `01-rtti-header.png` | article 1 only — its cover shows the RTTS code, so it belongs to that article |

**The series cover has no episode number on purpose.** The post carries the
number; baked into the image it becomes a second thing to keep in step, and
the picture stops being reusable the moment article 4 exists.

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

## Beyond #7

Ideas not yet placed, roughly in order of how well they would land:

| Working title | Core |
|---|---|
| No JavaScript — where the line is | standard UI5 fully from ABAP, custom controls are JS. A limits article, better once the series has credit |
| abap2UI5 in the Fiori launchpad | the hash split and the stripped `value` envelope — the two things that actually bite |
| Two-way binding without OData | `_bind( )`, model deltas, why the data is already current in the event handler |
| What CI looks like in an open source ABAP project | abaplint, the gates, generated artefacts |
| Building UI5 with an AI agent, without an SAP system | MCP server, headless render. Most reach potential, reads as hype if it comes early |
| From an SE80 report to UI5 in an afternoon | one concrete ALV migration; the natural sequel to #1 |

## Where the articles are published

Undecided. A LinkedIn article keeps everything in one place but is hard to link
to later and impossible to correct cleanly. A page under `docs/` — or a post on
a blog of your own — survives, can be fixed, and can be linked from the
repository; the LinkedIn post then introduces that instead. If the articles end
up in `docs/`, remember that the ABAP in them becomes gated code: `check:examples`
compiles complete view-building classes, so the fragments here would need to
grow into full classes or stay fragments on purpose.
