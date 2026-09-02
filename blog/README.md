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
| 2 | abap2UI5 is not a Programming Model | draft, 336 words |
| 3 | The Cost of a Screen | draft, 334 words |
| 4 | No Annotation in Between | draft, 346 words |
| 5 | UI5 Over-the-Wire | draft, 321 words |
| 6 | The Frontend That Does Not Know What It Shows | draft, 280 words |
| 7 | One Service for Every App | draft, 281 words |
| 8 | Only the Changed Part | draft, 229 words |
| 9 | PUBLIC Means Persisted | draft, 243 words |
| 10 | Swapping the View at Runtime | draft, 220 words |
| 11 | index.html Lives in a String | draft, 274 words |
| 12 | Where Your Own JavaScript Goes | draft, 285 words |
| 13 | Four Verbs | draft, 307 words |
| 14 | The Class That Runs | draft, 213 words |
| 15 | Where the Selection Screen Went | draft, 269 words |
| 16 | No Cache, No Deploy, Any IDE | draft, 290 words |
| 17 | One Codebase, 7.02 to ABAP Cloud | draft, 306 words |
| 18 | 2,300 Lines | draft, 283 words |
| 19 | Where the Line Is | draft, 324 words |
| 20 | Cloud-Ready Is a Property of Your App | draft, 230 words |
| 21 | Twenty-Five Years of ABAP on the Web | draft, 298 words |
| 22 | Where the View Lives | draft, 332 words |

**Every article answers one question**, and the arc is 2–4 positioning, 5–8
architecture, 9–13 mechanics, 14–16 developer experience, 17–20 practice and
limits, 21–22 the long view and the landscape. Someone arriving at #9 first
should not need #2.

| # | asks | evidence |
|---|---|---|
| **2** | what does it want from my architecture? | the `z2ui5_if_app` interface in full, then one app with three save handlers — EML, `MODIFY` on a table, a BAPI |
| **3** | why do small screens never get built? | a whole job monitor in one class |
| **4** | what do I write the screen in? | a three-step `sap.m.Wizard` — a control no generator produces — named directly in UI5 terms |
| **5** | what actually goes over the wire? | the two strings a response is: a UI5 XML view and a UI5 JSON model |
| **6** | what is in the app the browser gets? | a shell that cannot tell a table from a wizard, shared by every app |
| **7** | what does a screen cost in artefacts? | one generic handler, one stack frame, no service per app |
| **8** | does the screen rebuild on every click? | the `IF` that skips the view and sends the model alone |
| **9** | which convention will bite me? | `PUBLIC SECTION` is state that travels every roundtrip |
| **10** | how far does runtime freedom go? | one `IF` turns the table into a list |
| **11** | where is the frontend deployment? | there is none — the initial GET is answered from ABAP source |
| **12** | how do I get my own JavaScript in? | the three declared seams, and which one to reach for |
| **13** | why four verbs instead of a method per control? | the wrapper that could not name a control it lacked |
| **14** | how small is the smallest app? | `if_oo_adt_classrun` beside its abap2UI5 twin |
| **15** | what happened to `PARAMETERS`? | the variable and the field, reunited by `_bind` |
| **16** | what does the loop feel like? | change, activate, refresh — and everything that is not in that sentence |
| **17** | does it run on my release? | one code line, the generated 7.02 branch, UI5 from a CDN |
| **18** | how much of it do I have to trust? | one handler, two interfaces, one table |
| **19** | what does it not do? | offline, HANA pushdown, real-time, split teams, and where a floorplan wins |
| **20** | is my app cloud-ready because the framework is? | `SELECT FROM i_salesorder` beside `SELECT FROM vbak` |
| **21** | where does this sit in the line? | ITS to abap2UI5, and what actually moved |
| **22** | how does it sit next to the others? | build time, activation time, request time — the one axis the rest follows from |

## Source coverage — the docs pages this series drains

The series is the LinkedIn form of the **Technical Insight** section of
`docs/technical/`. Each page is one long read; the articles are the same
material cut where a single question ends. This table is the contract: **every
section has exactly one article that carries it**, so nothing is lost and
nothing is told twice.

The section holds six pages, not the three this started with. `cloud.md` and
`technology/overview.md` were not covered by anything and now have articles
**20** and **21**. `technology/rap.md` and `technology/ui5.md` are carried by **22**,
under a rule that changed for them — see below. The **Toolchain**
group (abapGit, ajson, S-RTTI, abaplint, open-abap, abap-cleaner, abapmerge) is
out of scope entirely: those pages document other people's projects and are
reference material, not narrative.

| source | section | carried by |
|---|---|---|
| `concept.md` | What is HTML Over-the-Wire / Comparison to Classic SSR | **5** |
| `concept.md` | How UI5 Freestyle Works / Sending Views from Backend | **5** |
| `concept.md` | Frontend Events on the Server | **6** |
| `concept.md` | Create and Update Data / Application Flow | **7** |
| `concept.md` | Partial HTML Updates | **8** |
| `concept.md` | Conclusion — benefits and limitations | **19** |
| `how_it_all_works.md` | 1–4 HTML Over-the-Wire, HDA, separation of concerns | **5** |
| `how_it_all_works.md` | 5–7 UI5 architecture, abap2UI5 architecture, merging data and presentation | **6** |
| `how_it_all_works.md` | 8 RAP | *not carried* — the series does not compare frameworks |
| `how_it_all_works.md` | 9, 12, 13, 14 one HTTP service, decoupled from view and model, REST | **7** |
| `how_it_all_works.md` | 10 define model at runtime | **1** (posted) |
| `how_it_all_works.md` | 11 define view at runtime | **10** |
| `how_it_all_works.md` | 15 the abap2UI5 app | **2** |
| `how_it_all_works.md` | 16 draft | **9** |
| `how_it_all_works.md` | 17 initial request, index.html as a string | **11** |
| `how_it_all_works.md` | 18 everything maintained in the backend | **3** |
| `how_it_all_works.md` | 19 no extra layer | **4** |
| `how_it_all_works.md` | 20 no hiding of complexity, the two view builders | **13** |
| `how_it_all_works.md` | 21 separated `_bind` and `_event` | **13** |
| `how_it_all_works.md` | 22 sending JS, HTML and CSS over the wire | **12**, corrected — see below |
| `how_it_all_works.md` | 23 as simple as possible | **14** |
| `how_it_all_works.md` | 24 downsides vs UI5 and RAP | **19** |
| `how_it_all_works.md` | 25 system footprint | **18** |
| `how_it_all_works.md` | 26–28 running everywhere, one code line, downporting | **17** |
| `dx.md` | Simple Output with `IF_OO_ADT_CLASSRUN` | **14** |
| `dx.md` | Classic Input Handling with Selection Screens | **15** |
| `dx.md` | ALV-Style Table Output in the Browser | **1** (posted) |
| `dx.md` | Classic Popups, Modern Events | *not carried* — see below |
| `dx.md` | Zero-Setup Deployment / No Caching / Any IDE / Pure ABAP Debugging / Easy Code Sharing | **16** |

**Section 22 of `how_it_all_works.md` describes a mechanism the framework no
longer has.** It says an app can send its own JavaScript or custom controls on
any request and the framework forwards them as-is. Today `custom_js` is a field
of `ty_s_http_config`, set once per system in `set_config_http_get` through
`z2ui5_if_exit` — the initial GET, not a per-request app decision — and custom
controls live in their own BSP behind the reserved resource roots `z2ui5_cci`
(addon) and `z2ui5_ccc` (customer extension). Article 12 describes the seams
that exist now, with `follow_up_action( cs_event-control_by_id )` as the third.
The docs page needs the same correction.

| `cloud.md` | What is ABAP Cloud / Is abap2UI5 Cloud Ready | **20** |
| `cloud.md` | Are abap2UI5 Apps Cloud Ready — `I_SalesOrder` vs `VBAK` | **20** |
| `cloud.md` | Do I Have to Use RAP to Be Cloud Ready | **20** (as "the framework's badge is not the app's") |
| `technology/overview.md` | ITS, BSP, Web Dynpro, UI5 Freestyle, RAP, abap2UI5 | **21** |
| `technology/rap.md` | architecture, communication, workflow, runtime | **22** |
| `technology/ui5.md` | architecture, communication, workflow, runtime | **22** |
| both | the verdict rows — Learning Curve, Use Case Fit, ✅/❌ | *not carried, deliberately* |

## The comparison rule, and how it changed

The series began with a rule against comparing frameworks, and that rule kept
section 8 of `how_it_all_works.md` out and left `technology/rap.md` and
`technology/ui5.md` unconverted. The owner then narrowed it rather than dropping
it: **a comparison is fine when it is purely informative and shows the
differences.** What stays out is ranking.

That line is easy to state and easy to lose, so concretely — article **22**
carries the architectural rows of both pages: where the view is defined, where
it is rendered, what travels, what is deployed per app, when the definition is
fixed. It does **not** carry the verdict rows those pages also have — "Learning
Curve: High / Low", "Use Case Fit", the ✅/❌ columns. Those score one approach
against another, which is the part the series still does not do, and it is also
the part that ages worst.

The article ends by saying so outright: a definition fixed early standardises
well, one fixed late adapts well, and those are different properties rather than
different amounts of the same one. If a future article starts totalling up
points again, it has crossed back over.

Everything else in those two pages is already carried positively elsewhere:

| the row | where it already lives |
|---|---|
| backend sends the view / client builds from metadata | **5**, **6**, **22** |
| model at design time / at runtime | **1**, **10** |
| drafts on model level / serialization on app level | **9** |
| separate frontend transport / single backend deployment | **3**, **11**, **16** |
| any ABAP IDE, no extra tooling | **16** |
| multiple layers / one layer | **7**, **18** |
| annotations to learn / the UI5 API to learn | **4**, **13** |
| cloud-ready and clean core | **20** |
| OData metadata flow / HTTP event flow | **5**, **7**, **8** |

## Archiving — audited, not yet safe

Every *argument* in the seven pages has an article. That is not the same as
every *thing* in them, and an audit against the pages rather than against the
table above turns up three blockers. None is a reason not to archive; each is
work that has to happen first.

**1. The content would leave the website.** `blog/` sits at the repository root,
outside `docs/`. The site is built with `vitepress build docs` and
`generate-llms.mjs` walks the same tree, so these articles are in neither. The
seven pages are ~10,400 words of the only architecture documentation the site
has; deleting them moves that to LinkedIn and the SAP Community, which are not
this project's documentation. A reader landing on the docs asking "how does this
actually work" would find nothing. Either the articles move into `docs/` — their
ABAP already passes the gates, so that is cheap — or the pages stay as the long
version and link out.

**2. Fifty diagrams and sixty-two references are not carried.**

| | source pages | articles |
|---|---|---|
| images | 50 (39 in `how_it_all_works.md`, 11 in `concept.md`) | 1 |
| external links | 62 | 4 |

In `how_it_all_works.md` the diagrams *are* the explanation — the Over-the-Wire
lifecycle, UI5 versus abap2UI5 communication, the call stack, `z2ui5_t_draft`,
CDN against local bootstrapping, plus animated demos of the model and the view
being swapped at runtime and of the class being edited without reloading the
frontend. Three hundred words of prose do not replace an animation of the thing
happening. The links are attribution as much as reference: `concept.md` quotes
signalvnoise verbatim and credits the SAP Community htmx article the idea came
from, and no article carries either citation.

**3. Six facts are in no article at all.** Measured by grepping the articles for
each term, not by reading the table:

| missing | source | why it matters |
|---|---|---|
| Hypermedia-Driven Application, MPA vs SPA vs HDA, the "sweet spot" | `how_it_all_works.md` 2, 4 | the vocabulary a reader searches for; **5** and **6** have the idea without the name |
| `z2ui5_t_draft` | 16 | **9** explains the mechanism and never names the table somebody has to look into |
| expression binding / side effects, and RAP's `+ - *` | 20 | carried nowhere; **12** covers the seams, not this |
| VDM by name | `cloud.md` | **20** says "the released data model" without the term |
| SEGW | 9 | minor, but it is the thing not being built |
| the SCN backlink | `how_it_all_works.md` header | the page *is* a published SAP Community post; deleting it orphans that |

**Also to handle before deletion**, cheap but easy to forget:
`docs/get_started/hello_world.md:44` links to `concept` and `how_it_all_works`
in prose; `docs/.vitepress/config.mjs` 496-509 has the whole Technical Insight
group; `test/playground.test.mjs:92` names `technical/dx.md` in a comment about
T100.

**The recommendation is archive, not delete.** Keep the seven pages as the long
version, add the article links to them, and let the series be the way in. If
they must go, the order is: move the articles into `docs/`, re-home the
diagrams, fold the six missing facts into the articles that should have had
them, and only then remove the pages.

The **Toolchain** group is untouched either way: those pages document other
people's projects and are reference material, not narrative.

**Two things the docs pages say that must not travel into an article.**
`dx.md` tells the reader to "use `Z2UI5_CL_XML_VIEW` to define simple views"
while the code beneath it uses `z2ui5_cl_ui5_view_builder`, and its popup
section teaches `z2ui5_cl_pop_to_confirm`. Both classes are `src/99` — frozen
legacy with zero in-repo consumers, kept only so existing installations keep
compiling, and the framework's own AGENTS.md says they "must never be used,
called from new code". A published snippet is the most-copied ABAP the project
produces, so the popup section is dropped rather than ported, and the
[popups addon](https://github.com/abap2UI5-addons/popups) is what an article
would link instead. **The docs pages themselves need the same fix** — that is a
`docs/` change, gated, and separate from this directory.

**Section 8 of `how_it_all_works.md` is deliberately not carried.** It is a RAP
comparison, and the series does not compare frameworks — that decision predates
this table and survives it. What the section is *for* — that a generic service
buys runtime freedom — is argued in **7** on its own evidence, without naming
what it is freer than.

## Publishing to the SAP Community as well

Each article goes out on LinkedIn first and is then posted on the SAP
Community. Cross-posting is allowed there **only when the source is stated** —
the Rules of Engagement permit syndicated content that names where it came
from, and prohibit duplicate content that does not. So every SCN post opens
with a line naming the LinkedIn original and linking it, and no article is
posted twice within SCN itself.

One thing to know before starting: `how_it_all_works.md` **is itself already an
SCN post** (linked at the top of the page). Articles 5–19 are cut from it, so
each SCN post also links that original as the long version. That is the
honest framing and it is also the useful one — the small article is the way in,
the deep dive is where a reader who wants all of it goes.

Practical differences from LinkedIn:

- SCN needs a primary tag; the ABAP Development and SAP Fiori tags are where
  this audience is.
- SCN renders real code blocks, so the article does not need the LinkedIn
  compromise of a screenshot or a link for code.
- Images have to be uploaded to SCN; GitHub-hosted URLs in the docs pages are
  not a substitute.

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

**Why "no annotation in between" is not a paragraph in #2.** It passes the
separability test above — it has its own reader. #2 answers what the framework
wants from the architecture *below* the screen, and its reader is weighing
coexistence with what is already installed. #4 answers what the screen is
written in, and its reader has already hit a control the annotation vocabulary
does not reach. #2 lists "no annotations" among the things the contract does not
contain and stops there; the moment it starts arguing about the UI5 API the two
have merged.

**One idea is still held back.** "Building UI5 with an AI agent" has the most
reach potential and would read as hype this early. The other one that was held
back, "No JavaScript — where the line is", has since been spent: **19** is the
limits article and **12** covers where custom JavaScript goes.

**The numbering has moved three times** — RTTS from #2 to #1, the contract/EML
merge pulling everything up one, and the docs conversion inserting #4 and #5.
That was free while only #1 was published, and it stops being free the moment #2
goes out. The series is complete now, so it should not move again.


## One thought per article

**250–350 words of prose, one claim, one piece of evidence.** Article 1 ran to
938 and was the exception, not the template; everything after it fits on one
screen. The test is whether the article can be summarised in a single sentence
without losing anything — if it takes two, it is two articles.

Each one also has to earn its place against what the reader is dealing with
right now, and each does so on a different front rather than all on the same
one: fewer governed objects (**7**), no build pipeline to audit (**11**), an app
that survives the release migration (**17**), a dependency small enough to read
(**18**), one file a reviewer or an agent can hold in full (**14**), iteration
speed as the thing that decides which ideas get built (**16**). Where an article
has no such angle, it does not get one bolted on — **8**, **13** and **15** are
purely technical and stay that way.

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

## Beyond #22

The Technical Insight pages are fully converted, so nothing below is owed to a
source page — these are ideas of their own, roughly in order of how well they
would land:

| Working title | Core |
|---|---|
| abap2UI5 in the Fiori launchpad | the hash split and the stripped `value` envelope — the two things that actually bite |
| Two-way binding without OData | `_bind( )`, model deltas, why the data is already current in the event handler |
| What CI looks like in an open source ABAP project | abaplint, the gates, generated artefacts |
| Building UI5 with an AI agent, without an SAP system | MCP server, headless render. Most reach potential, reads as hype if it comes early |
| From an SE80 report to UI5 in an afternoon | one concrete ALV migration; the natural sequel to #1 |

"No JavaScript — where the line is" was held back here for a long time and is
now spent: **19** is the limits article, and **12** covers where custom
JavaScript goes.

## Where the articles are published

Undecided for the article body. A LinkedIn article keeps everything in one place but is hard to link
to later and impossible to correct cleanly. A page under `docs/` — or a post on
a blog of your own — survives, can be fixed, and can be linked from the
repository; the LinkedIn post then introduces that instead. If the articles end
up in `docs/`, remember that the ABAP in them becomes gated code: `check:examples`
compiles complete view-building classes, so the fragments here would need to
grow into full classes or stay fragments on purpose.
