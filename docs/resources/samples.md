---
outline: [2, 4]
---
# Sample Catalogues

**612 working apps, in three repositories.** Every one is a single ABAP class
that compiles, renders, and is downported to three releases — so a sample is
never a fragment you have to trust, it is an app you can pull and run.

The three answer three different questions. Picking the wrong one costs more
time than reading this page.

| | | you are asking |
|---|--:|---|
| [**Learn**](https://abap2ui5.github.io/samples/) — abap2UI5/samples, [catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md) | 150 | *“Where do I start?”* — value help, navigation between apps, trees, tables, timers, file up- and download. Runs on a bare abap2UI5 install. |
| [**Controls**](https://abap2ui5.github.io/samples-controls/) — abap2UI5/samples-controls, [catalogue](https://github.com/abap2UI5/samples-controls/blob/main/SAMPLES.md) | 430 | *“Which control does what?”* — the UI5 demo kit, rebuilt control by control, grouped by library. |
| [**Stack**](https://abap2ui5.github.io/samples-stack/) — abap2UI5/samples-stack, [catalogue](https://github.com/abap2UI5/samples-stack/blob/main/SAMPLES.md) | 32 | *“Will my system run it?”* — OData, RAP, APC, MIME, the Fiori Launchpad. Each needs something the framework alone does not give you. |

They are not three steps to work through in order. **Learn** is a path and does
have a beginning; **Controls** is a reference you come back to whenever you
need one control; **Stack** only matters once the app has to talk to something
in your system. Pick the one that matches what you are asking today.

::: tip The three verbs are the same everywhere
**Learn**, **Controls** and **Stack** are what the three pages call each other
in the bar at the top of each of them, and what the playground calls them in
its About dialog. Same word, same page, wherever you meet it.
:::

## Two ways to read a catalogue

Each repository publishes its corpus twice, from one scan, so the two cannot
disagree:

| | |
|---|---|
| the **page** (the first link in each row above) | searchable in the browser, with the facets that corpus is actually asked for — a release your system runs, a control used anywhere in a view, a technology and the setup it needs |
| the **catalogue** `SAMPLES.md` | the same corpus as one long page on GitHub, for `Ctrl+F` and for reading offline. The counts above are of these. |

The pages are the better answer to *"is there a sample for X"*, because they
filter; the catalogues are the better answer to *"show me everything"*. The
page for **Learn** deliberately shows only the portable subset that survives
every build, so it lists fewer apps than the 150 the catalogue counts.

## The pages have the same shape on purpose

Every row of every catalogue reads the same way:

> **sap.m.Breadcrumbs**
> Breadcrumbs is useful for displaying link hierarchy
> <sub>breadcrumbs sap.m breadcrumb trail separator link hbox label select</sub>

The **title** says which sample it is, the **sentence** says what it shows, and
the **small type** are the words to search the page for — the terms somebody
would type who does not yet know the sample exists. `Ctrl+F` on any of the
three pages is a real search, not a scroll.

None of it is written on the page. All three lines live **on the class**, as
its abapGit short text and two comment lines above `CLASS … DEFINITION`:

```abap
" @keywords f4 search help suggestion input dialog select
" @summary The value help, both halves: suggestions while typing and the F4 dialog behind the field.
CLASS z2ui5_cl_smp_app_009 DEFINITION PUBLIC.
```

The catalogues are generated from those lines, and each repository refuses a
sample that is missing one. So a page can never describe a sample differently
from the class it links to — and a search engine that drops you into the class
gives you the same sentence the catalogue would have.

## For AI agents

The same three catalogues are queryable through the
[MCP server](/advanced/mcp_server) — the `examples` tool searches
all of them at once and answers with a repository, a class name and a path to
read, never with a copied snippet:

```
examples { query: "value help f4" }
examples { query: "wizard", repo: "samples-controls" }
```

The neighbouring question — *can abap2UI5 express this UI5 feature at all* — is
the `capabilities` tool, out of `samples-controls`'
[CAPABILITIES.md](https://github.com/abap2UI5/samples-controls/blob/main/CAPABILITIES.md).
Neither answers the other: a control being expressible says nothing about how
an app that uses it is put together.

## Running one

Install [abap2UI5](https://github.com/abap2UI5/abap2UI5), pull the repository
with [abapGit](https://abapgit.org), then start any class by name:

```
<your endpoint>?app_start=Z2UI5_CL_SMP_APP_009
```

Each repository also ships an **overview app** that lists its own samples with
a search box, so once the repository is in your system you do not need the page
here at all.

### Without a system

Every card on the **Learn** and **Controls** pages carries a button that opens
that class in the [playground](https://abap2ui5.github.io/playground/) — the
ABAP in an editor with the app running beside it, in the browser, nothing
installed anywhere.

**Stack** is the exception, and says so on its own cards: a Gateway service, a
RAP business object, an APC channel or a launchpad is precisely what the
playground does not have, so those samples would open there and then fail.
