---
outline: [2, 4]
---
# Sample Catalogues

**615 working apps, in three repositories.** Every one is a single ABAP class
that compiles, renders, and is downported to three releases — so a sample is
never a fragment you have to trust, it is an app you can pull and run.

The three answer three different questions. Picking the wrong one costs more
time than reading this page.

| | | you are asking |
|---|--:|---|
| [**samples**](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md) | 152 | *has somebody already built this pattern?* — value help, navigation between apps, trees, tables, timers, file up- and download. Runs on a bare abap2UI5 install. |
| [**samples-controls**](https://github.com/abap2UI5/samples-controls/blob/main/SAMPLES.md) | 431 | *how is this UI5 control expressed in ABAP?* — the UI5 demo kit, rebuilt control by control, grouped by library. |
| [**samples-stack**](https://github.com/abap2UI5/samples-stack/blob/main/SAMPLES.md) | 32 | *how do I reach my system from an app?* — OData, RAP, APC, MIME, the Fiori Launchpad. Each needs something the framework alone does not give you. |

Start with **samples** if you are learning abap2UI5, with **samples-controls**
if you know which control you want, and with **samples-stack** only when the
app has to talk to something in your system.

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
[MCP server](https://github.com/abap2UI5/ai-mcp) — the `examples` tool searches
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
