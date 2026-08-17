# abap2UI5-docs

Welcome! 👋

This repository contains the source files for the [abap2UI5 documentation](https://abap2ui5.github.io/docs/).

## Contributing

We appreciate your help! If you spot a typo, want to improve existing content, or have ideas for new topics, feel free to open a pull request or issue.

Every contribution makes the documentation better for the community!

## Working on it locally

```sh
npm ci
npm run docs:dev     # the site, with hot reload
npm run check        # what CI runs, all six steps
```

### What CI checks

A documentation repository has no compiler for its prose, but six things in
it are decidable, and all six are decided before a merge — `npm run check`
and `.github/workflows/check.yml` run the same list, in the same order:

| | |
|---|---|
| `test` | the sample-catalogue parser in `scripts/lib/`, against a row of every shape the three sample repositories generate |
| `check:version` | the release number in the nav bar, the deprecations page and the changelog, against the newest release tag of the framework — this one goes stale without anybody touching this repository |
| `docs:build` | a page that does not build is a page nobody can read |
| `check:examples` | the ABAP in the fenced blocks, against the real framework: does it compile, and does the view it builds name controls and properties that exist on the UI5 floor this documentation targets |
| `check:samples` | the **Working Samples** blocks, against [abap2UI5/samples](https://github.com/abap2UI5/samples) |
| `check:counts` | the four figures on `resources/samples.md` — one count per sample repository and the total they add up to — against the catalogues themselves |

### What the site publishes for machines

`docs:build` runs `scripts/generate-llms.mjs` first, which writes three things
into `docs/public/` — generated on every build and **gitignored**, because they
are a projection of the pages next to them:

| | |
|---|---|
| [`/docs/llms.txt`](https://abap2ui5.github.io/docs/llms.txt) | the map: every page with its title and one line of what it covers, plus the repositories around it |
| [`/docs/llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) | the whole documentation as one markdown document |
| `/docs/<page>.md` | each page as raw markdown, next to its `.html` |

This is for the reader nothing else reaches: an agent that is simply *asked*
about abap2UI5, with no MCP server and no checkout. Without it, it falls back
on training data — where abap2UI5 still looks like `z2ui5_cl_xml_view`.

The page list comes from the **sidebar**, not from a directory walk, so a
machine meets the documentation in the order a reader does. A page in the tree
that no sidebar links is reported by name and published anyway.

Nothing needs maintaining. Adding a page to the sidebar adds it here.

### Linking a sample from a page

A page lists the samples that demonstrate it in its frontmatter, by class name
and nothing else:

```yaml
---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_009
  - z2ui5_cl_smp_app_078
---
```

`npm run link:samples` then writes the *Working Samples* table at the bottom of
the page. **Do not edit that block** — the title, the folder and the URL are
read from the sample catalogue, so a sample that gets re-described or moved
updates on the next run, and one that gets deleted fails the check instead of
leaving a 404 on the site. The one thing a page states is which samples are
relevant, which is the one thing the catalogue cannot know.

It needs a samples checkout — `SAMPLES_HOME=...`, or `abap2UI5/samples` cloned
as a sibling directory. Without one both commands say so and do nothing: the
generated blocks are committed, so building the site never needs it.

The link is checked in **both** directions. A class this documentation points
at has to carry a `" @docs` line naming the page, so somebody who lands in the
samples repository from a search engine finds the chapter that explains what
they are reading. Add the `samples:` entry here first, then the `" @docs` line
over there — [`AGENTS.md` §4](https://github.com/abap2UI5/samples/blob/main/AGENTS.md)
in that repository describes it.
