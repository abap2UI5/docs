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
npm run check        # what CI runs: build + ABAP examples + sample links
```

### What CI checks

A documentation repository has no compiler for its prose, but three things in
it are decidable, and all three are decided before a merge:

| | |
|---|---|
| `docs:build` | a page that does not build is a page nobody can read |
| `check:examples` | the ABAP in the fenced blocks, against the real framework: does it compile, and does the view it builds name controls and properties that exist on the UI5 floor this documentation targets |
| `check:samples` | the **Working Samples** blocks, against [abap2UI5/samples](https://github.com/abap2UI5/samples) |

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
