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

A documentation repository has no compiler for its prose, but six things in it
are decidable, and `npm run check` decides all six before a merge — the prose
builds (`docs:build`), the fenced ABAP examples compile and the views they
build name real UI5 API (`check:examples`), every `client->` name and `cs_*`
constant the prose and snippets mention still exists in the release the site
names (`check:api-names`), the sample links still match the
sample repositories (`check:samples`), the release number in the nav bar still
matches the framework (`check:version`), and the catalogue parser still parses
(`test`).
`.github/workflows/check.yml` runs the same list in the same order, so a green
`npm run check` locally is a green pull request — and `deploy.yml` runs it
again before it publishes, so the site is only ever built from a tree that
passed. Several of these go stale without anybody touching this repository (a
release is published elsewhere, a sample class is renamed elsewhere), which is
why the deploy re-runs them rather than trusting the merge.

**[AGENTS.md](AGENTS.md) describes each of the six**, what a failure means and
which of them need a sibling checkout to say anything at all — read it before
changing anything beyond prose.

### What the site publishes for machines

Besides the site, `docs:build` writes [`llms.txt`](https://abap2ui5.github.io/docs/llms.txt),
[`llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) and a raw
`.md` next to every `.html`, so an AI assistant asked about abap2UI5 can read
the current documentation instead of guessing from training data. All three are
generated on every build and gitignored — never edit them, and nothing needs
maintaining: the page list comes from the **sidebar**, so adding a page there
adds it here. [AGENTS.md](AGENTS.md) has the details.

### Running an example from the page

An ABAP example that the [playground](https://github.com/abap2UI5/playground)
can start carries a **Run this example** button under it, and pressing it puts
the running app under the code — the framework compiled into the page, no
server and no SAP system anywhere. The code that runs is read out of the block
the reader is looking at, so the printed example and the running one cannot be
two different things.

Nothing is fetched until somebody presses it: a reader who never does makes no
request to another site.

Which blocks get a button is decided in `docs/.vitepress/playground.mjs`, and
the rule is narrow on purpose — a button on an example that cannot run is worse
than no button. It has to be a complete class implementing `z2ui5_if_app` that
displays something and needs nothing the browser has not got: no table of its
own, no CDS entity, no add-on repository, no on-premise SAP class. **38 of the
261 ABAP blocks here** clear that today. Every rule was written from an example
watched failing in a real playground; `test/playground.test.mjs` keeps one
fixture per shape, and [AGENTS.md](AGENTS.md) says how to redo the measurement.

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
