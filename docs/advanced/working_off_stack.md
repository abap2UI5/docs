---
outline: [2, 4]
---
# Working Off-Stack

An abap2UI5 app usually starts as a class typed straight into a system, living
there and nowhere else. That is fine for a first app and not fine for a second
one: nothing versions it, nobody reviews it, and the mistakes an abap2UI5 app
makes — a control that does not
exist on the release your users are on, a binding pointing at nothing — are
invisible to the ABAP compiler and show up as a blank screen.

A real project is the same class in a git repository that abapGit pulls into
the system, with the checks running before it gets there.
[**abap2UI5/app-template**](https://github.com/abap2UI5/app-template) is that
repository, already assembled.

## What it gives you

| | |
| --- | --- |
| `src/zcl_app_001` | a working app — an input, a bound table, an event — in the canonical shape its `AGENTS.md` describes |
| `abaplint.jsonc` | ABAP syntax and style, with the framework resolved as a dependency, so it compiles the app **without an SAP system** |
| `abap2ui5lint.jsonc` | the [abap2UI5 linter](/advanced/linter) — the view your ABAP builds, judged against the UI5 API and against your own class |
| `.github/workflows/check.yml` | both gates on every push and pull request, at the versions `package-lock.json` pins — so CI and your machine run the same thing |
| `AGENTS.md` | the conventions an AI assistant should follow in this project, plus a `.claude/settings.json` allowlist so it can run the gates without asking |

## From template to first green check

Press **Use this template** on GitHub, then:

```sh
git clone <your new repository>
cd <your new repository>
npm ci
npm run check
```

That is the first green check, and no SAP system was involved. Two more
commands are worth knowing:

```sh
npm run check:abap2ui5:fast   # skip the browser half — seconds, not a minute
npm run fix                   # apply the corrections the linter can make itself
```

The full check loads every view in a headless browser, which needs Chromium
once: `npx playwright install chromium`.

## Make it yours

Fresh from the template the repository is still called *app-template*, the
ABAP package still says *abap2UI5 app*, and the app is still `ZCL_APP_001`.
One command changes all three:

```sh
npm run rename -- --class zcl_my_app --package "My App" --repo my-app
```

Add `--dry` first to see what it would touch. It renames the class in the ABAP
**and** the `CLSNAME` in its `.clas.xml` sidecar — changing only one gives you
an object abapGit imports under one name and ABAP activates under another.

Two things it deliberately leaves to you: the **namespace** (`abaplint.jsonc`
requires `^ZCL_` or `^ZCX_`; change `object_naming` there if you develop behind
a company prefix or a registered namespace), and the **LICENSE**, which still
names abap2UI5 as the copyright holder.

## Then into the system

Install the [framework](/get_started/quickstart) and then your own repository,
both with [abapGit](https://abapgit.org). The starter app needs framework
**1.143.0** or newer — that is the release the gates lint against. Open
`<your endpoint>?app_start=zcl_my_app` and you are looking at the same app the
checks just passed.

## Next Steps

- [Tooling](/get_started/tooling) — the editor and agent side of the same loop
- [abap2UI5 linter](/advanced/linter) — what the view gate actually
  checks, and how to adopt it on a codebase that already exists
