---
outline: [2, 4]
---
# Tooling

Everything on this page is optional — abap2UI5 is one ABAP class and needs no
tools at all. What these four add is the loop that ABAP does not give you by
itself: catching a broken view **before** it reaches a system, and seeing the
app without leaving the editor.

They are independent of each other. Take the first one and stop, or take all
four.

## Start a project from the template

[**abap2UI5/app-template**](https://github.com/abap2UI5/app-template) — press
*Use this template* on GitHub and you have an app repository that is already
set up: one working app class, both gates configured, a CI workflow that runs
them on every push, and a guide for AI agents.

The alternative is creating a class by hand, which works fine — the template
only saves you from assembling the checks below yourself, and from finding out
half a year later that they were never running.

```
Use this template → clone → npm ci → npm run check
```

[Your Project](/get_started/project_setup) walks through it — what is in the
repository, the rename that makes it yours, and the way into your system.

## Check the view without a system

[**abap2UI5/linter**](https://github.com/abap2UI5/linter) — the view your app
builds only exists at runtime, so no UI5 tooling can see it and the ABAP
compiler has no opinion about it. This one reconstructs the view from your
builder chain and judges the two together:

- controls, properties, aggregations and enum values that UI5 does not have,
  or does not have **yet** on the release you target (the `@since` floor —
  1.71 by default, which is what most systems serve),
- bindings that point at nothing, events nothing handles, deprecated controls,
- and then it loads every view in a headless browser, which is the only way to
  find a view that does not merely render wrongly but fails to load at all.

```sh
npx @abap2ui5/linter src
```

No SAP system, no install beyond npm. It also ships as a GitHub Action, and
the [app-template](https://github.com/abap2UI5/app-template) has it wired into
CI already. The [linter page](/technical/tools/linter) has the rest: the two
gates, `--fix`, and the baseline for switching it on over a codebase that
already exists.

## Run the app from the editor

[**abap2UI5/vscode-extension**](https://github.com/abap2UI5/vscode-extension) —
press `F9` on an app class and it starts in a preview beside your code,
against your real system. Plus completion and hover for the whole UI5 API
inside the builder chain, the linter running as you type, a click-a-control →
jump-to-the-line inspector, and a traffic log of every roundtrip.

Install it from the VS Code Marketplace or Open VSX; it needs the
[ABAP remote filesystem](https://marketplace.visualstudio.com/items?itemName=murbani.vscode-abap-remote-fs)
extension for the system connection.

## Let an AI agent build and see the app

[**abap2UI5/mcp-server**](https://github.com/abap2UI5/mcp-server) — an MCP server that
gives a coding agent the whole loop *without* an SAP system: it validates the
view, transpiles the framework and the app to Node, boots the app in a
headless browser and hands the agent a **screenshot**. That last step is the
difference between "the code compiles" and "the app works".

Works with any MCP client (Claude Code, Cursor, VS Code). The full loop needs
a few checkouts and a first build that takes a while — the README says which
tools need what, and validating views alone needs almost nothing.

::: tip Building with an AI agent?
Point it at [`llms.txt`](https://abap2ui5.github.io/docs/llms.txt) — this site
generates one on every build: every chapter with a one-line summary, and
[`llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) for all of it
in a single fetch. There is a second one in the
[framework repository](https://github.com/abap2UI5/abap2UI5/blob/main/llms.txt);
the difference is what each maps — this site's is the map of the **prose**, the
framework's is the map of the **code**, down to the interface files an agent
should read instead of guessing at a signature.

The [app-template](https://github.com/abap2UI5/app-template) then ships an
`AGENTS.md` that states the conventions an agent should follow in your own
project. [Building with AI](/get_started/ai) puts the whole setup in order.
:::

## Where the samples live

Three repositories, in the order they build on each other:

| | |
| --- | --- |
| [samples](https://github.com/abap2UI5/samples) | the fundamentals — binding, events, popups, navigation, and complete little apps |
| [samples-controls](https://github.com/abap2UI5/samples-controls) | the UI5 demo kit rebuilt in abap2UI5, one app per official sample |
| [samples-stack](https://github.com/abap2UI5/samples-stack) | abap2UI5 together with OData, RAP, WebSockets and the Fiori Launchpad |

All three install with abapGit and carry an overview app that lists everything
they contain.
