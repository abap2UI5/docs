---
outline: [2, 4]
---
# Tooling

Everything on this page is optional — abap2UI5 is one ABAP class and needs no
tools at all. What these three add is the loop that ABAP does not give you by
itself: catching a broken view **before** it reaches a system, and seeing the
app without leaving the editor.

They are independent of each other. Take the first one and stop, or take all
three. Building with an AI assistant? That whole side of the tooling — the
indexes, the agent conventions, the MCP server — is collected on
[Building with AI](/get_started/ai).

## Start a project from the template

[**abap2UI5/app-template**](https://github.com/abap2UI5/app-template) — press
*Use this template* on GitHub and you have an app repository that is already
set up: one working app class, both gates configured, and a CI workflow that
runs them on every push.

The alternative is creating a class by hand, which works fine — the template
only saves you from assembling the checks below yourself, and from finding out
half a year later that they were never running.

```
Use this template → clone → npm ci → npm run check
```

[Working Off-Stack](/advanced/working_off_stack) walks through it — what is in the
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
CI already. The [linter page](/advanced/linter) has the rest: the two
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

## Where the samples live

Three repositories, in the order they build on each other:

| | |
| --- | --- |
| [samples](https://github.com/abap2UI5/samples) | the fundamentals — binding, events, popups, navigation, and complete little apps |
| [samples-controls](https://github.com/abap2UI5/samples-controls) | the UI5 demo kit rebuilt in abap2UI5, one app per official sample |
| [samples-stack](https://github.com/abap2UI5/samples-stack) | abap2UI5 together with OData, RAP, WebSockets and the Fiori Launchpad |

All three install with abapGit and carry an overview app that lists everything
they contain.
