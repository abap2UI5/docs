---
outline: [2, 4]
---
# Developing with AI

abap2UI5 is unusually well suited to being written by an AI assistant, and the
reason is structural rather than lucky:

- **An app is one ABAP class — source code, and nothing else.** No service to
  generate, no OData artifacts, no frontend project, no manifest, no
  deployment pipeline. There is exactly one file for an agent to write, and
  the thing it writes is the thing that runs.
- **The whole app is text.** View, logic, state and data flow live in the same
  class, in one language. An agent never has to keep an ABAP backend and a
  JavaScript frontend in step, because there is no second half to drift.
- **There are hundreds of working examples to learn from.** The three sample
  catalogues hold complete, tested apps — one per pattern, every one linted and
  rendered — so "has somebody already built a value help, a tree, navigation
  between two apps?" is a question with a real answer instead of a guess.
- **The result can be checked without an SAP system.** The
  [abap2UI5 linter](/advanced/linter) reconstructs the UI5 view out of the ABAP
  that builds it and reports what UI5 does not have. An agent that can verify
  its own work stops handing you apps that do not render.

Everything below turns those properties into a setup, in rising order of
effort. Start at the top; each level is useful on its own.

## Paste the essentials

The zero-setup version, for any assistant with web access: paste this ahead of
your task.

```text
Before writing any abap2UI5 code, read https://abap2ui5.github.io/docs/llms.txt
and follow it to the pages you need.

The shape of an abap2UI5 app:
1. An app is ONE ABAP class implementing z2ui5_if_app. Everything enters main( ),
   which dispatches on client->check_on_navigated( ) (the display branch, true on
   first start too), client->check_on_event( `X` ) and - for one-time setup only -
   client->check_on_init( ).
2. Build the view with z2ui5_cl_ui5_view_builder and its verbs ele / tag / a / end /
   stringify.
3. Bind with client->_bind( ). It is bidirectional; only what the user edited comes back.
4. Every roundtrip is a fresh ABAP session. Nothing survives on the server except
   the app class itself, which is serialized.

Before building something from scratch, check whether it exists: the sample
catalogue lists every app with the words to search it by, at
https://github.com/abap2UI5/samples/blob/main/SAMPLES.md

When you are done, check the result with the abap2UI5-linter
(npx abap2ui5lint) - it reads the view your ABAP builds and needs no SAP system.
```

## Point it at the right index

Two files describe this project to a machine, and they answer different
questions:

| | |
| --- | --- |
| [`abap2ui5.github.io/docs/llms.txt`](https://abap2ui5.github.io/docs/llms.txt) | the map of the **prose** — every chapter of this site with one line of what it covers, and [`llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) for all of it in one fetch |
| [`github.com/abap2UI5/abap2UI5/llms.txt`](https://github.com/abap2UI5/abap2UI5/blob/main/llms.txt) | the map of the **code** — the interface files to read instead of guessing at a signature, and the guide for building apps that ships with the framework |

Both are short and both are free to give an assistant that has web access.
This is the cheapest step on the page: an agent that has read either one is
working from what abap2UI5 is today rather than from what it recalls.

## Put the conventions in the repository

An index tells an agent what abap2UI5 is. `AGENTS.md` tells it what *your
project* is — and it is read automatically, by every session, without anybody
remembering to paste anything.

The [app-template](/advanced/working_off_stack) ships one written for
app-building: the class shape, the lifecycle, the view builder, binding,
events, and the gates to run before calling the work done. It also ships a
`.claude/settings.json` allowlist so an agent can run `npm run check` itself
instead of stopping to ask.

## Give it the gates

An agent that cannot check its own work will hand you an app that does not
render. The two gates of the template need no SAP system, which means an agent
can run them on its own:

```sh
npm run check
```

The [abap2UI5 linter](/advanced/linter) half is the one that matters
here: it reconstructs the view from the builder chain and reports the names UI5
does not have, the properties that do not exist on the release you target, and
the bindings that point at nothing.

## Give it the loop

The [**MCP server**](/advanced/mcp_server) turns the checks into a development
loop, still without a system. It works with any MCP client — Claude Code,
Cursor, VS Code:

```sh
claude mcp add abap2ui5 -- npx --yes @abap2ui5/mcp-server
```

The tools an agent then has:

| | |
| --- | --- |
| `examples` | search the three sample catalogues — *has somebody already built a value help, a tree, navigation between two apps?* Answers with a class to read, never with a snippet to trust |
| `capabilities` | whether abap2UI5 can express a UI5 feature at all, from the verified capability map |
| `validate_view` | the linter's gates, in seconds, against your project's own config |
| `deploy_app` | write the class into a local sandbox and compile it |
| `build_backend` / `run_app` | transpile the framework and the app to Node, boot it headless, and hand back the errors **and a screenshot** |
| `pitfalls` | the defects a green run still does not catch — abapGit import, activation, the oldest UI5 release |

Set-up is levelled: validating views needs one small checkout and a minute;
the screenshot loop needs a browser and a first build measured in tens of
minutes. Stop where the value stops for you — the
[MCP Server page](/advanced/mcp_server) has the three levels, every tool and
the loop they are meant to be used in.

## From the editor

The [VS Code extension](/advanced/vscode)
registers that same MCP server for every client in the window — Copilot agent
mode, Claude Code, anything else speaking MCP — so an agent working in your
editor has the loop without any separate configuration. Point
`abap2ui5.mcp.reposRoot` at the folder holding the checkouts and the extension
passes the paths through.

It adds a second server of its own for the half that one deliberately does not
have: your configured **systems**. An agent can list them, search app classes
over ADT and get the app rendered on the real system as a screenshot — while
every credential prompt stays an ordinary VS Code dialog the agent never sees.

## Next Steps

- [Working Off-Stack](/advanced/working_off_stack) — the repository all of this
  assumes
- [Tooling](/get_started/tooling) — the human side of the same loop: the
  template, the linter, and the VS Code extension
