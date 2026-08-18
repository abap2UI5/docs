---
outline: [2, 4]
---
# Building with AI

An AI assistant writing abap2UI5 starts at a disadvantage nothing about your
project causes. Almost every abap2UI5 example on the public web builds its view
with `z2ui5_cl_xml_view`, the frozen predecessor of
`z2ui5_cl_ui5_view_builder` — so that is what a model writes when asked for an
app, confidently, and in an API that is no longer the one to use.

Everything below is a way of telling it otherwise, in rising order of effort.

## Point it at the right index

Two files describe this project to a machine, and they answer different
questions:

| | |
| --- | --- |
| [`abap2ui5.github.io/docs/llms.txt`](https://abap2ui5.github.io/docs/llms.txt) | the map of the **prose** — every chapter of this site with one line of what it covers, and [`llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) for all of it in one fetch |
| [`github.com/abap2UI5/abap2UI5/llms.txt`](https://github.com/abap2UI5/abap2UI5/blob/main/llms.txt) | the map of the **code** — the interface files to read instead of guessing at a signature, and the guide for building apps that ships with the framework |

Both are short and both are free to give an assistant that has web access. It
is the cheapest correction available: an agent that has read either one does
not reach for the frozen builder.

## Put the conventions in the repository

An index tells an agent what abap2UI5 is. `AGENTS.md` tells it what *your
project* is — and it is read automatically, by every session, without anybody
remembering to paste anything.

The [app-template](/get_started/project_setup) ships one written for
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

The [abap2UI5 linter](/technical/tools/linter) half is the one that matters
here: it reconstructs the view from the builder chain and reports the names UI5
does not have, the bindings that point at nothing — and a class still built on
the frozen builder.

## Give it the loop

[**abap2UI5/mcp-server**](https://github.com/abap2UI5/mcp-server) is an MCP server that
turns the checks into a development loop, still without a system. It works with
any MCP client — Claude Code, Cursor, VS Code:

```sh
claude mcp add abap2ui5 -- node /path/to/mcp-server/server.mjs
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
minutes. Stop where the value stops for you.

## From the editor

The [VS Code extension](/get_started/tooling#run-the-app-from-the-editor)
registers that same MCP server for every client in the window — Copilot agent
mode, Claude Code, anything else speaking MCP — so an agent working in your
editor has the loop without any separate configuration. Point
`abap2ui5.mcp.reposRoot` at the folder holding the checkouts and the extension
passes the paths through.

It adds a second server of its own for the half mcp-server deliberately does not
have: your configured **systems**. An agent can list them, search app classes
over ADT and get the app rendered on the real system as a screenshot — while
every credential prompt stays an ordinary VS Code dialog the agent never sees.

## Next Steps

- [Your Project](/get_started/project_setup) — the repository all of this
  assumes
- [Tooling](/get_started/tooling) — the same four tools, for a human
