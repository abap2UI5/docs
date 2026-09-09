---
outline: [2, 4]
description: Setting up an AI agent for abap2UI5 development, in rising order of effort - the conventions file it reads, the gates it can run without a system, the MCP server that gives it the build-and-look loop, and the editor.
---
# Agent Setup

[Developing with AI](/get_started/ai) is the one paragraph to paste and the
index to point an assistant at. This page is the rest: everything that turns
an assistant that knows abap2UI5 into one that can check its own work, in
rising order of effort. Start at the top; each level is useful on its own.

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
| `examples` | search the three sample catalogs — *has somebody already built a value help, a tree, navigation between two apps?* Answers with a class to read, never with a snippet to trust |
| `capabilities` | whether abap2UI5 can express a UI5 feature at all, from the verified capability map |
| `validate_view` | the linter's gates, in seconds, against your project's own config |
| `deploy_app` | write the class into a local sandbox and compile it |
| `build_backend` / `run_app` | transpile the framework and the app to Node, boot it headless, and hand back the errors **and a screenshot** |
| `pitfalls` | the defects a green run still does not catch — abapGit import, activation, the oldest UI5 release |

Set-up is leveled: validating views needs one small checkout and a minute;
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
- [MCP Server](/advanced/mcp_server) — the three levels, every tool and the loop
- [Tooling](/advanced/tooling) — the human side of the same loop
