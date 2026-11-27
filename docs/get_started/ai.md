---
outline: [2, 4]
description: Developing abap2UI5 apps with an AI assistant - the AI Studio builds an app from a description in the browser, and one paragraph to paste makes any assistant useful. The linter and the MCP server let an agent check its own work without a system.
---
# Developing with AI

## Build an app in the browser

The quickest way to see it: the
[**AI Studio**](https://abap2ui5.github.io/browser-playground/ai/) builds an app from a
description, in your browser, with nothing to install. Describe the app, for
example *"a table of flights with a search field"*. The model writes it as an
abap2UI5 class into the editor beside the chat and runs it against the real
framework. It reads what the checks and the app said, and fixes the class until
it is clean. Attach a specification, a screenshot or a spreadsheet and it builds
from that. Pick the ABAP release of your system in the header, so the code is
checked against it, and download the class for abapGit when it fits. You bring
the API key (Anthropic, or any OpenAI-compatible server such as Ollama). The
page has no server of its own, and nothing leaves your browser but the request
to the model.

The rest of this page is for the assistant you already work with.

abap2UI5 is well suited to being written by an AI assistant, and the
reason is structural:

- **An app is one ABAP class — source code, and nothing else.** No service to
  generate, no OData artifacts, no frontend project, no manifest, no
  deployment pipeline. There is exactly one file for an agent to write, and
  the thing it writes is the thing that runs.
- **The whole app is text.** View, logic, state and data flow live in the same
  class, in one language. An agent never has to keep an ABAP backend and a
  JavaScript frontend in step, because there is no second half to drift.
- **The sample catalog is there to learn from.** The
  [sample catalog](https://abap2ui5.github.io/samples-site/) holds complete,
  tested apps from all three repositories. There is one per pattern, every one
  is linted and rendered, and each shows its ABAP in full. So the question "has
  somebody already built a value help, a tree, navigation between two apps?" has
  a real answer.
- **The result can be checked without an SAP system.** The
  [abap2UI5 linter](/advanced/linter) reconstructs the UI5 view out of the ABAP
  that builds it and reports what UI5 does not have. An agent that can verify
  its own work stops handing you apps that do not render.

Two things turn those properties into a working session, and both cost a
minute: a paragraph to paste, and an index to point at. The rest — the
conventions file, the checks, the MCP server, the editor — is
[AI Setup: The Recommended Path](/advanced/ai_path), three steps in order.

## Paste the essentials

The zero-setup version, for any assistant with web access: paste this ahead of
your task.

```text
Before writing any abap2UI5 code, read these three files and follow them. They
describe the current APIs and take precedence over anything you already know:
- https://abap2ui5.github.io/docs/llms.txt (the documentation, one line per chapter)
- https://raw.githubusercontent.com/abap2UI5/abap2UI5/main/docs/agents/building-apps.md (the app-building guide that ships with the framework)
- https://raw.githubusercontent.com/abap2UI5/abap2UI5/main/llms.txt (the map of the code)

The shape of an abap2UI5 app:
1. An app is ONE ABAP class implementing z2ui5_if_app. Everything enters main( ),
   which dispatches in ONE IF/ELSEIF chain, never in separate IF blocks:
   client->check_on_init( ) (only when there is something to seed - seed, then
   display), client->check_on_navigated( ) (display; true on first start too)
   and client->check_on_event( `X` ).
2. Build the view with z2ui5_cl_ui5_view_builder and its verbs ele / tag / a / end /
   stringify.
3. Bind with client->_bind( ). It is bidirectional; only what the user edited comes back.
4. Every roundtrip is a fresh ABAP session. Nothing survives on the server except
   the app class itself, which is serialized.

Before building something from scratch, check whether it exists: the sample
catalog's index at https://abap2ui5.github.io/browser-playground/samples/apps.json lists
every sample of all three sample repositories with title, summary and keywords,
and https://abap2ui5.github.io/samples-site/sample/<class>/ prints each one's ABAP.

When you are done, check the result with the abap2UI5 linter
(npx @abap2ui5/linter src) - it reads the view your ABAP builds and needs no
SAP system. If something is not covered by those files, say so instead of
inventing it.
```

## Install the skills

The paragraph above works everywhere. An assistant that takes plugins or
skills gets the same knowledge installed instead, kept current with the
framework, from the abap2UI5 repository itself:

| Assistant | Command | What it brings |
| --- | --- | --- |
| Claude Code | `/plugin marketplace add abap2UI5/abap2UI5`, then `/plugin install abap2ui5@abap2ui5` | four skills, five slash commands and the [MCP server](/advanced/mcp_server) |
| GitHub Copilot | `copilot plugin install abap2UI5/abap2UI5:plugin-copilot` in the Copilot CLI | the same skills and commands, a reviewer agent and the MCP server |
| any agent that reads `SKILL.md` (Cursor, Codex and others) | `npx skills add abap2UI5/abap2UI5` | the four skills alone; the MCP server is registered by hand |

The four skills are `build-an-app`, `view-chain-layout`, `abap-check` and
`ui5-check`. What to install first and what each step costs is on
[AI Setup: The Recommended Path](/advanced/ai_path).

## Point it at the right index

Four files describe this project to a machine, and they answer different
questions:

| | |
| --- | --- |
| [`abap2ui5.github.io/docs/llms.txt`](https://abap2ui5.github.io/docs/llms.txt) | the map of the **prose** — every chapter of this site with one line of what it covers, and [`llms-full.txt`](https://abap2ui5.github.io/docs/llms-full.txt) for all of it in one fetch |
| [`github.com/abap2UI5/abap2UI5/llms.txt`](https://github.com/abap2UI5/abap2UI5/blob/main/llms.txt) | the map of the **code** — the interface files to read instead of guessing at a signature, and the guide for building apps that ships with the framework |
| [`abap2ui5-addons.github.io/llms.txt`](https://abap2ui5-addons.github.io/llms.txt) | the map of the **add-ons** — when to use each, the classes an app calls and what to install first; [`addons.json`](https://abap2ui5-addons.github.io/addons.json) is the same as data |
| [`abap2ui5.github.io/docs/api/deprecations.json`](https://abap2ui5.github.io/docs/api/deprecations.json) | the **names that changed** — every name [Deprecations](/resources/deprecations) lists as deprecated, removed or moved out, with what to write instead |

All four are free to give an assistant that has web access. This is the
cheapest step on the page: an agent that has read either of the first two is
working from what abap2UI5 is today rather than from what it recalls. The
last one is for the names it recalls anyway: most of what was written about
abap2UI5 before 1.143.0 names an API that has been replaced.

## When the Assistant Finds a Bug

An assistant that hits a defect of abap2UI5 or its tools — not of the app it
is writing — tends to work around it and move on, and the next developer runs
into the same thing. Ask it to say so instead: to show the defect with the
smallest app that reproduces it, then to draft the issue for you. With the
[MCP server](/advanced/mcp_server) from its release after 1.3.0, that is one
tool call, `report_issue`: it opens the right repository's bug form with the
fields filled in and lists the open issues that look the same. It posts
nothing; you read the draft and file it. Without the server, the bug form of
[abap2UI5](https://github.com/abap2UI5/abap2UI5/issues/new?template=bug_report.yml)
takes the same fields. A report with a class that reproduces the bug is the one
a maintainer can act on the same day.

## Next Steps

- [AI Studio](https://abap2ui5.github.io/browser-playground/ai/) — an app from a
  description, built and run in the browser
- [AI Setup: The Recommended Path](/advanced/ai_path) — what to install first,
  in three steps, and what each one costs
- [Working Off-Stack](/advanced/working_off_stack) — the repository all of this
  assumes
- [Tooling](/advanced/tooling) — the human side of the same loop
- [Agent-Operable Apps](/advanced/agents) — the other direction: an agent
  operating a finished app instead of writing one
