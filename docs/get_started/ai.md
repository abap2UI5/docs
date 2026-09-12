---
outline: [2, 4]
description: Developing abap2UI5 apps with an AI assistant - one paragraph to paste into a prompt, and the linter and MCP server that let an agent check its own work without a system.
---
# Developing with AI

abap2UI5 is well suited to being written by an AI assistant, and the
reason is structural:

- **An app is one ABAP class — source code, and nothing else.** No service to
  generate, no OData artifacts, no frontend project, no manifest, no
  deployment pipeline. There is exactly one file for an agent to write, and
  the thing it writes is the thing that runs.
- **The whole app is text.** View, logic, state and data flow live in the same
  class, in one language. An agent never has to keep an ABAP backend and a
  JavaScript frontend in step, because there is no second half to drift.
- **There are over 700 working examples to learn from.** The
  [sample catalog](https://abap2ui5.github.io/playground/samples/) holds
  complete, tested apps from all three repositories — one per pattern, every
  one linted and rendered, each with the ABAP printed in full — so "has
  somebody already built a value help, a tree, navigation between two apps?"
  is a question with a real answer instead of a guess.
- **The result can be checked without an SAP system.** The
  [abap2UI5 linter](/advanced/linter) reconstructs the UI5 view out of the ABAP
  that builds it and reports what UI5 does not have. An agent that can verify
  its own work stops handing you apps that do not render.

Two things turn those properties into a working session, and both cost a
minute: a paragraph to paste, and an index to point at. The rest — the
conventions file, the gates, the MCP loop, the editor — is
[Agent Setup](/advanced/agent_setup), in rising order of effort.

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
   which dispatches on client->check_on_navigated( ) (the display branch, true on
   first start too), client->check_on_event( `X` ) and - for one-time setup only -
   client->check_on_init( ).
2. Build the view with z2ui5_cl_ui5_view_builder and its verbs ele / tag / a / end /
   stringify.
3. Bind with client->_bind( ). It is bidirectional; only what the user edited comes back.
4. Every roundtrip is a fresh ABAP session. Nothing survives on the server except
   the app class itself, which is serialized.

Before building something from scratch, check whether it exists: the sample
catalog's index at https://abap2ui5.github.io/playground/samples/apps.json lists
every sample of all three sample repositories with title, summary and keywords,
and https://abap2ui5.github.io/playground/samples/<class>/ prints each one's ABAP.

When you are done, check the result with the abap2UI5 linter
(npx @abap2ui5/linter src) - it reads the view your ABAP builds and needs no
SAP system. If something is not covered by those files, say so instead of
inventing it.
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

## Next Steps

- [Agent Setup](/advanced/agent_setup) — the conventions file, the gates, the
  MCP loop and the editor, in rising order of effort
- [Working Off-Stack](/advanced/working_off_stack) — the repository all of this
  assumes
- [Tooling](/advanced/tooling) — the human side of the same loop
