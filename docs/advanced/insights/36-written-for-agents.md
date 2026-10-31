---
description: Why a one-class, one-language app is the shape an AI coding agent handles best, and where the setup - the published page map, the linter, the MCP server, the sample catalogs - is documented.
---
# #36 Written for Agents

The property this series kept returning to — an app is one class, and the
class is text — has a consequence nobody was designing for in 2023. An agent
writing a Fiori Elements app has to keep a CDS view, its annotations, a
behavior definition, a service binding and a generated frontend in step, and
can verify none of it without a system. An agent writing a freestyle app holds
an ABAP backend and a JavaScript frontend at once and keeps the contract
between them true. An agent writing an abap2UI5 app writes one file, in one
language — the shape of [#14](/advanced/insights/14-a-classrun-for-the-browser)
— and the thing it writes is the thing that runs.

What turns that into a working setup is three things around the framework,
none of them a feature of it: a published [`llms.txt`](https://abap2ui5.github.io/docs/llms.txt)
so an agent works from what abap2UI5 is today rather than from training data
where the view builder still has a method per control; the
[linter](/advanced/linter), which reconstructs the UI5 view out of the ABAP
that builds it and reports what UI5 does not have, so an agent can check its
own work without a system; and the sample catalogs, a complete tested app per
pattern. The setup itself — the AI Studio in the browser, the paragraph to
paste into a prompt, the MCP server, the agent skills — is on
[Developing with AI](/get_started/ai), and an app that an agent *operates*
rather than writes is [Agent-Operable Apps](/advanced/agents).
