---
outline: [2, 4]
description: What comes after the first app - extending the framework, connecting it to other systems, running it on older releases and other stacks, and the tools you develop with.
---
# Advanced Topics

Everything in this section is something you do to an app that already runs:
extend the framework underneath it, connect it to another system, take it to
a release or a stack it was not written on, or set up the machine you write
it on. None of it is needed for a first app — the
[Quickstart](/get_started/quickstart) and the [Cookbook](/cookbook/) come
first — and every group below stands on its own.

| Group | What it covers | Start with |
|---|---|---|
| Extensibility | User exits on the HTTP handler, the frontend artifacts as an app of your own, custom UI5 controls | [User Exits](/advanced/extensibility/user_exits) |
| Integration | Calling abap2UI5 apps on another system over RFC or HTTP, and an abap2UI5 app inside a Fiori Elements object page | [RFC Connector](/advanced/rfc) |
| Releases, Stacks | The downport to NetWeaver 7.02, renaming the `Z2UI5_` prefix, developing in a repository off the stack, the legacy-free UI5 runtime | [Downporting](/advanced/downporting) |
| Developer Setup | The tooling around the framework, the linter, the MCP server, the VS Code extension, and the setup for an AI agent | [Tooling](/advanced/tooling) |
| Toolchain | The open-source projects the framework is built with — abapGit, ajson, S-RTTI, abaplint, open-abap, abap-cleaner, abapmerge | [Toolchain](/technical/tools/) |
