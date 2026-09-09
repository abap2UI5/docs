---
outline: [2, 4]
description: The open-source projects abap2UI5 is built with - abapGit, ajson, S-RTTI, abaplint, open-abap, abap-cleaner, abapmerge - and what each one does for the framework.
---
# Toolchain

abap2UI5 is built with, and ships parts of, a handful of open-source ABAP
projects. Four of them have a page of their own; the rest are a paragraph each,
here.

| Project | What it does for abap2UI5 |
| --- | --- |
| [abapGit](/technical/tools/abapgit) | Version control, and the way the framework and every app are installed |
| [ajson](/technical/tools/ajson) | All JSON between the browser and the backend, from NetWeaver 7.02 to ABAP Cloud |
| [S-RTTI](/technical/tools/srtti) | Serializing app instances that hold data typed at runtime |
| [abaplint](/technical/tools/abaplint) | Static analysis on every pull request, the 7.02 downport and the namespace renaming |
| [open-abap](#open-abap) | Running ABAP without a system: the unit tests, the frontend tests, the playground |
| [abap-cleaner](#abap-cleaner) | The formatting of the framework's source |
| [abapmerge](#abapmerge-and-abap2ui5-local) | The single-file build, abap2UI5-local |

## open-abap

abap2UI5 uses [open-abap](https://github.com/open-abap) to run unit tests and
frontend tests, and to execute samples directly in your browser - with no
backend needed. It is what the [playground](https://abap2ui5.github.io/playground/)
and the [MCP server](/advanced/mcp_server) run the framework on.

## abap-cleaner

The [abap-cleaner](https://github.com/SAP/abap-cleaner) tool formats the
abap2UI5 source code. Use this
[profile](https://github.com/abap2UI5/abap2UI5/blob/main/.github/cleaner-profile.cfj)
to match the project standards.

## abapmerge and abap2UI5-local

The [abapmerge](https://github.com/larshp/abapmerge) tool creates the
[abap2UI5-local](https://github.com/abap2UI5/abap2UI5-local) build by merging
ABAP includes into a single file, bundling all abap2UI5 classes into one local
implementation of an HTTP handler.

abap2UI5-local is therefore a special build that bundles all framework classes
into a single HTTP handler class. Besides that, you only need to create two
additional database tables (`z2ui5_t_99` and `z2ui5_t_98` - separate from a
normal installation's, which is what keeps the two independent). This gives you
a self-contained copy of abap2UI5 that runs independently of any other
abap2UI5 installation on the same system - for a demo, a workshop, or an app
that has to carry its own framework version.

For full details, see the repository:
[abap2UI5-local](https://github.com/abap2UI5/abap2UI5-local)
