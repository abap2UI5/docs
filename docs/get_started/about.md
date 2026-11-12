---
outline: [2, 4]
description: The technical picture of abap2UI5 on one page — UI5 apps written purely in ABAP, how the roundtrip works, what it runs on, where an app can appear, and the tooling around it.
---
# In a Nutshell

**Build UI5 Apps Purely in ABAP**

abap2UI5 is an open-source framework that brings the simplicity of classic ABAP development to modern UI5 apps. Just as Selection Screens and ALV grids let you build working UIs with only a few lines of ABAP, abap2UI5 brings that same simplicity to modern web apps:

```abap
CLASS zcl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

That's it — your first UI5 app is ready. `client` is the single parameter of `main`, passed in by the framework. The [Hello World](/get_started/hello_world) page runs this same class in your system and explains it.

A finished app looks like any other UI5 app, because it is one — a selection screen, a table, a dialog, all of it built from the same single class:

![An abap2UI5 app: a selection form above a table of invoices, with a dialog editing one row](/tutorials/walkthrough-preview.webp)

→ *The [Tutorial](/tutorials/) grows exactly this app in twelve steps, each one a complete class you can run in the browser — then takes it to production, and under unit tests*

::: tip Next: install it
The [Quickstart](/get_started/quickstart) takes you from abapGit to a running
app in three steps. No system at hand? The
[playground](https://abap2ui5.github.io/playground/){target="_self"} runs the
same class in your browser.
:::

This page is the technical picture: what an app is, how one click travels, and
what it runs on. What abap2UI5 means for an organization — what it can build,
how it compares, what it costs — is on
[abap2UI5 for decision makers](https://abap2ui5.github.io/why/).

## How It Works

The frontend is a UI5 shell that only renders. Your ABAP class builds a UI5 XML view, the framework sends it to the browser in one response with the JSON model its controls bind to, and every user interaction comes back as a fresh call into `main( )`. No OData service sits in between.

One click is one **roundtrip**: the browser sends a request, `main( )` runs once, the answer comes back. No ABAP session waits between two clicks. After each roundtrip the framework saves your app object on the server, and the browser keeps only the id of that copy. Before the next call of `main( )` the copy is restored, so your app is exactly as the user left it. The saved copy is called the app **draft** (it is not a RAP draft). This makes an app stateless like any other UI5 app, which is what makes it scale. Every word is in the [Glossary](/get_started/glossary). For the few cases that need a pinned session, a [stateful mode](/cookbook/expert_more/statefulness) exists.

![One roundtrip in five steps: the draft is loaded, the user's changes are applied, main( ) runs, the response is built and the draft is saved under a new id](/get_started/roundtrip.svg)

→ *The [Insights](https://abap2ui5.github.io/insights/07-one-click-one-request/) take each of the five steps apart, and compare the result with [RAP](https://abap2ui5.github.io/insights/36-rap-or-abap2ui5/) and [UI5 freestyle](https://abap2ui5.github.io/insights/37-freestyle-or-abap2ui5/)*

Outside the SAP world this pattern has a name — *HTML Over-the-Wire*, the idea behind htmx, Hotwire and Phoenix LiveView. abap2UI5 applies it to UI5.


## What Follows from That

- **A thin frontend.** The browser only renders. Logic, data and state stay in
  the backend, so there is nothing to configure on the client and no cache to
  clear.
- **One request per click.** The data comes in the same response as the view:
  no OData call per control. → [Performance](/configuration/performance)
- **No service to query from outside.** The browser gets only what the app puts
  into the view. Logon is the job of the ICF node, as for any service.
  Authorization checks stay yours, in the app.
  → [Security](/configuration/security),
  [Authorization](/configuration/authorization)
- **A small footprint.** Classes, interfaces and one table for the app drafts.
  Everything else is an optional [add-on](/resources/addons).

## Compatibility

abap2UI5 uses released APIs only, so the same source runs on ABAP Cloud and on Standard ABAP; CDS, ABAP SQL and EML are available inside an app where the system has them.

### System Support
Works with both ABAP Cloud and Standard ABAP:
- S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
- S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
- R/3 NetWeaver AS ABAP 7.02 or later (Standard ABAP)

Below 7.50 you pull the `X.Y.Z-702` tag instead of `X.Y.Z` — same features, generated
from `main` automatically; the [Quickstart](/get_started/quickstart#_1-install-the-framework-via-abapgit) table says which tag belongs to which system. See [Downporting](/advanced/downporting).

### UI5 Versions
The frontend is UI5 itself, loaded at bootstrap like it is for any other UI5 app. abap2UI5 bootstraps OpenUI5 from its public CDN by default; a single exit points it at SAPUI5, at a pinned version, or at the UI5 already delivered by your own system — which is what a system without internet access uses. `1.71` is the oldest supported release, and every change is tested against UI5 2.x as well.

→ *See [UI5 Versions](/configuration/ui5_versions) for the distributions, and [Bootstrapping](/configuration/setup/ui5_bootstrapping) for choosing the source*

## Integration

### Where an App Appears
An app runs in a browser tab on its own, or embedded into:
- Fiori Launchpads on S/4 On-Premise
- Tiles on S/4 Public Cloud
- Build Work Zone Websites on BTP
- Any UI5 app of your own, a Fiori elements page or a UI Integration Card, with the [embed control](https://github.com/abap2UI5/embed-control) (`@abap2ui5/embed-control` on npm)
- A tab in Microsoft Teams, the task pane of Microsoft Excel, or a native app on iOS and Android

→ *See [Integration](/integration/) for every place an app can appear*

## Later

None of this is needed for a first app; each line is a page of its own.

- **Tooling.** A [linter](/advanced/linter), an
  [MCP server](/advanced/mcp_server) and a
  [VS Code extension](/advanced/vscode) — see [Tooling](/advanced/tooling) and
  [Developing with AI](/get_started/ai).
- **Without an SAP system.** Every release is also published transpiled to
  JavaScript — see [Working Off-Stack](/advanced/working_off_stack).
- **Help and community.** [Support](/resources/support) on GitHub and Slack,
  the [Contribution Guide](/resources/contribution), and
  [sponsorship](/resources/sponsor) if the project saves you time.
