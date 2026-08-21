---
outline: [2, 4]
description: What abap2UI5 is, on one page — UI5 apps written purely in ABAP, how the framework works, what it runs on, and where it fits.
---
# In a Nutshell

**Build UI5 Apps Purely in ABAP**

abap2UI5 is an open-source framework that brings the simplicity of classic ABAP development to modern UI5 apps. Just as Selection Screens and ALV grids let you build working UIs with only a few lines of ABAP, abap2UI5 brings that same simplicity to modern web apps:

```abap
CLASS zcl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( `Hello World` ).
  ENDMETHOD.
ENDCLASS.
```

That's it — your first UI5 app is ready. (`client` is the single parameter of `main`, passed in by the framework — explained on the [Hello World](/get_started/hello_world) page.)

A finished app looks like any other UI5 app, because it is one — a selection screen, a table, a dialog, all of it built from the same single class:

![An abap2UI5 app: a selection form above a table of invoices, with a dialog editing one row](/tutorials/walkthrough-preview.png)

→ *The [Tutorial](/tutorials/overview) grows exactly this app in ten steps, each one a complete class you can run in the browser*

## Background
Since launching in 2023, abap2UI5 has grown from a small side project into a community-driven framework used by ABAP developers worldwide. The framework absorbs frontend complexity, so you can focus on business logic with your existing ABAP skills.

→ *See the [Getting Started Guide](/get_started/quickstart) for step-by-step setup*

→ *See [Sample Apps](/get_started/next#sample-apps) to watch abap2UI5 in action*

## Why abap2UI5?

Traditional UI5 development needs JavaScript expertise, frontend deployment, and OData service setup. abap2UI5 cuts out those complexities:

- **Use your existing ABAP skills** — do what you do best; no frontend expertise needed
- **Broad compatibility** — build apps that run on legacy R/3 systems and modern S/4 Cloud environments
- **Extend beyond RAP** — build UIs for cases RAP does not cover, such as free-style screens, custom flows, or non-CDS data
- **Prototype fast** — iterate rapidly on business apps

Each app ships as an [abapGit](https://abapgit.org) project, so installation across systems needs no separate frontend deployment.

## How It Works

The frontend is a UI5 shell that only renders. Your ABAP class builds a UI5 XML view, the framework sends it to the browser with the data already embedded in it, and every user interaction comes back as a fresh call into `main( )`. No OData service sits in between.

Nothing survives on the server between two clicks, and that is deliberate: `z2ui5_if_app` inherits `if_serializable_object`, so the app's state travels with the roundtrip and is restored before `main( )` runs again. Each request is a new ABAP session holding your app exactly as the user left it — stateless like any other UI5 app, which is what makes it scale. For the few cases that need a pinned session — a classic enqueue, an open RFC connection — a [stateful mode](/cookbook/expert_more/statefulness) exists.

Outside the SAP world this pattern has a name — *HTML Over-the-Wire*, the idea behind htmx, Hotwire and Phoenix LiveView. abap2UI5 applies it to UI5.

→ *See [UI5 Over-the-Wire](/technical/concept) for the architecture, and [Behind the Scenes](/technical/how_it_all_works) for what happens on a single request*

## Overview
<br>

### Architecture
abap2UI5 takes a "thin frontend" approach — all processing, logic, and data handling stay in the backend. This design simplifies configuration, cuts client-side complexity (no more cache-clearing headaches), and keeps business logic and sensitive data safely on the server.

### Performance
abap2UI5 is fast. The frontend focuses only on UI rendering via the UI5 framework, while the ABAP backend handles all processing. Unlike traditional UI5 apps that need separate OData calls for each view, abap2UI5 embeds data directly in XML views — cutting network roundtrips and speeding up the response.

→ *See [Performance](/configuration/performance) for what to measure and what to tune*

### Security
abap2UI5 is secure by design. All business logic stays in the ABAP backend. The frontend receives only the data the backend embeds directly in XML views. Unlike traditional UI5 apps that expose OData endpoints, abap2UI5 delivers only what users need — no access to raw services or database queries from external tools.

Authentication is the ICF node's job, exactly as for any other service on your system, and the framework ships a Content-Security-Policy by default. Authorization stays yours: whatever the user sends still arrives from a browser, so check it in the app or on the service node, as you would in any other program.

→ *See [Security](/configuration/security) for the endpoint, the CSP and what the framework does not do for you, and [Authorization](/configuration/authorization) for setting up checks*

### System Footprint
The framework has a small system footprint — essentially classes, interfaces, and a single draft table. The core stays minimal; optional add-ons provide extra functionality only when you need it.

→ *See [Add-ons](/advanced/addons) for the ecosystem around the core — layout variants, table maintenance, charts*

### Transparency
All source code lives publicly on GitHub. We discuss features and issues openly, publish technical blog posts that explain key concepts, and ship fixes quickly via abapGit. You'll always understand how the framework works and can confirm its behavior.

## Where It Fits

abap2UI5 is built for business apps: worklists, forms, CRUD, dashboards, selection screens — the kind of thing you would otherwise write as a Selection Screen, an ALV report or a freestyle UI5 app.

It is deliberately not built for everything. Three cases are a poor fit, and knowing them up front saves a prototype:

- **Heavily interactive or real-time collaborative apps.** Every interaction that needs the server is a roundtrip.
- **Offline use and complex client-side behaviour.** The browser renders; it does not hold the application.
- **Teams that work strictly separately.** The app is one ABAP class — that is the point, and it means there is no frontend project for a frontend team to own.

→ *See [Use Cases](/advanced/use_cases) for the on-stack, side-by-side and SaaS scenarios, and [UI5 Over-the-Wire](/technical/concept) for the reasoning behind the boundary*

## Compatibility
<br>

### ABAP Cloud
abap2UI5 uses only released APIs, making it a strong fit for on-stack and side-by-side extensions on ABAP for Cloud. Use ABAP syntax features like CDS, ABAP SQL, and EML inside your apps.

### Clean Core
By relying only on released APIs, abap2UI5 keeps your apps "cloud-ready" and "upgrade-stable," in line with SAP's clean-core principles. Your investment in abap2UI5 apps stays safe through future SAP system upgrades.

### System Support
Works with both ABAP Cloud and Standard ABAP:
- S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
- S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
- R/3 NetWeaver AS ABAP 7.50 or later (Standard ABAP)

For systems on releases before 7.50 (down to 7.02), a separate downported version is available.

### UI5 Versions
The frontend is UI5 itself, loaded at bootstrap like it is for any other UI5 app. abap2UI5 bootstraps OpenUI5 from its public CDN by default; a single exit points it at SAPUI5, at a pinned version, or at the UI5 already delivered by your own system — which is what a system without internet access uses. `1.71` is the oldest supported release, and every change is tested against UI5 2.x as well.

→ *See [UI5 Versions](/configuration/ui5_versions) for the distributions, and [Bootstrapping](/configuration/setup/ui5_bootstrapping) for choosing the source*

## Enterprise Ready

abap2UI5 combines SAP's UI5 framework with ABAP's backend capabilities, shaped for enterprise SAP environments. It runs smoothly across S/4HANA Public/Private Cloud, BTP ABAP Environment, and NetWeaver systems.

### Production Usage
Use abap2UI5 like any other UI5 app or ABAP program in production. Add the framework and your apps to a transport request to ship them. Updates ship as versioned releases with every change listed: see the [Release Notes](/resources/changelog), and the [Deprecations](/resources/deprecations) page for what is on its way out and what replaces it.

### Licensing
Technically, abap2UI5 apps are standard UI5 freestyle apps. License them the same way you license other UI5 apps at your organization. abap2UI5 itself is MIT licensed (free for commercial use).

### Launchpad Integration
Embed your apps into:
- Fiori Launchpads on S/4 On-Premise
- Tiles on S/4 Public Cloud
- Build Work Zone Websites on BTP

### Installation
Getting started is easy:
1. Import the project via abapGit
2. Set up an HTTP service for browser communication
3. Start building!

No system at hand? The Quickstart opens with two ways to try abap2UI5 in the
browser — the playground and the live demo — before anything is installed.

→ *See the [Quickstart Guide](/get_started/quickstart) for full instructions*

## Tooling

None of it is required — an app is one class in your system, and ADT is enough. What exists is worth the ten minutes: a [linter](/advanced/linter) that reconstructs the UI5 view out of the ABAP that builds it and reports what UI5 does not have, without an SAP system; a [VS Code extension](/advanced/vscode) that runs the app next to the code on `F9`; and an [MCP server](/advanced/mcp_server) that lets an AI assistant build an app *and look at the result*.

That last point is not a novelty: an app is a single ABAP class, in one language, that an assistant can also verify — which makes abap2UI5 unusually well suited to being written with one.

→ *See [Tooling](/get_started/tooling) for the setup, and [Developing with AI](/get_started/ai) for the assistant workflow*

## Community
<br>

### Support
The community offers support. Open an issue on GitHub or join the abap2UI5 Slack channel to get help.

→ *See the [Support page](/resources/support) for more options*

### Contribution
Contributions are always welcome. Whether you fix bugs, build features, or improve the docs, every contribution helps the project thrive.

→ *See the [Contribution Guide](/resources/contribution) to learn how to get involved*

### Sponsor
Volunteers maintain abap2UI5. If you or your company benefits from the project, please consider sponsoring it.

→ *Read more about [sponsorship opportunities](/resources/sponsor)*

### In Production Elsewhere
Companies, workshops and open-source projects already run on abap2UI5, with the system release and the use case named.

→ *See [Who Uses abap2UI5?](/resources/who_uses) — and add your own scenario*
