---
outline: [2, 3]
---
# Introduction

**Build UI5 Apps Purely in ABAP**

abap2UI5 is an open-source framework that brings the simplicity of classic ABAP development to modern UI5 apps. Just as Selection Screens and ALVs once produced working UIs with minimal code, abap2UI5 lets you build web apps with a few lines of ABAP:

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

That's it — your first UI5 app is ready.

## About

Since launching in 2023, abap2UI5 has grown from a small side project into a community-driven framework used by ABAP developers worldwide. The framework absorbs frontend complexity, so you can focus on business logic with your existing ABAP skills.

→ *See the [Getting Started Guide](/get_started/quickstart) for step-by-step setup*

→ *See [Sample Apps](/get_started/samples) to watch abap2UI5 in action*

## Why abap2UI5?

Traditional UI5 development needs JavaScript expertise, frontend deployment, and OData service setup. abap2UI5 cuts out those complexities:

- **Use your existing ABAP skills** — do what you do best; no frontend expertise needed
- **Broad compatibility** — build apps that run on legacy R/3 systems and modern S/4 Cloud environments
- **Extend beyond RAP** — when standard capabilities reach their limits
- **Prototype fast** — iterate rapidly on business apps

Each app ships as an [abapGit](https://abapgit.org) project, so installation across systems needs no separate frontend deployment.

## Overview

### Architecture
abap2UI5 takes a "thin frontend" approach: all processing, logic, and data handling stay in the backend. Cache-clearing on the client becomes irrelevant, and business logic and sensitive data stay on the server.

### Performance
The frontend renders UI5 controls; the ABAP backend handles everything else. View data is embedded directly in the XML view, so a roundtrip means one HTTP request — not one per OData entity.

### Security
Business logic stays in the ABAP backend. The frontend only receives what the backend chooses to embed in the rendered view. There is no public OData endpoint to discover or query.

### System Footprint
The framework consists of classes and interfaces only — no database tables, no metadata. Optional add-ons cover specialized needs.

### Transparency
All source code lives on GitHub. Issues and features are discussed openly, and fixes ship via abapGit.

## Compatibility

### ABAP Cloud
abap2UI5 uses only released APIs, making it a strong fit for on-stack and side-by-side extensions on ABAP for Cloud. Use ABAP syntax features like CDS, ABAP SQL, and EML inside your apps.

### Clean Core
By relying only on released APIs, abap2UI5 keeps your apps "cloud-ready" and "upgrade-stable," in line with SAP's clean-core principles. Your investment in abap2UI5 apps stays safe through future SAP system upgrades.

### System Support
Works with both ABAP Cloud and Standard ABAP, covering ABAP releases from 7.02 to ABAP Cloud:
- S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
- S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
- R/3 NetWeaver AS ABAP 7.02 or later (Standard ABAP)

A downported version is available for systems on earlier ABAP releases (before 7.50).

## In Production

abap2UI5 runs on S/4HANA Public and Private Cloud, BTP ABAP Environment, and NetWeaver systems.

### Production Usage
Treat abap2UI5 like any other UI5 app or ABAP program: include the framework and your apps in a transport request and ship.

### Licensing
abap2UI5 apps are standard UI5 freestyle apps. License them like any other UI5 app at your organization. abap2UI5 itself is MIT licensed (free for commercial use).

### Launchpad Integration
Embed your apps into:
- Fiori Launchpads on S/4 On-Premise
- Tiles on S/4 Public Cloud
- Build Work Zone Websites on BTP

### Installation
Three steps:
1. Import the project via abapGit.
2. Set up an HTTP service for browser communication.
3. Start building.

→ *See the [Quickstart](/get_started/quickstart) for full instructions.*

## Community

### Support
The community offers support. Open an issue on GitHub or join the abap2UI5 Slack channel to get help.

→ *See the [Support page](/resources/support) for more options*

### Contribution
Contributions are always welcome. Whether you fix bugs, build features, or improve the docs, every contribution helps the project thrive.

→ *See the [Contribution Guide](/resources/contribution) to learn how to get involved*

### Sponsor
Volunteers maintain abap2UI5. If you or your company benefits from the project, please consider sponsoring it.

→ *Read more about [sponsorship opportunities](/resources/sponsor)*
