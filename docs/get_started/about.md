---
outline: [2, 4]
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

### About
Since launching in 2023, abap2UI5 has grown from a small side project into a community-driven framework used by ABAP developers worldwide. The framework takes care of frontend complexity, so you can focus on business logic with your existing ABAP skills.

→ *See the [Getting Started Guide](/get_started/quickstart) for step-by-step setup*

→ *Browse [Sample Apps](/get_started/samples) to see abap2UI5 in action*

### Why abap2UI5?

Traditional UI5 development needs JavaScript expertise, frontend deployment, and OData service configuration. abap2UI5 cuts out those complexities:

- **Use your existing ABAP skills** — do what you know best; no frontend expertise needed
- **Broad compatibility** — build apps that run on legacy R/3 systems and modern S/4 Cloud environments
- **Extend beyond RAP** — when standard capabilities reach their limits
- **Prototype fast** — iterate quickly on business apps

Each app ships as an [abapGit](https://abapgit.org) project, so installation across systems needs no separate frontend deployment.

### Overview
<br>

#### Architecture
abap2UI5 takes a "thin frontend" approach — all processing, logic, and data handling stay in the backend. This design simplifies configuration, cuts client-side complexity (no more cache-clearing headaches), and keeps business logic and sensitive data safely on the server.

#### Performance
abap2UI5 is fast. The frontend focuses only on UI rendering via the UI5 framework, while the ABAP backend handles all processing. Unlike traditional UI5 apps that need separate OData calls for each view, abap2UI5 embeds data directly in XML views — cutting network roundtrips and speeding up the response.

#### Security
abap2UI5 is secure by design. All business logic stays in the ABAP backend. The frontend receives only the data the backend embeds directly in XML views. Unlike traditional UI5 apps that expose OData endpoints, abap2UI5 delivers only what users need — no access to raw services or database queries from external tools.

#### System Footprint
The framework has a small system footprint — only classes and interfaces. Add functionality with optional add-ons to keep your system lean.

#### Transparency
All source code lives publicly on GitHub. We discuss features and issues openly, publish technical blog posts that explain key concepts, and ship fixes quickly via abapGit. You'll always understand how the framework works and can confirm its behavior.

### Compatibility
<br>

#### ABAP Cloud
abap2UI5 uses only released APIs, making it a strong fit for on-stack and side-by-side extensions in ABAP for Cloud. Use ABAP syntax features like CDS, ABAP SQL, and EML inside your apps.

#### Clean Core
By relying only on released APIs, abap2UI5 keeps your apps "cloud-ready" and "upgrade-stable," in line with SAP's clean-core principles. Your investment in abap2UI5 apps stays safe through future SAP system upgrades.

#### System Support
Works with both ABAP Cloud and Standard ABAP, covering ABAP releases from 7.02 to ABAP Cloud:
- S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
- S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
- R/3 NetWeaver AS ABAP 7.02 or later (Standard ABAP)

A downported version is available for systems running earlier ABAP releases (before 7.50).

### Enterprise Ready

abap2UI5 combines SAP's UI5 framework with ABAP's backend capabilities, tailored for enterprise SAP environments. It runs smoothly across S/4HANA Public/Private Cloud, BTP ABAP Environment, and NetWeaver systems.

#### Production Usage
Use abap2UI5 like any other UI5 app or ABAP program in production. Add the framework and your apps to a transport request to ship them.

#### Licensing
Technically, abap2UI5 apps are standard UI5 freestyle apps. License them the same way you license other UI5 apps at your organization. abap2UI5 itself is MIT licensed (free for commercial use).

#### Launchpad Integration
Integrate your apps into:
- Fiori Launchpads on S/4 On-Premise
- Tiles on S/4 Public Cloud
- Build Work Zone Websites on BTP

#### Installation
Getting started is easy:
1. Import the project via abapGit
2. Set up an HTTP service for browser communication
3. Start building!

→ *See the [Installation Guide](/get_started/quickstart) for full instructions*

### Community
<br>

#### Support
The community offers support. Open an issue on GitHub or join the abap2UI5 Slack channel to get help.

→ *See the [Support page](/resources/support) for more options*

#### Contribution
Contributions are always welcome. Whether you fix bugs, build features, or improve the docs, every contribution helps the project thrive.

→ *See the [Contribution Guide](/resources/contribution) to learn how to get involved*

#### Sponsor
Volunteers maintain abap2UI5. If you or your company benefits from the project, please consider sponsoring it.

→ *Read more about [sponsorship opportunities](/resources/sponsor)*
