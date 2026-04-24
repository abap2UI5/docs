---
outline: [2, 4]
---
# Introduction

**Build UI5 Apps Purely in ABAP**

abap2UI5 is an open-source framework that brings the simplicity of classic ABAP development to modern UI5 applications. Just as Selection Screens and ALVs could generate functional UIs with minimal code, abap2UI5 lets you build web applications with a few lines of ABAP:

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

That's it — your first UI5 app is ready!

### About
Since launching in 2023, abap2UI5 has grown from a small side project into a community-driven framework trusted by ABAP developers worldwide. The framework handles all the frontend complexity, letting you focus on business logic with your existing ABAP skills.

→ *See our [Getting Started Guide](/get_started/quickstart) for step-by-step instructions*

→ *Browse [Sample Apps](/get_started/samples) to see abap2UI5 in action*

### Why abap2UI5?

Traditional UI5 development requires JavaScript expertise, frontend deployment, and OData service configuration. abap2UI5 removes these complexities:

- **Use your existing ABAP skills** — do what you know best; no frontend expertise required
- **Universal compatibility** — build apps that run on legacy R/3 systems and modern S/4 Cloud environments
- **Extend beyond RAP** — when standard capabilities reach their limits
- **Prototype rapidly** — iterate quickly on new business applications

Each application ships as an [abapGit](https://abapgit.org) project, so installation across systems needs no separate frontend deployments.

### Overview
<br>

#### Architecture
abap2UI5 takes a "thin frontend" approach — all processing, logic, and data management stays in the backend. This design simplifies configuration, reduces client-side complexity (say goodbye to cache-clearing hassles), and keeps business logic and sensitive data securely on the server.

#### Performance
abap2UI5 is fast. The frontend focuses only on UI rendering via the UI5 framework, while the ABAP backend handles all processing. Unlike traditional UI5 apps that need separate OData calls for each view, abap2UI5 embeds data directly in XML views — cutting network roundtrips and speeding up response times.

#### Security
abap2UI5 is secure by design. All business logic stays in the ABAP backend. The frontend receives only the data the backend populates directly in XML views. Unlike traditional UI5 apps that expose OData endpoints, abap2UI5 delivers only what users need — no access to raw services or database queries via external tools.

#### System Footprint
The framework has a minimal system footprint — only classes and interfaces. Add extra functionality via optional add-ons to keep your system lean.

#### Transparency
All source code lives publicly on GitHub. We discuss features and issues openly, publish technical blog posts explaining key concepts, and rapidly deploy fixes through abapGit. You'll always understand how the framework works and can verify its behavior.

### Compatibility
<br>

#### ABAP Cloud
abap2UI5 uses only released APIs, making it a great fit for both on-stack and side-by-side extensions in ABAP for Cloud. Use modern ABAP syntax features like CDS, ABAP SQL, and EML within your apps.

#### Clean Core
By relying only on released APIs, abap2UI5 keeps your applications "cloud-ready" and "upgrade-stable," aligned with SAP's clean-core principles. Your investment in abap2UI5 apps stays safe across future SAP system upgrades.

#### System Support
Works with both ABAP Cloud and Standard ABAP, covering all ABAP releases from version 7.02 to ABAP Cloud:
- S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
- S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
- R/3 NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

A downported version exists for systems running earlier ABAP releases (before 7.50).

### Enterprise Ready

abap2UI5 combines SAP's UI5 framework with ABAP's backend capabilities, tailored for enterprise SAP environments. The framework runs cleanly across S/4HANA Public/Private Cloud, BTP ABAP Environment, and NetWeaver systems.

#### Production Usage
Use abap2UI5 like any other UI5 app or ABAP program in production. Add the framework and your apps to a transport request for deployment.

#### Licensing
Technically, abap2UI5 apps are regular UI5 freestyle applications. License them the same way you handle your other UI5 apps in your organization. abap2UI5 itself is MIT licensed (free for commercial use).

#### Launchpad Integration
Integrate your apps into:
- Fiori Launchpads on S/4 On-Premise
- Tiles on S/4 Public Cloud
- Build Work Zone Websites on BTP

#### Installation
Getting started is simple:
1. Import the project via abapGit
2. Create an HTTP service for browser communication
3. Start developing!

→ *See our [Installation Guide](/get_started/quickstart) for detailed instructions*

### Community
<br>

#### Support
The community provides support. Submit an issue on GitHub or join the abap2UI5 Slack channel for help.

→ *Visit our [Support page](/resources/support) for more options*

#### Contribution
Contributions are always welcome! Whether you fix bugs, build new features, or improve documentation, your input helps the project thrive.

→ *Read our [Contribution Guide](/resources/contribution) to learn how to get involved*

#### Sponsor
Volunteers maintain abap2UI5. If you or your company benefit from the project, consider supporting it.

→ *Learn more about [sponsorship opportunities](/resources/sponsor)*
