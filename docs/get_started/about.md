---
outline: [2, 4]
---
# Introduction

**Build UI5 Apps Purely in ABAP**

abap2UI5 is an open-source framework that brings back the simplicity of classic ABAP development to modern UI5 applications. Just as Selection Screens and ALVs could generate functional UIs with minimal code, abap2UI5 lets you create web applications with just a few lines of ABAP:

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

That's it -- your first UI5 app is ready.

Since launching in 2023, abap2UI5 has evolved from a small side project into a community-driven framework trusted by ABAP developers worldwide. The framework takes care of all the frontend complexity, letting you focus purely on business logic while leveraging your existing ABAP skills.

> *See the [Getting Started Guide](/get_started/quickstart) for step-by-step instructions*
> *Browse [Sample Applications](/get_started/samples) to see abap2UI5 in action*

### Why abap2UI5?

Traditional UI5 development requires JavaScript expertise, frontend deployment, and OData service configuration. abap2UI5 removes these hurdles:

- **Use your existing ABAP skills** -- no frontend expertise required
- **Run everywhere** -- from legacy R/3 systems to modern S/4 Cloud environments
- **Go beyond RAP** -- when standard Fiori Elements capabilities reach their limits, abap2UI5 gives you full control over the UI
- **Iterate fast** -- prototype and refine business applications quickly

Each application is packaged as an [abapGit](https://abapgit.org) project, so installation works across systems without separate frontend deployments.

### Overview

#### Architecture
abap2UI5 follows a "thin frontend" approach -- all processing, logic, and data management stays in the ABAP backend. The browser handles only UI rendering. This keeps configuration simple, avoids client-side complexity, and ensures that business logic and sensitive data remain on the server.

#### Performance
The frontend renders UI5 controls while the ABAP backend handles all processing. Unlike traditional UI5 apps that need separate OData calls for each view, abap2UI5 embeds data directly in XML views. This reduces network round trips and speeds up response times.

#### Security
All business logic stays in the ABAP backend. The frontend receives only the data populated in XML views -- no OData endpoints are exposed, and no raw services or database queries are accessible from outside.

#### System Footprint
The framework consists of classes and interfaces only. Additional functionality can be added through optional add-ons, keeping your system lean.

#### Transparency
All source code is publicly available on GitHub. Features and issues are discussed openly, and fixes are deployed quickly through abapGit.

### Compatibility

#### ABAP Cloud
abap2UI5 uses only released APIs, making it suitable for both on-stack and side-by-side extensions in ABAP for Cloud. You can take advantage of modern ABAP features like CDS, ABAP SQL, and EML within your apps.

#### Clean Core
By relying only on released APIs, abap2UI5 ensures your applications remain cloud-ready and upgrade-stable, aligning with SAP's clean core principles.

#### System Support
abap2UI5 works with both ABAP Cloud and Standard ABAP, supporting all releases from version 7.02 to ABAP Cloud:
* S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
* S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
* R/3 NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

A downported version is available for systems running ABAP versions before 7.50.

### Enterprise Ready

abap2UI5 combines SAP's UI5 framework with ABAP's backend capabilities for enterprise SAP environments. It works across S/4HANA Public/Private Cloud, BTP ABAP Environment, and NetWeaver systems.

#### Production Usage
Use abap2UI5 just like any other UI5 app or ABAP program in production. Add the framework and your apps to a transport request for deployment.

#### Licensing
Technically, abap2UI5 apps are regular UI5 freestyle applications. License them the same way you handle your other UI5 apps. The framework itself is MIT licensed (free for commercial use).

#### Launchpad Integration
Integrate your apps into:
* Fiori Launchpads on S/4 On-Premise
* Tiles on S/4 Public Cloud
* Build Work Zone Websites on BTP

#### Installation
Getting started takes three steps:
1. Import the project via abapGit
2. Create an HTTP service for browser communication
3. Start developing

> *See the [Installation Guide](/get_started/quickstart) for detailed instructions*

### Community

#### Support
Support is provided by the community. Submit an issue on GitHub or join the abap2UI5 Slack channel for help.

> *Visit the [Support page](/resources/support) for more options*

#### Contribution
Contributions are always welcome -- whether you're fixing bugs, building new features, or improving documentation.

> *Read the [Contribution Guide](/resources/contribution) to get involved*

#### Sponsor
abap2UI5 is maintained by volunteers. If you or your company benefit from the project, consider supporting it.

> *Learn more about [sponsorship opportunities](/resources/sponsor)*
