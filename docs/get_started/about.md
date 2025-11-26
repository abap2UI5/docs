---
outline: [2, 6]
---
# Introduction

**Developing UI5 Apps Purely in ABAP – no JavaScript, OData, or RAP needed.**

abap2UI5 is an open-source framework that brings back the simplicity of classic ABAP development to modern UI5 applications. Just as Selection Screens and ALVs could generate functional UIs with minimal code, abap2UI5 lets you create web applications with just a few lines of ABAP:

```abap
CLASS zcl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( |Hello World| ).
  ENDMETHOD.
ENDCLASS.
```

That's it – your first UI5 app is ready!

##### About
Since launching in 2023, abap2UI5 has evolved from a small side project into a community-driven framework trusted by ABAP developers worldwide. The framework takes care of all the frontend complexity, letting you focus purely on business logic while leveraging your existing ABAP skills.

→ *See our [Getting Started Guide](/get_started/quickstart) for step-by-step instructions*  
→ *Browse [Sample Applications](/get_started/samples) to see abap2UI5 in action*

### Why abap2UI5?

Traditional UI5 development requires JavaScript expertise, frontend deployment pipelines, and OData service configuration. abap2UI5 cuts through these complexities, making it perfect for:

- **Modernizing legacy applications** – no need to rewrite business logic
- **Extending beyond RAP** – when RAP capabilities reach their limits
- **Rapid prototyping** – quick iteration on new business applications
- **Boosting productivity** – especially when frontend resources are tight

Each application is packaged as an [abapGit](https://abapgit.org) project, simplifying installation across systems without separate frontend deployments.

### Key Benefits
<br>

##### Architecture
abap2UI5 takes a "thin frontend" approach – all processing, logic, and data management stays in the backend. This design simplifies configuration, cuts down on client-side complexity (say goodbye to cache clearing hassles), and keeps business logic and sensitive data securely on the server.

##### Performance
abap2UI5 delivers exceptional performance. The frontend focuses exclusively on UI rendering via the UI5 framework, while the powerful ABAP backend handles all processing. Unlike traditional UI5 apps that need separate OData calls for each view, abap2UI5 embeds data directly in XML views – dramatically reducing network round trips and speeding up response times.

##### Security
abap2UI5 is secure by design. All business logic stays exclusively in the ABAP backend. The frontend receives only the data populated directly in XML views. Unlike traditional UI5 apps that expose OData endpoints, abap2UI5 delivers only what users need – no access to raw services or database queries through external tools.

##### Minimal System Footprint
The base framework includes only essential classes and interfaces. Additional functionality can be added through optional addons, keeping your system lean.

##### Transparency
All source code is publicly available on GitHub. We discuss features and issues openly, publish technical blog posts explaining key concepts, and rapidly deploy fixes through abapGit. You'll always understand how the framework works and can verify its behavior.

### Compatibility
<br>

##### ABAP Cloud
abap2UI5 uses only released APIs, making it perfect for both on-stack and side-by-side extensions in ABAP for Cloud. Take advantage of modern ABAP syntax features like CDS, ABAP SQL, and EML within your apps.

##### Clean Core
By relying only on released APIs, abap2UI5 ensures your applications remain "cloud-ready" and "upgrade-stable," aligning with SAP's clean core principles. Your investment in abap2UI5 apps is protected against future SAP system upgrades.

##### System Support
Works with both ABAP Cloud and Standard ABAP, supporting all ABAP releases from version 7.02 to ABAP Cloud:
* S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
* S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
* R/3 NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

A downported version is available for systems running earlier ABAP versions (before 7.50).

### Enterprise Ready

abap2UI5 combines SAP's UI5 framework with ABAP's robust backend capabilities, tailored for enterprise SAP environments. The framework works seamlessly across S/4HANA Public/Private Cloud, BTP ABAP Environment, and NetWeaver systems.

##### Production Usage
Use abap2UI5 just like any other UI5 app or ABAP program in production. Simply add the framework and your apps to a transport request for deployment.

##### Launchpad Integration
Integrate your apps into:
* Fiori Launchpads on S/4 On-Premise
* Tiles on S/4 Public Cloud
* Build Workzone Websites on BTP

##### Installation
Getting started is straightforward:
1. Import the project via abapGit
2. Create an HTTP service for browser communication
3. Start developing!

→ *See our [Installation Guide](/get_started/quickstart) for detailed instructions*

### Community
<br>

##### Support
Support is provided by the community. Submit an issue on GitHub or join the abap2UI5 Slack channel for assistance.

→ *Visit our [Support page](/resources/support) for more options*

##### Contribution
Contributions are always welcome! Whether you're fixing bugs, building new features, or improving documentation, your input helps the project grow.

→ *Read our [Contribution Guide](/resources/contribution) to learn how to get involved*

##### Sponsor
abap2UI5 is maintained by volunteers. If you or your company benefit from the project, consider supporting it.

→ *Learn more about [sponsorship opportunities](/resources/sponsor)*
