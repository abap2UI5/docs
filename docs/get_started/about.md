---
outline: [2, 6]
---
# Introduction

**Developing UI5 Apps Purely in ABAP– no JavaScript, OData, or RAP required.**

abap2UI5 is an open-source framework that brings back the simplicity of classic ABAP development to modern UI5 applications. Just as Selection Screens and ALVs could generate functional UIs with minimal code, abap2UI5 lets you create web applications with just a few lines of ABAP.

```abap
CLASS my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->view_display( client->_z2ui5( )->page( 
      title = 'Hello World'
      content = client->_z2ui5( )->button( text = 'Click Me' ) ) ).
  ENDMETHOD.
ENDCLASS.
```

That's it – your first UI5 app is ready!

##### About
Since launching in 2023, abap2UI5 has evolved from a personal project into a community-driven framework trusted by ABAP developers worldwide. The framework handles all frontend complexity, allowing you to focus purely on business logic while leveraging your existing ABAP skills.

→ *See our [Getting Started Guide](/get_started/quickstart) for step-by-step instructions*  
→ *Browse [Sample Applications](/get_started/samples) to see abap2UI5 in action*

### Why abap2UI5?

Traditional UI5 development requires JavaScript expertise, frontend deployment pipelines, and OData service configuration. abap2UI5 eliminates these complexities, making it ideal for:

- **Modernizing legacy applications** without rewriting business logic
- **Implementing extended features** when RAP capabilities reach their limits
- **Rapid prototyping** of new business applications
- **Developer productivity** when frontend resources are limited

Each application is packaged as an abapGit project, simplifying installation across systems without separate frontend deployments.

### Key Benefits
<br>

##### Architecture
abap2UI5 employs a "thin frontend" approach – all processing, logic, and data management stays in the backend. This design simplifies configuration, minimizes client-side complexity (no cache clearing hassles), and ensures business logic and sensitive data remain securely on the server.

##### Performance
The frontend focuses exclusively on UI rendering via the UI5 framework, while the powerful ABAP backend handles all processing. Unlike traditional UI5 applications requiring separate OData calls for each view, abap2UI5 delivers XML views with data already embedded – significantly reducing network round trips and accelerating response times.

##### Security
All business logic remains exclusively in the ABAP backend. The frontend receives only data populated directly in XML views. Unlike traditional UI5 applications that expose OData endpoints, abap2UI5 delivers only the specific data users need – eliminating access to raw services or database queries through external tools.

##### Minimal System Footprint
The base framework includes only essential classes and interfaces. Additional functionality can be added through optional addons, keeping your system lean.

##### Transparency
All source code is publicly available on GitHub. We discuss features and issues openly, publish technical blog posts explaining key concepts, and rapidly deploy fixes through abapGit. You always understand how the framework works and can verify its behavior.

### Compatibility
<br>

##### ABAP Cloud
abap2UI5 uses only released APIs, making it suitable for both on-stack and side-by-side extensions in ABAP for Cloud. Use modern ABAP syntax features like CDS, ABAP SQL, and EML within your apps.

##### Clean Core
By relying only on released APIs, abap2UI5 ensures your applications remain "cloud-ready" and "upgrade-stable," aligning with SAP's clean core principles. Your investment in abap2UI5 apps is protected against future SAP system upgrades.

##### System Support
Compatible with both ABAP Cloud and Standard ABAP, supporting all ABAP releases from version 7.02 to ABAP Cloud:
* S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
* S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
* R/3 NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

A downported version is available for systems running ABAP versions earlier than 7.50.

### Enterprise Ready

abap2UI5 combines SAP's UI5 framework with ABAP's robust backend capabilities, tailored for enterprise SAP environments. The framework works seamlessly across S/4HANA Public/Private Cloud, BTP ABAP Environment, and NetWeaver systems.

##### Production Usage
Use abap2UI5 like any other UI5 app or ABAP program in production. Simply add the framework and your apps to a transport request for deployment.

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
Contributions are always welcome! Whether you're fixing bugs, creating new features, or improving documentation, your input helps the project grow.

→ *Read our [Contribution Guide](/resources/contribution) to learn how to get involved*

##### Sponsor
abap2UI5 is maintained by volunteers. If you or your company benefit from the project, consider supporting it.

→ *Learn more about [sponsorship opportunities](/resources/sponsor)*
