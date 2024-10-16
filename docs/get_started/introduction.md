---
outline: [2, 6]
---
# Introduction

Welcome to abap2UI5 – an open-source project that allows you to develop UI5 apps purely in ABAP.

##### Evolution
Launched in 2023 as a personal project, abap2UI5 started with basic functionality like displaying selection screens and an initial blog post on SCN. Thanks to contributions from the ABAP open-source community, the project has grown significantly in UI capabilities, system compatibility, and extensibility. Today, abap2UI5 is a thriving, community-driven initiative.

##### Features
This project offers a pure ABAP approach for developing UI5 apps - entirely without JavaScript, OData or RAP. Just like in the past, when a few lines of ABAP were enough to display input forms and tables using Selection Screens & ALVs. Designed with a minimal system footprint, it works in both on-premise and cloud environments. Developing purely in ABAP (no JavaScript, DDL, EML or Customizing)

##### Compatibility
Runs on all ABAP releases (from NW 7.02 to ABAP Cloud). every system fro 702 to cloud releases is compative to abap2UI5. in on premise landscapes it can be used with S/4 Private Cloud or On-Premise, R/3 NetWeaver AS ABAP 7.50 or higher, R/3 NetWeaver AS ABAP 7.02 to 7.42  and in cloud envornments with BTP ABAP Environment, S/4 Public Cloud

##### Installation
Install via abapGit, no additional app deployment required. Implement a single interface to create a standalone UI5 application. Uses a simple HTTP handler (no BSP, OData, CDS or RAP)

##### Development
Every abap2UI5 app is just an implentation of a single method of an interface.

##### Clean Core

##### Cloud Ready
abap2UI5 relies only on released APIs, making it ready for clean core implementations with the new language version ABAP for Cloud making the abap2UI5 apps "cloud-ready and upgrade-stable. All new CDS capabilities and EML features can be used inside the app implementations.

##### ABAP Classic
abap2UI5 only relies on releaes API, making it ready fo the use under clean core aspects with the ne wlanguage version ABAP for Cloud. While skiping the UI part all new CDS cabapilities and EML features can be used making it so called "cloud ready and upgrade stable".

##### Architecture
The abap2UI5 system footprint is kept minimal. In its base version, it includes only essential classes and interfaces.  If you're new to these basics, you can further extend the project by installing additional addons listed here.

##### Security
You need to manually create an HTTP handler, providing full control over the externalization of the project. Learn more here.

##### Go Live
abapUI5 is technically just an implementation of an HTTP handler and can be used as any other HTTP Service also in a productive scenario.

##### Support
Running into problems or found a bug? Create an issue or join the abap2UI5 community on Slack for assistance. Community support is offered on a best-effort basis.

##### Contribution
We welcome all contributions! Share your knowledge, hunt for or fix bugs, submit a PR, extend this documentation, leave a comment, or simply spread the word about abap2UI5. This project thrives on your support!

##### Sponsor
abap2UI5 relies on dedicated developers investing their free time. If you or your company can't contribute directly, consider supporting the project in other ways, as detailed here. 🙏