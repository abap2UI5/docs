---
outline: [2, 6]
---
# Introduction

Welcome to abap2UI5 – an open-source project for developing UI5 apps purely in ABAP.

##### Evolution
Launched in 2023 as a free-time project, abap2UI5 began with the basic functionality of a selection screen feature for ABAP Cloud and an initial blog post on SCN. Thanks to contributions from the ABAP open-source community, the project has continuously evolved, incorporating additional UI5 capabilities and expanding system compatibility. Today, abap2UI5 boasts a wide range of features and is a thriving, community-driven initiative.

##### Features
This project offers a pure ABAP approach for developing UI5 apps – entirely without JavaScript, OData, or RAP. Just like in classic ABAP, when a few lines of code were enough to display input forms and tables using Selection Screens & ALVs. It enables you to build apps with minimal system footprint, running seamlessly in both on-premise and cloud environments.

##### Compatibility
Running on all ABAP releases, from NW 7.02 to ABAP Cloud. It is compatible with on-premise landscapes, S/4HANA Private Cloud, On-Premise, and R/3 NetWeaver AS ABAP 7.50 or higher, as well as cloud environments BTP ABAP Environment and S/4HANA Public Cloud.

##### Installation
The framework only consists of classes and interfaces and can be easily isntalled via abapGit – no additional app deployment is required. For frontend backend communicaton you just need to create a single HTTP service.

##### Development
Simply implement a single interface to create a standalone UI5 application. Every abap2UI5 app is just an implentation of a single method of an interface. Every app is therefore again a full supported abapGit project which can be easily installed on various systems.

##### Cloud Ready
abap2UI5 relies only on released APIs, making it ready for clean core implementations with the new ABAP for Cloud language version. The abap2ui5 framework is "cloud ready". In your app implementation you have all freedom and can also use all new CDS, ABAP SQL or EML language features.

##### Clean Core
If you call in your app implementation only released APIs your whole develpment can be called "cloud ready & Upgrade stable" and is ready for the future!

##### ABAP Classic
As abap2UI5 only relies on class and interfaces it can also be used for onpremise abnd classic abap implementations. For older systeme there is also a downported verison availible.

##### Architecture
The abap2UI5 system footprint is kept minimal.  In its base version, it includes only essential classes and interfaces. If you need extened funcitonality you can install additional addons.

##### Security
You manually create an HTTP handler for the frontend comunication of this framwork, giving you full control over all external communication.

##### Use in Production
abap2UI5 is simply an implementation of an HTTP handler and can be used like any other HTTP service in a productive environment.

##### Support
Running into problems or found a bug? Create an issue or join the abap2UI5 community on Slack for assistance. Community support is provided on a best-effort basis.

##### Contribution
We welcome all contributions! Share your knowledge, hunt for or fix bugs, submit a PR, extend this documentation, leave a comment, or simply spread the word about abap2UI5. This project thrives on your support!

##### Sponsor
abap2UI5 relies on dedicated developers investing their free time. If you or your company can't contribute directly, consider supporting the project in other ways. Details are available here.