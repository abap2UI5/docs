---
outline: [2, 6]
---
# About

Welcome to **abap2UI5** – an open-source project designed to enable UI5 app development purely in ABAP.

##### Features
abap2UI5 provides a purely ABAP-based approach for developing UI5 applications, with no need for JavaScript, OData, or RAP. It recalls the simplicity of classic ABAP, where just a few lines of code could generate input forms and tables using Selection Screens and ALVs.

##### Evolution
Launched in 2023 as a personal project, abap2UI5 initially focused on offering basic selection screen functionality for ABAP Cloud, accompanied by an introductory blog post on the SAP Community Network (SCN). With the support of the ABAP open-source community, it has grown significantly, adding a broad range of features. Today, abap2UI5 is a thriving, community-driven initiative.

##### Compatibility
![alt text](image-15.png){ width=50% }
abap2UI5 is compatible with both `ABAP Cloud` and `Standard ABAP`, supporting all ABAP releases from version 7.02 to ABAP for Cloud. It runs seamlessly on systems including R/3 NetWeaver, S/4HANA On-Premise, and S/4HANA Private Cloud, as well as cloud environments such as the BTP ABAP Environment and S/4HANA Public Cloud.

##### Installation
![alt text](image-14.png){ width=50% }
The framework consists solely of classes and interfaces, making it easy to install via abapGit, with no additional deployment required. Communication with the client is established by creating a new HTTP service.

##### Development
Creating UI5 applications in abap2UI5 is straightforward: define a new ABAP class and implement a single method from the abap2UI5 interface. Each app is fully supported as an abapGit project, allowing seamless installation across different systems.

##### Cloud Ready
abap2UI5 relies exclusively on released APIs, making it ideal for both on-stack and side-by-side extensions in the new ABAP for Cloud language version. Within your app, you’re free to utilize modern ABAP capabilities such as CDS, ABAP SQL, and EML.

##### Clean Core
By using only released APIs, abap2UI5 ensures your app is "cloud-ready and upgrade-stable," aligning with clean core principles and guaranteeing compatibility with future upgrades.

##### ABAP Classic
The abap2UI5 framework does not use any newer ABAP syntax features. This makes it also compatible for on-premise and R/3 NetWeaver systems, enabling development in classic ABAP (Tier 3) extensions. A downported version is also available for systems below ABAP 7.50.

##### Architecture
The abap2UI5 system footprint is kept minimal. The base version includes only essential classes and interfaces, and additional functionality can be incorporated by installing optional add-ons.

##### Security
Since you configure the HTTP handler yourself, you retain complete control over all external communications, ensuring security.

##### Productive Usage
As an implementation of an HTTP handler, abap2UI5 can be deployed in productive environments, similar to any other HTTP service.

##### Support
Support for abap2UI5 is provided by the community on a best-effort basis. If you need assistance, submit an issue or join the abap2UI5 community on Slack.

##### Contribution
Contributions are encouraged! Whether through bug fixes, feature additions, documentation enhancements, or community engagement, your involvement helps the project thrive. Consider submitting a pull request, sharing feedback, or helping spread the word about abap2UI5.

##### Sponsor
abap2UI5 is maintained by dedicated developers who volunteer their time. If you or your company benefit from abap2UI5, but cannot contribute directly, please consider supporting the project in other ways. More details are available [here.](/resources/sponsor)