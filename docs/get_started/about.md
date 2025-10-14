---
outline: [2, 6]
---
# Introduction

abap2UI5 is an open-source project that enables ABAP developers to build modern SAP Fiori/UI5 applications using only ABAP code — no JavaScript, OData, or RAP required.

##### About
abap2UI5 simplifies UI5 development for both cloud and on-premise environments, bringing back the simplicity of classic ABAP development. Just as Selection Screens and ALVs could generate functional UIs with minimal code, abap2UI5 lets you create modern web applications with just a few lines of ABAP. The framework handles all frontend complexity, allowing you to focus on business logic.

Since launching in 2023, abap2UI5 has evolved from a personal project into a community-driven framework trusted by SAP developers worldwide, with active contributions, regular updates, and continuous improvements.

##### Why abap2UI5?
Traditional UI5 development requires JavaScript expertise, frontend deployment pipelines, and OData service configuration. abap2UI5 eliminates these complexities, letting ABAP developers leverage their existing skills to build modern UIs quickly. Whether you're modernizing legacy applications, building new business apps, or creating prototypes, abap2UI5 streamlines the development process without requiring additional frontend resources.

##### Development
Creating UI5 applications with abap2UI5 is straightforward:
1. Define a new ABAP class
2. Implement a method from the abap2UI5 interface
3. Your app is ready!

Each application is supported as an abapGit project, simplifying installation across systems without the need for extra deployment of frontend artifacts.

→ *See our [Getting Started Guide](#) for step-by-step instructions*  
→ *Browse [Sample Applications](#) to see abap2UI5 in action*

### Key Benefits


##### Architecture
abap2UI5 employs a "thin frontend" approach, with all processing, logic, and data management handled in the backend. This design simplifies configuration, minimizes client-side actions (such as clearing caches), and ensures that business logic and sensitive data remain securely on the server.

##### Performance
abap2UI5 is fast! By leveraging the power of the ABAP backend for most processes, it ensures efficient execution comparable to other UI5 applications. The frontend focuses solely on rendering UI via the UI5 framework.

##### System Footprint
The abap2UI5 system footprint is kept as small as possible, including only essential classes and interfaces in the base version. Additional functionality can be added by installing optional addons.

##### Security
With complete control over HTTP handler configuration, you maintain oversight of all external communication. Additionally, abap2UI5 apps contain all business logic fully in the backend, ensuring the highest level of security.

##### Transparency
All source code is publicly available on GitHub. We discuss features and issues openly, publish technical blog posts explaining key concepts, and rapidly deploy fixes through abapGit. This commitment to transparency ensures you always understand how the framework works and can verify its behavior.

### Compatibility

##### ABAP Cloud
abap2UI5 uses only released APIs, making it suitable for both on-stack and side-by-side extensions in ABAP for Cloud. You can use modern ABAP syntax features like CDS, ABAP SQL, and EML within your apps.

##### Clean Core
By relying only on released APIs, abap2UI5 ensures that your applications remain "cloud-ready" and "upgrade-stable," aligning with SAP's clean core principles. Your investment in abap2UI5 apps is protected against future SAP system upgrades.

##### ABAP Classic
The framework does not rely on newer ABAP syntax features, ensuring compatibility with on-premise and R/3 NetWeaver systems. A downported version is available for systems running ABAP versions earlier than 7.50.

##### System Support
abap2UI5 is compatible with both ABAP Cloud and Standard ABAP, supporting all ABAP releases from version 7.02 to ABAP Cloud:
* S/4 Public Cloud and BTP ABAP Environment (ABAP for Cloud)
* S/4 Private Cloud or On-Premise (ABAP for Cloud, Standard ABAP)
* R/3 NetWeaver AS ABAP 7.02 or higher (Standard ABAP)

### Operations


##### Installation
Installing abap2UI5 is easy:
1. Import the project via abapGit
2. Create a new HTTP service to establish browser communication
3. Start developing!

No additional deployment or configuration steps are needed.

→ *See our [Installation Guide](#) for detailed instructions*

##### Enterprise Ready
This framework is tailored for enterprise SAP environments, combining SAP's UI5 framework with ABAP's robust backend capabilities. abap2UI5 provides an enterprise-ready way to create software compatible with a wide range of SAP products, from S/4HANA Public/Private Cloud to BTP ABAP Environment and NetWeaver systems.

##### Productive Usage
abap2UI5 can be used like any other UI5 app or ABAP program in a productive environment. Simply add the framework and your apps to a transport request to deploy them to your productive system.

##### Launchpad Integration
Apps developed with abap2UI5 can be integrated into:
* Fiori Launchpads on S/4 On-Premise
* Tiles on S/4 Public Cloud
* Build Workzone Websites on BTP

### Community


##### Support
Support is provided by the community. Need help? Submit an issue on GitHub or join the abap2UI5 Slack channel for assistance.

→ *Visit our [Support page](#) for more options*

##### Contribution
Contributions are always welcome! Whether you're fixing bugs, creating new features, or improving documentation, your input helps the project grow. Check out our contribution guidelines to get started.

→ *Read our [Contribution Guide](#) to learn how to get involved*

##### Sponsor
abap2UI5 is maintained by volunteers. If you or your company benefit from the project but cannot contribute directly, consider supporting the project in other ways.

→ *Learn more about [sponsorship opportunities](#)*
