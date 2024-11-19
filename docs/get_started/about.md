---
outline: [2, 6]
---
# About
Welcome to abap2UI5 – an open-source framework designed to enable UI5 app development purely in ABAP. 

##### Features
Whether you're developing in the cloud or on-premise, abap2UI5 simplifies UI5 development, making it fast and efficient without the need for JavaScript, OData, or RAP. It brings back the simplicity of classic ABAP, where just a few lines of code could generate input forms and tables using Selection Screens and ALVs.

##### Evolution
Launched in 2023 as a personal project, abap2UI5 initially focused on providing basic selection screen functionality for ABAP Cloud, introduced via a blog post on the SAP Community Network. Thanks to the support of the ABAP community, it has since grown significantly, adding numerous features. Today, it is a community-driven project, continuously evolving through contributions and feedback.

##### Development
Creating UI5 applications with abap2UI5 is straightforward:
1. Define a new ABAP class
2. Implement a method from the abap2UI5 interface
3. Your app is ready!

Each application is supported as an abapGit project, simplifying installation across systems without the need for extra deployment of frontend artifacts.

##### Architecture
abap2UI5 employs a "thin frontend" approach, with all processing, logic, and data management handled in the backend. This design simplifies configuration, minimizes client-side actions (such as clearing caches), and ensures that business logic and sensitive data remain securely on the server. 

##### Performance 
abap2UI5 is fast. By leveraging the power of the ABAP backend for most processes, it ensures faster execution compared to client-side operations. The frontend focuses solely on rendering UI via the UI5 framework.

##### System Footprint
The abap2UI5 system footprint is kept as small as possible including only essential classes and interfaces in the base version. Additional functionality can be added by installing optional addons.

##### Compatibility
abap2UI5 is compatible with both ABAP Cloud and Standard ABAP, supporting all ABAP releases from version 7.02 to ABAP Cloud:
* S/4 Public Cloud
* BTP ABAP Environment
* S/4 Private Cloud
* S/4 On-Premise
* R/3 NetWeaver

##### Installation
Installing abap2UI5 is easy:
1. Import the framework via abapGit
2. Create a new HTTP service to establish browser communication
3. Start developing!

No additional deployment steps are needed.

##### ABAP Cloud
abap2UI5 uses only released APIs, making it suitable for both on-stack and side-by-side extensions in ABAP for Cloud. You can use modern ABAP syntax features like CDS, ABAP SQL, and EML within your app.

##### ABAP Classic
The framework does not rely on newer ABAP syntax features, ensuring compatibility with on-premise and R/3 NetWeaver systems. A downported version is available for systems running ABAP versions earlier than 7.50.

##### Clean Core
By relying only on released APIs, abap2UI5 ensures that your applications remain "cloud-ready" and "upgrade-stable," aligning with SAP's clean core principles.

##### Security
With complete control over HTTP handler configuration, you maintain oversight of all external communication, ensuring security.

##### Transparency
Transparency is a core principle of abap2UI5. The entire source code is publicly accessible, key concepts are explained in blog posts, and issues are openly discussed. Urgent changes can be promptly implemented and distributed via abapGit.

##### Enterprise Ready
This framework is specifically tailored to meet the needs of ABAP developers building business applications. By leveraging SAP’s UI5 framework for the frontend and adhering to clean core principles on the backend with SAP’s ABAP language, abap2UI5 provides an enterprise-ready solution compatible with a wide range of SAP products. 

##### Productive Usage
abap2UI5 can be used like any other UI5 app or ABAP program in a productive environment. Simply add the framework and your apps to a transport request to deploy them to your production system.

##### Launchpad
Apps developed with abap2UI5 can be integrated into:
* Fiori Launchpads on S/4 On-Premise
* Build Workzone Websites on BTP
* Tiles on S/4 Public Cloud

##### Support
Support is provided on a best-effort basis by the community. Need help? Submit an issue or join the abap2UI5 Slack community for assistance.

##### Contribution
Contributions are always welcome! Whether you’re fixing bugs, requesting features, or improving documentation, your input helps the project grow. You can submit pull requests, provide feedback, or share your experience with the community. Get started with abap2UI5 today!

##### Sponsor
abap2UI5 is maintained by volunteers. If you or your company benefit from the project but cannot contribute directly, consider supporting the project in other ways. Learn more about sponsorship opportunities [here.](/resources/sponsor)
