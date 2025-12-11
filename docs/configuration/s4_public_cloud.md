---
outline: [2, 4]
---
# S/4 Public Cloud

abap2UI5 can be used for on-stack extension scenarios in S/4 Public Cloud. It uses exclusively released APIs and is fully compatible with the ABAP Cloud language version, making it cloud-ready and upgrade-stable while meeting all current SAP requirements for ABAP development.

Follow these steps to install abap2UI5 on S/4 Public Cloud:

### 1. Install abap2UI5 with abapGit

S/4 Public Cloud only supports the ADT abapGit version. Since abap2UI5 consists purely of classes, interfaces, and tables, it's fully compatible and can be pulled without issues.

Use the standard installation process via [abapGit for Eclipse](https://eclipse.abapgit.org/).

### 2. Install the HTTP Service

Use the cloud branch of the [abap2UI5-frontend](https://github.com/abap2UI5/frontend) repository.

**Important:** In S/4 Public Cloud and BTP ABAP Environment, you can only access the HTTP endpoint with `S_DEVELOP` authorization. This is the quickest way for testing in development environments, but makes this scenario unsuitable for productive usage. Therefore, we install the frontend artifacts in the next step.

### 3. Deploy the UI5 App

Since UI5 apps (BSPs) aren't supported directly via ADT abapGit, you need to manually deploy the app:

1. Download the app folder from [abap2UI5-frontend](https://github.com/abap2UI5/frontend)
2. Deploy it into your system following this [deployment guide](https://developers.sap.com/tutorials/abap-s4hanacloud-procurement-purchasereq-shop-ui..html#4c15de5c-bce6-46d0-a634-0008261b3117)

### 4. Configure Launchpad & Tiles

Set up the Fiori Launchpad, pages, sections, and tiles for your abap2UI5 apps:

1. Follow this [configuration guide](https://developers.sap.com/tutorials/abap-s4hanacloud-procurement-purchasereq-flp.html)
2. Configure tiles for business users and manage permissions

::: tip **BTP ABAP Environment**
The BTP ABAP Environment shares the same technical foundation as S/4 Public Cloud. All instructions above apply to both systems.
:::
