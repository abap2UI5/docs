---
outline: [2, 4]
---
# S/4 Public Cloud

abap2UI5 can be used for on-stack extension scenarios in S/4 Public Cloud. It exclusively uses released APIs and is fully compatible with the ABAP Cloud language version, meeting all SAP requirements for "clean core" cloud extensions.

Follow these steps to install abap2UI5 on S/4 Public Cloud:

### 1. Install abap2UI5 with abapGit

S/4 Public Cloud only supports the ADT abapGit version. abap2UI5 consists purely of classes, interfaces, and tables and can be pulled without issues.

Use the standard installation process via [abapGit for Eclipse](https://eclipse.abapgit.org/).

<img width="649" height="573" alt="image" src="https://github.com/user-attachments/assets/9ea46657-5ff5-4075-af12-4b5b359c212d" />

<img width="650" height="571" alt="image" src="https://github.com/user-attachments/assets/41558033-3802-4234-8f50-2611574c870a" />

<img width="641" height="569" alt="image" src="https://github.com/user-attachments/assets/69eafc9b-7d83-48f8-b970-c68a6a4577ef" />

<img width="880" height="1199" alt="image" src="https://github.com/user-attachments/assets/09497311-8653-4e6f-ae0a-3138d0eac47e" />

You'll see the pull operation is now running:

<img width="1438" height="26" alt="image" src="https://github.com/user-attachments/assets/ee085bb9-d708-4de3-a6f0-5493adc57054" />

After a few moments, the installation will complete:

<img width="1332" height="28" alt="image" src="https://github.com/user-attachments/assets/dab3578e-755f-4a6a-adfa-5ef5f04bbfa9" />


### 2. Mass Activate the Imported Artifacts

When installing repositories with ADT abapGit, all artifacts are initially imported in an inactive state. You'll need to manually activate them. The easiest approach is to create a dedicated inactive/active folder tree for the abap2UI5 project:

<img width="481" height="182" alt="image" src="https://github.com/user-attachments/assets/a47fe1f1-5445-46da-96b9-8df3bfe9a99d" />

<img width="691" height="732" alt="image" src="https://github.com/user-attachments/assets/f7ef6eb9-c13d-4d2f-a541-8854ac27300c" />

<img width="638" height="179" alt="image" src="https://github.com/user-attachments/assets/e133ba2f-d284-47fa-8dbe-cadee9679f2e" />

<img width="639" height="577" alt="image" src="https://github.com/user-attachments/assets/06997014-1891-4da2-b3a5-25587a6ad587" />

<img width="447" height="112" alt="image" src="https://github.com/user-attachments/assets/ba6614b9-d79e-41ee-9ba0-6339aaee800f" />

<img width="1279" height="410" alt="image" src="https://github.com/user-attachments/assets/157ed036-cf6a-4d7b-b93a-2d8bd20af6cb" />

When the activation process is completed, refresh the folder tree:

<img width="626" height="762" alt="image" src="https://github.com/user-attachments/assets/86b26c00-e71e-4a3f-a9e9-2beaaeddf577" />

Now you'll see that all artifacts are activated:
<img width="641" height="88" alt="image" src="https://github.com/user-attachments/assets/632ea17d-2613-440a-81cd-4d40c526553a" />


### 3. Set Up the HTTP Service

Create a new HTTP handler manually or use the cloud branch of the [abap2UI5-frontend](https://github.com/abap2UI5/frontend) repository.

**Important:** In S/4 Public Cloud you can only access the HTTP endpoint with `S_DEVELOP` authorization. This is the quickest way for testing in development environments, but makes this scenario unsuitable for productive usage. 

To use abap2UI5 in production environments, you'll need to complete the frontend deployment and tile configuration in the following steps.

### 4. Deploy the UI5 App (Optional)

Since UI5 apps (BSPs) aren't supported directly via ADT abapGit, you need to manually deploy the app:

1. Download the app folder from [abap2UI5-frontend](https://github.com/abap2UI5/frontend)
2. Deploy it into your system following this [deployment guide](https://developers.sap.com/tutorials/abap-s4hanacloud-procurement-purchasereq-shop-ui..html#4c15de5c-bce6-46d0-a634-0008261b3117)

### 5. Configure Launchpad & Tiles (Optional)

Set up the Fiori Launchpad, pages, sections, and tiles for your abap2UI5 apps:

1. Follow this [configuration guide](https://developers.sap.com/tutorials/abap-s4hanacloud-procurement-purchasereq-flp.html)
2. Configure tiles for business users and manage permissions

::: tip **BTP ABAP Environment**
The BTP ABAP Environment shares the same technical foundation as S/4 Public Cloud. All instructions above apply to both systems.
:::
