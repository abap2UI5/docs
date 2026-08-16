---
outline: [2, 4]
---
# S/4 Public Cloud

Use abap2UI5 for on-stack extension scenarios in S/4 Public Cloud. It uses only released APIs and is fully compatible with the ABAP Cloud language version, satisfying SAP's "clean core" cloud extension requirements.

To install abap2UI5 on S/4 Public Cloud, follow these steps:

## 1. Install abap2UI5 with abapGit

S/4 Public Cloud supports only the ADT abapGit version. abap2UI5 contains only classes, interfaces, and tables, so you can pull it without issues.

Use the standard installation process with [abapGit for Eclipse](https://eclipse.abapgit.org/).

::: warning **Two choices you cannot change later without relinking**
**Folder logic must be `PREFIX`.** It is what `.abapgit.xml` declares, and the repository's package tree is built for it.

**Keep the target package name short.** abap2UI5 ships sub-packages two levels deep (`src/00/01`, `src/01/04`, …), and with `PREFIX` logic every derived sub-package name starts with the name you pick here. ABAP caps a package name at 30 characters, so a long root leaves the deeper ones no room — the pull then fails and the repository has to be unlinked and linked again against a shorter package. Something like `ZABAP2UI5` is plenty.
:::

<img width="649" height="573" alt="abapGit repository link dialog in ADT" src="https://github.com/user-attachments/assets/9ea46657-5ff5-4075-af12-4b5b359c212d" />

<img width="650" height="571" alt="abapGit pull dialog for abap2UI5 repository" src="https://github.com/user-attachments/assets/41558033-3802-4234-8f50-2611574c870a" />

<img width="641" height="569" alt="abapGit pull operation in progress" src="https://github.com/user-attachments/assets/69eafc9b-7d83-48f8-b970-c68a6a4577ef" />

<img width="880" height="1199" alt="abapGit import log showing pulled artifacts" src="https://github.com/user-attachments/assets/09497311-8653-4e6f-ae0a-3138d0eac47e" />

A status bar shows the progress of the pull operation:

<img width="1438" height="26" alt="abapGit pull operation completed status bar" src="https://github.com/user-attachments/assets/ee085bb9-d708-4de3-a6f0-5493adc57054" />

After a moment, the installation completes:

<img width="1332" height="28" alt="Installation complete confirmation" src="https://github.com/user-attachments/assets/dab3578e-755f-4a6a-adfa-5ef5f04bbfa9" />

## 2. Mass Activate the Imported Artifacts

When installing repositories with ADT abapGit, all artifacts arrive inactive. Activate them manually. The simplest approach: create a dedicated inactive/active folder tree for the abap2UI5 project:

<img width="481" height="182" alt="Inactive objects folder tree in ADT" src="https://github.com/user-attachments/assets/a47fe1f1-5445-46da-96b9-8df3bfe9a99d" />

<img width="691" height="732" alt="Inactive artifacts list requiring activation" src="https://github.com/user-attachments/assets/f7ef6eb9-c13d-4d2f-a541-8854ac27300c" />

Trigger the mass activation for all inactive artifacts. Some objects may refuse to activate on the first pass because something they depend on is still inactive — repeat the mass activation, or force it, until the list is empty:

<img width="638" height="179" alt="Mass activation of abap2UI5 artifacts in progress" src="https://github.com/user-attachments/assets/e133ba2f-d284-47fa-8dbe-cadee9679f2e" />

<img width="639" height="577" alt="Mass activation dialog for abap2UI5 artifacts" src="https://github.com/user-attachments/assets/06997014-1891-4da2-b3a5-25587a6ad587" />

<img width="447" height="112" alt="Activation completed status message" src="https://github.com/user-attachments/assets/ba6614b9-d79e-41ee-9ba0-6339aaee800f" />

<img width="1279" height="410" alt="Activated artifacts in project explorer" src="https://github.com/user-attachments/assets/157ed036-cf6a-4d7b-b93a-2d8bd20af6cb" />

When activation completes, refresh the folder tree:

<img width="626" height="762" alt="All artifacts activated in folder tree" src="https://github.com/user-attachments/assets/86b26c00-e71e-4a3f-a9e9-2beaaeddf577" />

Now you'll see that all artifacts are active:

<img width="641" height="88" alt="Fully activated abap2UI5 package" src="https://github.com/user-attachments/assets/632ea17d-2613-440a-81cd-4d40c526553a" />

## 3. Set Up the HTTP Service

Build an HTTP handler manually or use the cloud branch of the [frontend](https://github.com/abap2UI5/frontend) repository.

**Important:** In S/4 Public Cloud, accessing the HTTP endpoint needs `S_DEVELOP` authorization. This is the quickest way to test in development environments, but makes the scenario unfit for production.

For production, finish the frontend deployment and tile configuration in the steps below.

## 4. Deploy the UI5 App (Optional)

Up to here the HTTP service is enough — you can open it and use abap2UI5 with a developer role. Deploying the app is what makes it reachable for **business users**, through a Launchpad tile.

ADT abapGit cannot import a UI5 app (BSP), so this step runs from VS Code with the [SAP Fiori Tools](https://marketplace.visualstudio.com/items?itemName=SAPSE.sap-ux-fiori-tools-extension-pack) extension pack instead.

1. Clone the branch that matches your stack — `cloud_v2` for the current UI5 runtime:

   ```sh
   git clone --branch cloud_v2 --single-branch https://github.com/abap2UI5/frontend.git
   ```

2. In the `app` folder, open the Fiori **Application Info** page and add a **deployment configuration** pointing at your system. Keep the target package name short here too.
3. Add a **Launchpad configuration** in the same place. It writes the descriptors that let a tile start the app; pick the semantic object and action that suit your scenario.
4. Deploy.

::: tip **Where the deployed app sends its requests**
Only the *separately deployed* app needs this — when the backend serves the page itself, it tells the frontend to post back to the same URL.

The `cloud` and `cloud_v2` branches ship `sap.app.dataSources.http.uri` as `/sap/bc/http/sap/z2ui5`, the path an HTTP service with ID `Z2UI5` is published under. If you created the service under a **different ID**, change that entry in `manifest.json` to match — otherwise the POSTs go to a service that does not exist and the app fails with a `403 ICFEUCONFORBIDDEN` (UCON) error, which does not say which URL it was.

The on-premise branches (`standard`, `standard_v2`) use the SICF path `/sap/bc/z2ui5` instead.
:::

## 5. Give Business Users Access (Optional)

Opening the HTTP service directly works because a developer has `S_DEVELOP`. A business user has not, so the app has to be reached through a tile — which means one chain of objects, each published locally before the next one can see it:

**LADI → IAM App → Business Catalog → Business Role → Space, Page, Tile**

1. **Launchpad App Descriptor Item (LADI)** — create it and add the navigation parameters naming the app to start.

   ::: warning **The `id` has to be UPPERCASE**
   A lower-case `id` is rejected, and the form editor may refuse to save without making clear why. Use *Open With → Source Editor* to edit the descriptor directly.
   :::

2. **IAM App** — create one, include the **HTTP service**, add the LADI from step 1, and publish locally.
3. **Business Catalog** — link the IAM App to it and publish locally.
4. **Business Role** — add the business catalog to a role in the ABAP Launchpad.
5. **Space and page** — create them and place the tile, so the user has somewhere to click.

SAP's [Launchpad configuration guide](https://developers.sap.com/tutorials/abap-s4hanacloud-procurement-purchasereq-flp.html) covers the generic mechanics of these objects.

::: tip **A walkthrough with screenshots**
Warren Eiserman documented a full first install of this scenario, step by step and with screenshots of every dialog:
[UI5 in ABAP Cloud (without RAP or Fiori Elements)](https://blog.decabase.com/ui5-in-abap-cloud-without-rap-or-fiori-elements-4e8a70d961c3). Several of the warnings on this page come from that write-up.
:::

::: tip **BTP ABAP Environment**
BTP ABAP Environment shares the same technical base as S/4 Public Cloud. The instructions above work for both systems.
:::
