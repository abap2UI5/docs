---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_481
  - z2ui5_cl_smps_app_482
  - z2ui5_cl_smps_app_483
  - z2ui5_cl_smps_app_484
description: Embed abap2UI5 apps as tiles in the SAP Fiori Launchpad on S/4HANA on-premise or private cloud - installation, target mapping, launchpad features, KPIs and troubleshooting.
---
# Fiori Launchpad

Embed your abap2UI5 apps into the SAP Fiori Launchpad (FLP) on S/4 On-Premise or Private Cloud. Each app appears as a regular tile; inside the Launchpad shell, abap2UI5 apps can set their title, read startup parameters, and participate in cross-app navigation like any other Fiori app.

## Installation

The Launchpad starts abap2UI5 from the UI5 ABAP repository of your system (as app `z2ui5`): the BSP `Z2UI5`. It is part of abap2UI5 itself - its package `src/03`, next to the node `/sap/bc/z2ui5` and its handler `Z2UI5_CL_UI5_ICF_HANDLER` - so the abapGit pull that installs the framework installs it too (`main`, or a release tag from 1.151.0 on). The downported branch `702` brings no BSP: the app needs a newer UI5 than releases below 7.50 ship. Nothing else is pulled, and the target mapping below needs nothing more.

The BSP carries no copy of the frontend: one view with one `z2ui5.embed.Container`, which loads the frontend from the node (`?z2ui5-bundle`) at run time. After an abap2UI5 update there is nothing to pull again - the frontend always has the backend's version. The trade-off: the frontend is fetched from the backend on every page load instead of being served out of the BSP. For ABAP Cloud see [S/4 Public Cloud](/configuration/s4_public_cloud). After the import, check that the app index is up to date (see [Troubleshooting](#troubleshooting) below).

If an abapGit repository on your system still points at abap2UI5/frontend, where the BSP came from before 1.151.0, see [Coming from the frontend repository](/configuration/installation#frontend-branches).

## Target Mapping
The BSP ships one inbound of its own, `z2ui5-display` (semantic object `z2ui5`, action `display`), which opens the start page of abap2UI5. To give one app a tile of its own, create a target mapping for it. The semantic object is yours to choose; a convention that keeps the targets apart is the name of the app class, as below. What decides which app starts is the parameter `app_start` — replace `Z2UI5_CL_MY_APP` with your app class:
- Semantic Object: `Z2UI5_CL_MY_APP`
- Action: `display`
- URL: `/sap/bc/ui5_ui5/sap/z2ui5`
- ID: `z2ui5`
- Parameter: `app_start / Z2UI5_CL_MY_APP`

## Launchpad Features

Inside your app, the client API gives you access to the Launchpad context. The working samples at the end of this page show each feature in a runnable class.

### Detect the Launchpad Context
`client->get( )-check_launchpad_active` tells you whether the app currently runs inside a Launchpad — useful to hide your own page header or to guard Launchpad-only features:

```abap
IF client->get( )-check_launchpad_active = abap_false.
  client->message_box_display( `This feature needs the Launchpad.` ).
ENDIF.
```

### Set the Title Dynamically
`cs_event-set_title` titles the screen from ABAP at any time. Inside the Launchpad it sets the shell title as well, so one call covers an app that runs in both places:

```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_title
    t_arg = VALUE #( ( `My Dynamic Title` ) ) ).
```

`cs_event-set_title_launchpad` sets the shell title only. [Title](/cookbook/browser_interaction/title) has both in detail.

### Read Startup Parameters
`client->get_start_param( )` reads one start parameter, whichever way the app was started: the URL query first, then the launchpad's startup parameters from the target mapping, then the query of the launchpad intent in the hash. The name is matched in any case; the value comes back as the source carries it, and empty when no source has the parameter:

```abap
DATA(lv_product) = client->get_start_param( `PRODUCT` ).
```

The parameters belong to the page, so they stay readable on every later roundtrip and in every app the user navigates to. The launchpad's startup parameters are also available as name/value pairs in `client->get( )-t_comp_params`:

```abap
DATA(lt_params) = client->get( )-t_comp_params.
DATA(lv_product) = VALUE #( lt_params[ n = `PRODUCT` ]-v OPTIONAL ).
```

### Cross App Navigation

Navigating between Fiori apps goes through the Launchpad's own cross-app
navigation rather than through an abap2UI5 roundtrip, so the shell's history
and back button keep working. The intent, the parameters and the way back are
on [Navigation → Cross App](/cookbook/event_navigation/navigation/cross_app).

## Troubleshooting
Sometimes installation via abapGit causes cache-related issues. Here's how to clear them:

### Cache Management

1. Recalculate the app index of z2ui5 with report `/UI5/APP_INDEX_CALCULATE`
<img width="537" height="170" alt="App index calculation report selection screen" src="https://github.com/user-attachments/assets/50c505ab-c58e-46a6-999e-67c4e4cdb929" />
<img width="473" height="282" alt="App index calculation report output" src="https://github.com/user-attachments/assets/81f8feae-fcfe-4175-aa91-28ce8d681539" />

2. Recalculate the index of the distribution layer with report `/UI5/APP_INDEX_CALCULATE` (if the tab isn't visible, switch to another tab first — it usually shows up after that)
<img width="661" height="219" alt="Distribution layer tab in app index calculation report" src="https://github.com/user-attachments/assets/3fce0f2e-96f9-4487-9226-7940336582b1" />
<img width="525" height="271" alt="Distribution layer recalculation output" src="https://github.com/user-attachments/assets/dc149874-6731-496d-90bf-79cb83d8c97d" />

3. Clear HTTP caches in transaction `SMICM`
<img width="726" height="365" alt="HTTP cache invalidation in transaction SMICM" src="https://github.com/user-attachments/assets/497b7677-8009-472e-9b50-34719105a12e" />

4. Clear browser caches and hard reload

### Manual Deployment
If clearing caches doesn't fix it, push the frontend app manually:

1. Build the app the BSP is written from: the folder `app_source` of abap2UI5, with `npm install` and `npm run build`. Its `dist` folder is what to push.

2. Use the SAP program `/UI5/UI5_REPOSITORY_LOAD` to push the app to the server.
<img width="942" height="858" alt="UI5 Repository Load program for manual frontend deployment" src="https://github.com/user-attachments/assets/2eac29f4-596e-4bab-8a17-7a8f86630b95">

## Launchpad KPIs

Extend your Fiori Launchpad with Key Performance Indicators (KPIs) via the abap2UI5 Launchpad KPI add-on.

<svg class="a2ui5-mark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg> [Repository](https://github.com/abap2UI5-addons/launchpad-kpi)

### Functionality
<img width="800" height="328" alt="Launchpad KPI tiles showing dynamic count values" src="https://github.com/abap2UI5/abap2UI5-connector_launchpad_kpi/assets/102328295/c7db9e46-6876-40d8-a632-be79e2fbcb91">
<br>

### Approach
The integration has three steps: implement a simple interface, the Launchpad calls a generic OData proxy service, and the proxy delegates to your ABAP class to compute the KPI count.

(1/3) Implement the `z2ui5_if_lp_kpi` interface. The `count` method takes an optional `filter` string (from the OData `$filter` parameter) and returns the KPI as an integer:
```abap
INTERFACE z2ui5_if_lp_kpi
  PUBLIC.

  METHODS count
    IMPORTING
      filter        TYPE string
    RETURNING
      VALUE(result) TYPE i.

ENDINTERFACE.
```
(2/3) Implement the interface in your app class next to `z2ui5_if_app`. The `count` method holds your KPI logic (e.g., counting open items from the database):
<!-- playground: no Run button — z2ui5_if_lp_kpi lives in abap2UI5-addons, which the playground does not carry -->
```abap
CLASS z2ui5_cl_lp_kpi_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_lp_kpi.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_lp_kpi_hello_world IMPLEMENTATION.

  METHOD z2ui5_if_lp_kpi~count.
    "kpi calculation...
    result = 10.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    "abap2UI5 app logic here...
  ENDMETHOD.

ENDCLASS.
```
(3/3) A generic OData proxy service (`Z2UI5_PROXY_KPI_SRV`) handles the rest. It takes the `$filter` parameter with your class name, creates an instance of the class, calls `count`, and returns that many dummy OData entries. The Launchpad then shows the `$count` result as the tile KPI. Configure the tile with this endpoint:
```text
.../sap/opu/odata/sap/Z2UI5_PROXY_KPI_SRV/ENTITYCollection/$count?$filter=CLASS eq 'z2ui5_cl_lp_kpi_hello_world'
```

## Background
The steps above are complete on their own. The article series they come from has a screenshot of every dialog:
- [Installation & Configuration](https://www.linkedin.com/pulse/copy-abap2ui5-host-your-apps-sap-fiori-launchpad-abap2ui5-ocn2e/)
- [Features: Title, Parameters, Navigation](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-23-features-abap2ui5-upche/)
- [Integration of KPIs](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/)

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Read Startup Parameters — what the tile passed in - client->get( )-t_comp_params | [`Z2UI5_CL_SMPS_APP_481`](https://github.com/abap2UI5/samples-stack/blob/main/src/09/z2ui5_cl_smps_app_481.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `09-launchpad` |
| Set Shell Title — follow_up_action( cs_event-set_title_launchpad ) | [`Z2UI5_CL_SMPS_APP_482`](https://github.com/abap2UI5/samples-stack/blob/main/src/09/z2ui5_cl_smps_app_482.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `09-launchpad` |
| Cross-App Navigation Sender — hands two values over to another tile | [`Z2UI5_CL_SMPS_APP_483`](https://github.com/abap2UI5/samples-stack/blob/main/src/09/z2ui5_cl_smps_app_483.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `09-launchpad` |
| Cross-App Navigation Receiver — reads them back out of its startup parameters | [`Z2UI5_CL_SMPS_APP_484`](https://github.com/abap2UI5/samples-stack/blob/main/src/09/z2ui5_cl_smps_app_484.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `09-launchpad` |

<!-- samples:end -->
