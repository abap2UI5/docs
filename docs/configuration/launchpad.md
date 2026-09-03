---
outline: [2, 4]
---
# Fiori Launchpad

Embed your abap2UI5 apps into the SAP Fiori Launchpad (FLP) on S/4 On-Premise or Private Cloud. Each app appears as a regular tile; inside the Launchpad shell, abap2UI5 apps can set their title, read startup parameters, and participate in cross-app navigation like any other Fiori app.

## Installation

The Launchpad loads the abap2UI5 frontend from the UI5 ABAP repository of your system (as app `z2ui5`). Install the Launchpad connector from the [abap2UI5-addons](https://github.com/abap2UI5-addons) organization via abapGit — it ships the frontend app for the UI5 repository. After the import, check that the app index is up to date (see [Troubleshooting](#troubleshooting) below).

## Target Mapping
Use these parameters for target mapping in your Launchpad configuration. abap2UI5 uses the app's class name as the Semantic Object so each app gets its own navigation target — replace `Z2UI5_CL_MY_APP` with your app class:
- Semantic Object: `Z2UI5_CL_MY_APP`
- Action: `display`
- URL: `/sap/bc/ui5_ui5/sap/z2ui5`
- ID: `z2ui5`
- Parameter: `app_start / Z2UI5_CL_MY_APP`

## Launchpad Features

Inside your app, the client API gives you access to the Launchpad context. Runnable samples: `Z2UI5_CL_SMPS_APP_481` to `Z2UI5_CL_SMPS_APP_484` in the [samples-stack repository](https://github.com/abap2UI5/samples-stack) (package `src/09`).

### Detect the Launchpad Context
`client->get( )-check_launchpad_active` tells you whether the app currently runs inside a Launchpad — useful to hide your own page header or to guard Launchpad-only features:

```abap
IF client->get( )-check_launchpad_active = abap_false.
  client->message_box_display( `This feature needs the Launchpad.` ).
ENDIF.
```

### Set the Tile Title Dynamically
Change the Launchpad shell title from ABAP at any time with the `set_title_launchpad` frontend event:

```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_title_launchpad
    t_arg = VALUE #( ( `My Dynamic Title` ) ) ).
```

### Read Startup Parameters
Parameters from the target mapping (or the start URL) arrive as name/value pairs in `client->get( )-t_comp_params`:

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
![App index calculation report selection screen](https://github.com/user-attachments/assets/50c505ab-c58e-46a6-999e-67c4e4cdb929)
![App index calculation report output](https://github.com/user-attachments/assets/81f8feae-fcfe-4175-aa91-28ce8d681539)

2. Recalculate the index of the distribution layer with report `/UI5/APP_INDEX_CALCULATE` (if the tab isn't visible, switch to another tab first — it usually shows up after that)
![Distribution layer tab in app index calculation report](https://github.com/user-attachments/assets/3fce0f2e-96f9-4487-9226-7940336582b1)
![Distribution layer recalculation output](https://github.com/user-attachments/assets/dc149874-6731-496d-90bf-79cb83d8c97d)

3. Clear HTTP caches in transaction `SMICM`
![HTTP cache invalidation in transaction SMICM](https://github.com/user-attachments/assets/497b7677-8009-472e-9b50-34719105a12e)

4. Clear browser caches and hard reload

### Manual Deployment
If clearing caches doesn't fix it, push the frontend app manually:

1. Download the webapp folder of the project.

2. Use the SAP program `/UI5/UI5_REPOSITORY_LOAD` to push the app to the server.
<img width="942" alt="UI5 Repository Load program for manual frontend deployment" src="https://github.com/user-attachments/assets/2eac29f4-596e-4bab-8a17-7a8f86630b95">

## Launchpad KPIs

Extend your Fiori Launchpad with Key Performance Indicators (KPIs) via the abap2UI5 Launchpad KPI add-on.

<svg class="a2ui5-mark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true"><path fill="currentColor" d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg> [Repository](https://github.com/abap2UI5-addons/launchpad-kpi)

### Functionality
<img width="800" alt="Launchpad KPI tiles showing dynamic count values" src="https://github.com/abap2UI5/abap2UI5-connector_launchpad_kpi/assets/102328295/c7db9e46-6876-40d8-a632-be79e2fbcb91">
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

## Further Reading
The original article series with additional screenshots:
- [Installation & Configuration](https://www.linkedin.com/pulse/copy-abap2ui5-host-your-apps-sap-fiori-launchpad-abap2ui5-ocn2e/)
- [Features: Title, Parameters, Navigation](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-23-features-abap2ui5-upche/)
- [Integration of KPIs](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/)
