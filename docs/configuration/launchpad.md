---
outline: [2, 4]
---
# Fiori Launchpad

Embed your abap2UI5 apps into the SAP Fiori Launchpad (FLP) on S/4 On-Premise or Private Cloud. Each app appears as a regular tile; inside the Launchpad shell, abap2UI5 apps can set their title, read startup parameters, and participate in cross-app navigation like any other Fiori app.

### Installation

The Launchpad loads the abap2UI5 frontend from the UI5 ABAP repository of your system (as app `z2ui5`). Install the Launchpad connector from the [abap2UI5-addons](https://github.com/abap2UI5-addons) organization via abapGit — it ships the frontend app for the UI5 repository. After the import, check that the app index is up to date (see [Troubleshooting](#troubleshooting) below).

### Target Mapping
Use these parameters for target mapping in your Launchpad configuration. abap2UI5 uses the app's class name as the Semantic Object so each app gets its own navigation target — replace `Z2UI5_CL_MY_APP` with your app class:
- Semantic Object: `Z2UI5_CL_MY_APP`
- Action: `display`
- URL: `/sap/bc/ui5_ui5/sap/z2ui5`
- ID: `z2ui5`
- Parameter: `app_start / Z2UI5_CL_MY_APP`

### Launchpad Features

Inside your app, the client API gives you access to the Launchpad context. Runnable samples: `Z2UI5_CL_DEMO_APP_LP_01` to `Z2UI5_CL_DEMO_APP_LP_04` in the [samples repository](https://github.com/abap2UI5/samples).

#### Detect the Launchpad Context
`client->get( )-check_launchpad_active` tells you whether the app currently runs inside a Launchpad — useful to hide your own page header or to guard Launchpad-only features:

```abap
IF client->get( )-check_launchpad_active = abap_false.
  client->message_box_display( `This feature needs the Launchpad.` ).
ENDIF.
```

#### Set the Tile Title Dynamically
Change the Launchpad shell title from ABAP at any time with the `set_title_launchpad` frontend event:

```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_title_launchpad
    t_arg = VALUE #( ( `My Dynamic Title` ) ) ).
```

#### Read Startup Parameters
Parameters from the target mapping (or the start URL) arrive as name/value pairs in `client->get( )-t_comp_params`:

```abap
DATA(lt_params) = client->get( )-t_comp_params.
DATA(lv_product) = VALUE #( lt_params[ n = `PRODUCT` ]-v OPTIONAL ).
```

#### Cross App Navigation
Handle view changes and popups through the abap2UI5 backend as usual. But for navigation *between* apps in a Launchpad, use the Launchpad's own cross-app navigation instead of a backend roundtrip — this keeps browser navigation and history working. Fire the `cross_app_nav_to_ext` event with the target intent (and optional parameters, here taken from a bound structure):

```abap
)->button(
    text  = `go to app 128`
    press = client->_event_client(
        val   = client->cs_event-cross_app_nav_to_ext
        t_arg = VALUE #(
            ( `{ semanticObject: "Z2UI5_CL_LP_SAMPLE_04", action: "display" }` )
            ( `$` && client->_bind_edit( nav_params ) ) ) ) )
```

To navigate back to the previous Launchpad app, use `cross_app_nav_to_prev_app`:

```abap
)->button(
    text  = `BACK`
    press = client->_event_client( client->cs_event-cross_app_nav_to_prev_app ) )
```

### Troubleshooting
Sometimes installation via abapGit causes cache-related issues. Here's how to clear them:

#### Cache Management

1. Recalculate the app index of z2ui5 with report `/UI5/APP_INDEX_CALCULATE`
![App index calculation report selection screen](https://github.com/user-attachments/assets/50c505ab-c58e-46a6-999e-67c4e4cdb929)
![App index calculation report output](https://github.com/user-attachments/assets/81f8feae-fcfe-4175-aa91-28ce8d681539)

2. Recalculate the index of the distribution layer with report `/UI5/APP_INDEX_CALCULATE` (if the tab isn't visible, switch to another tab first — it usually shows up after that)
![Distribution layer tab in app index calculation report](https://github.com/user-attachments/assets/3fce0f2e-96f9-4487-9226-7940336582b1)
![Distribution layer recalculation output](https://github.com/user-attachments/assets/dc149874-6731-496d-90bf-79cb83d8c97d)

3. Clear HTTP caches in transaction `SMICM`
![HTTP cache invalidation in transaction SMICM](https://github.com/user-attachments/assets/497b7677-8009-472e-9b50-34719105a12e)

4. Clear browser caches and hard reload

#### Manual Deployment
If clearing caches doesn't fix it, push the frontend app manually:

1. Download the webapp folder of the project.

2. Use the SAP program `/UI5/UI5_REPOSITORY_LOAD` to push the app to the server.
<img width="942" alt="UI5 Repository Load program for manual frontend deployment" src="https://github.com/user-attachments/assets/2eac29f4-596e-4bab-8a17-7a8f86630b95">

### Launchpad KPIs

Extend your Fiori Launchpad with Key Performance Indicators (KPIs) via the abap2UI5 Launchpad KPI add-on.

<i class="fa-brands fa-github"></i> [Repository](https://github.com/abap2UI5-addons/launchpad-kpi)

#### Functionality
<img width="800" alt="Launchpad KPI tiles showing dynamic count values" src="https://github.com/abap2UI5/abap2UI5-connector_launchpad_kpi/assets/102328295/c7db9e46-6876-40d8-a632-be79e2fbcb91">
<br>

#### Approach
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
```abap
CLASS z2ui5_cl_lp_kpi_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_lp_kpi.
    INTERFACES z2ui5_if_app.

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

### Further Reading
The original article series with additional screenshots:
- [Installation & Configuration](https://www.linkedin.com/pulse/copy-abap2ui5-host-your-apps-sap-fiori-launchpad-abap2ui5-ocn2e/)
- [Features: Title, Parameters, Navigation](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-23-features-abap2ui5-upche/)
- [Integration of KPIs](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/)
