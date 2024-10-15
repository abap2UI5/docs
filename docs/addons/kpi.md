# Launchpad KPIs

<i class="fa-brands fa-github"></i> [Repository](https://github.com/abap2UI5-addons/launchpad-kpi)

Find more information [here.](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/?trackingId=bOVbNH171LDtGAqrDcAt4Q%3D%3D)

### Key Features
* KPI Connector: Send KPIs of your abap2UI5 Apps to SAP Fiori Launchpad
* User-Friendly: Implement just a single interface and method to return the KPI value
* Project Consistency: Easily integrable with your abap2UI5 apps
* Compatibility: Runs with SAP Netweaver (v.7.30 or higher) or S/4 Private (Standard ABAP)

### Functionality
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-connector_launchpad_kpi/assets/102328295/c7db9e46-6876-40d8-a632-be79e2fbcb91">
<br>

[link](https://excalidraw.com/#json=d-kRyy0bzOtYQgxweVXon,u2mAWDGdB9dg-J0NXbMvnw)

### Preview
<img width="621" alt="Pasted Graphic 3" src="https://github.com/abap2UI5/abap2UI5-connector_launchpad_kpi/assets/102328295/1b24c31e-5570-4324-92d0-5db915394ceb">


### Approach
(1/4) Use a single Interface:
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
(2/4) Which can be used on app level to return KPIs:
```abap
CLASS z2ui5_cl_lp_kpi_hello_world DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_proxy_kpi.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_proxy_kpi_hello_world IMPLEMENTATION.

  METHOD z2ui5_if_lp_kpi~count.
    "kpi calculation....
    result = 10.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    "abap2UI5 app logic here...
  ENDMETHOD.

ENDCLASS.
```
(3/4) A generic OData service takes care of everything else (which just returns n dummy entries):
```abap
  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

    DATA lt_result TYPE zcl_z2ui5_proxy_kpi_mpc=>tt_entity.
    DATA(lt_filter_cond) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    TRY.
        DATA(lv_classname)   = to_upper( lt_filter_cond[ property = `CLASS` ]-select_options[ 1 ]-low ).
      CATCH cx_root.
        INSERT VALUE #( id = `ERROR_NO_PARAMETER_FOUND_WITH_NAME_CLASS` ) INTO TABLE lt_result.
        copy_data_to_ref( EXPORTING is_data = lt_result CHANGING cr_data = er_entityset ).
        RETURN.
    ENDTRY.

    TRY.
        DATA(lv_filter) = to_upper( lt_filter_cond[ property = `FILTER` ]-select_options[ 1 ]-low ).
      CATCH cx_root.
    ENDTRY.

    DATA li_lp_kpi TYPE REF TO z2ui5_if_lp_kpi.
    CREATE OBJECT li_lp_kpi TYPE (lv_classname).
    DATA(lv_count) = li_lp_kpi->count( lv_filter ).

    DO lv_count TIMES.
      INSERT VALUE #( id = sy-index ) INTO TABLE lt_result.
    ENDDO.

    copy_data_to_ref( EXPORTING is_data = lt_result CHANGING cr_data = er_entityset ).

  ENDMETHOD.
```
(4/4) Maintain the KPI at the Launchpad with the following endpoint:
.../sap/opu/odata/sap/Z2UI5_PROXY_KPI_SRV/ENTITYCollection/$count?$filter=CLASS eq 'z2ui5_cl_proxy_kpi_hello_world'

#### Installation
[**Guideline**](https://www.linkedin.com/pulse/abap2ui5-host-your-apps-sap-fiori-launchpad-33-kpis-abap2ui5-uuxxe/)
