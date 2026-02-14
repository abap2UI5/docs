---
outline: [2, 4]
---
# Device Model

abap2UI5 offers two ways to access device information: directly in the view via the UI5 device model (frontend), or by collecting it in ABAP attributes via a custom control (backend).

### Frontend

The device model is bound to the view by default with the name `device`. You can use standard UI5 binding syntax to display device properties directly — no backend roundtrip needed. For example:
```abap
page->input( 
  description = `device model - resize - width` 
  value       = `{device>/resize/width}`  ).
```
Explore all available parameters in the [UI5 Documentation.](https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.Device)

### Backend
If you need device information in your ABAP logic (e.g. to adapt behavior based on the browser or screen size), use the `info_frontend` custom control. It collects the values on the frontend and sends them back to the backend via two-way binding (`_bind_edit`). Once the `finished` event fires, all bound attributes are populated and available in ABAP:
```abap
CLASS z2ui5_cl_sample_device DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA ui5_version       TYPE string.
    DATA ui5_theme         TYPE string.
    DATA ui5_gav           TYPE string.
    DATA device_systemtype TYPE string.
    DATA device_os         TYPE string.
    DATA device_browser    TYPE string.
    DATA check_initialized TYPE abap_bool.
    DATA device_phone      TYPE abap_bool.
    DATA device_desktop    TYPE abap_bool.
    DATA device_tablet     TYPE abap_bool.
    DATA device_combi      TYPE abap_bool.
    DATA device_height     TYPE string.
    DATA device_width      TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_device IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory(
        )->page( )->_z2ui5(
            )->info_frontend( 
                finished          = client->_event( `POST` )
                device_browser    = client->_bind_edit( device_browser )
                device_os         = client->_bind_edit( device_os )
                device_systemtype = client->_bind_edit( device_systemtype )
                ui5_gav           = client->_bind_edit( ui5_gav )
                ui5_theme         = client->_bind_edit( ui5_theme )
                ui5_version       = client->_bind_edit( ui5_version )
                device_phone      = client->_bind_edit( device_phone   )
                device_desktop    = client->_bind_edit( device_desktop )
                device_tablet     = client->_bind_edit( device_tablet  )
                device_combi      = client->_bind_edit( device_combi   )
                device_height     = client->_bind_edit( device_height  )
                device_width      = client->_bind_edit( device_width   ) ).

    client->view_display( view->stringify( ) ).

    IF client->get( )-event = `POST`.
      "process device info here...
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```
For a working example, refer to `z2ui5_cl_demo_app_122`.