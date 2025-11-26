---
outline: [2, 4]
---
# Device Model

### Frontend

The device model is bound to the view by default with the name `device`. You can access it easily in your view. For example:
```abap
page->input( 
  description = `device model - resize - width` 
  value       = `{device>/resize/width}`  ).
```
Explore all available parameters in the [UI5 Documentation.](https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.Device)

### Backend
You can also retrieve detailed device information on the backend using a custom info frontend control. This allows you to access UI5 version, device type, OS, browser details, and screen dimensions. Below is an example implementation, which demonstrates how to collect and use this information:
```abap
CLASS z2ui5_cl_sample_device DEFINITION PUBLIC CREATE PUBLIC.

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

    DATA(lo_view) = z2ui5_cl_xml_view=>factory(
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

    client->view_display( lo_view->stringify( ) ).

    IF client->get( )-event = `POST`.
      "process device info here...
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```
For a working example, refer to `z2ui5_cl_demo_app_122`.