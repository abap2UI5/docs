---
outline: [2, 4]
---
# Info

abap2UI5 offers a custom control for reading all frontend information from the user's device — UI5 version and theme, browser, operating system, system type, screen dimensions, and device class (phone, tablet, desktop). This is handy whenever your ABAP logic needs to adapt to the runtime environment.

The control collects the values on the frontend and returns them via two-way binding. Once the `finished` event fires, all bound attributes are filled and available to ABAP. See also `Z2UI5_CL_DEMO_APP_122`.

```abap
CLASS z2ui5_cl_sample_frontend_info DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA ui5_version       TYPE string.
    DATA ui5_theme         TYPE string.
    DATA ui5_gav           TYPE string.
    DATA device_systemtype TYPE string.
    DATA device_os         TYPE string.
    DATA device_browser    TYPE string.
    DATA device_phone      TYPE abap_bool.
    DATA device_desktop    TYPE abap_bool.
    DATA device_tablet     TYPE abap_bool.
    DATA device_combi      TYPE abap_bool.
    DATA device_height     TYPE string.
    DATA device_width      TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_frontend_info IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    client->view_display( view->shell(
        )->page( )->_z2ui5(
            )->info_frontend(
                finished          = client->_event( `POST` )
                device_browser    = client->_bind_edit( device_browser )
                device_os         = client->_bind_edit( device_os )
                device_systemtype = client->_bind_edit( device_systemtype )
                ui5_gav           = client->_bind_edit( ui5_gav )
                ui5_theme         = client->_bind_edit( ui5_theme )
                ui5_version       = client->_bind_edit( ui5_version )
                device_phone      = client->_bind_edit( device_phone )
                device_desktop    = client->_bind_edit( device_desktop )
                device_tablet     = client->_bind_edit( device_tablet )
                device_combi      = client->_bind_edit( device_combi )
                device_height     = client->_bind_edit( device_height )
                device_width      = client->_bind_edit( device_width )
        )->stringify( ) ).

    CASE client->get( )-event.
      WHEN `POST`.
        "process frontend info here...
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

::: tip **Bind Directly in the View**
If you only need device information in the view (not in ABAP logic), bind to the built-in UI5 device model via `{device>/...}` instead — no backend roundtrip required. See [Device Model](../model/device_model.md).
:::
