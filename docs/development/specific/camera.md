---
outline: [2, 4]
---
# Camera

abap2UI5 provides a custom control for capturing photos directly from the device camera. The control returns the image as a base64-encoded string, ready for backend processing.

A basic example, based on sample `Z2UI5_CL_DEMO_APP_306`:
```abap
CLASS z2ui5_cl_demo_app_306 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_picture_base TYPE string.

ENDCLASS.

CLASS z2ui5_cl_demo_app_306 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      DATA(page) = z2ui5_cl_xml_view=>factory( )->shell( )->page( `abap2UI5 - Device Camera Picture`
                )->_z2ui5( )->camera_picture(
                    value    = client->_bind_edit( mv_picture_base )
                    onphoto  = client->_event( `CAPTURE` ) ).
      client->view_display( page->stringify( ) ).
    ENDIF.

    IF client->get( )-event = `CAPTURE`.
      "process mv_picture_base here...
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

::: tip **Browser Restrictions**
Camera access depends on browser permissions and security settings. Most browsers require HTTPS and show a permission prompt. Watch for browser warnings during testing.
:::
