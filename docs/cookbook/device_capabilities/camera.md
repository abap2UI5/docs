---
outline: [2, 4]
---
# Camera

abap2UI5 offers a custom control for taking photos directly from the device's camera. The control returns the image as a base64-encoded string, ready for backend processing.

A minimal example based on sample `Z2UI5_CL_SMP_APP_306`:
```abap
CLASS z2ui5_cl_smp_app_306 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_picture_base TYPE string.

ENDCLASS.

CLASS z2ui5_cl_smp_app_306 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`       v = `sap.m`
              )->a( n = `xmlns:mvc`   v = `sap.ui.core.mvc`
              )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `abap2UI5 - Device Camera Picture`

                      )->tag( n = `CameraPicture` ns = `z2ui5`
                          )->a( n = `value`   v = client->_bind( mv_picture_base )
                          )->a( n = `OnPhoto` v = client->_event( `CAPTURE` ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

    IF client->get( )-event = `CAPTURE`.
      "process mv_picture_base here...
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

::: tip **Browser Restrictions**
Camera access relies on browser permissions and security settings. Most browsers need HTTPS and display a permission prompt. Watch for browser warnings while testing.
:::
