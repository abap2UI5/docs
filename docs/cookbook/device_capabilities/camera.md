---
outline: [2, 4]
---
# Camera

::: warning This page still shows the previous view builder
The examples below build views with `z2ui5_cl_xml_view`. That class is frozen:
it still runs, and your existing apps keep working — but it is no longer the
one to write new code against. The current builder is
`z2ui5_cl_ui5_view_builder`, and it has four verbs instead of a control per
method, which makes every UI5 control available rather than the curated set.

See [View → Definition](/cookbook/view/definition) for what the chain looks
like, and [Deprecations](/resources/deprecations) for the translation.
:::

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
      DATA(page) = z2ui5_cl_xml_view=>factory( )->shell( )->page( `abap2UI5 - Device Camera Picture`
                )->_z2ui5( )->camera_picture(
                    value    = client->_bind( mv_picture_base )
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
Camera access relies on browser permissions and security settings. Most browsers need HTTPS and display a permission prompt. Watch for browser warnings while testing.
:::
