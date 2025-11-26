---
outline: [2, 4]
---
# Camera


For camera capabilities, refer to the sample `z2ui5_cl_sample_app_306`:
```abap
CLASS z2ui5_cl_demo_app_306 DEFINITION PUBLIC FINAL CREATE PUBLIC .

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
                    onphoto  = client->_event( `CAPTURE` )  ).
      client->view_display( page->stringify( ) ).
    ENDIF.

    IF client->get( )-event = `CAPTURE`.
      "handle mv_picture_base
      "...
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

::: tip **Browser Restrictions**
Depending on browser permissions and configuration settings, access to or use of native device capabilities (NDC) may be restricted. Watch out for any warning messages directly from your browser during testing.
:::
