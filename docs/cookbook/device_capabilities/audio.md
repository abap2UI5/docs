---
outline: [2, 4]
---
# Audio

## Play Sounds

Audio feedback is handy in some scenarios. Fire the `play_audio` frontend event with the URL of a sound file — for example a `.wav` from the SAP MIME repository at `/SAP/PUBLIC/BC/ABAP/mime_demo/bam.wav`.

The example below is a typical input-validation beep: when the user submits an empty Company Code, the app plays an alert sound and shows an error message; a filled input is simply accepted and cleared:

```abap
CLASS z2ui5_cl_sample_sound DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA company_code TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_sound IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ) OR client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->ele( `VBox`
                      )->tag( `Input`
                          )->a( n = `id`          v = `inputApp`
                          )->a( n = `value`       v = client->_bind( company_code )
                          )->a( n = `type`        v = `Number`
                          )->a( n = `placeholder` v = `Company Code`
                          )->a( n = `submit`      v = client->_event( `CHECK_INPUT` )
                      )->tag( `Button`
                          )->a( n = `text`  v = `check`
                          )->a( n = `press` v = client->_event( `CHECK_INPUT` ) ).

      client->view_display( view->stringify( ) ).

      RETURN.
    ENDIF.

    IF client->get( )-event = `CHECK_INPUT`.
      IF company_code IS INITIAL.
        client->follow_up_action( val   = client->cs_event-play_audio
                        t_arg = VALUE #( ( `/SAP/PUBLIC/BC/ABAP/mime_demo/bam.wav` ) ) ).
        client->message_box_display( type = `error` text = `Input is empty!` ).
      ELSE.
        CLEAR company_code.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
For a complete sound sample, see `Z2UI5_CL_SMPS_APP_487` in the [samples-stack repository](https://github.com/abap2UI5/samples-stack).
