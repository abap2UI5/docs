---
outline: [2, 4]
---
# Audio

#### Play Sounds

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

    IF client->check_on_init( ).

      DATA(view) = z2ui5_cl_xml_view=>factory( ).
      DATA(vbox) = view->page( )->vbox( ).
      vbox->input( id          = `inputApp`
                   value       = client->_bind( company_code )
                   type        = `Number`
                   placeholder = `Company Code`
                   submit      = client->_event( `CHECK_INPUT` ) ).
      vbox->button( text  = `check`
                    press = client->_event( `CHECK_INPUT` ) ).

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
      client->view_model_update( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
For a complete sound sample, see `Z2UI5_CL_DEMO_APP_S_03`.
