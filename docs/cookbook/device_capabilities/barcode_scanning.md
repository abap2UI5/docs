---
outline: [2, 4]
---
# Barcode Scanning

Barcode scanning is common in enterprise apps. With abap2UI5, you can:
- Scan barcodes
- Handle focus transitions
- Play sounds for user feedback
- Display barcodes

This section walks through what you need to get started.

#### Scanning

Since UI5 version 1.102, the `sap.ndc.BarcodeScannerButton` control is part of the UI5 library, making barcode scanning easy. Use it like any other UI5 control with abap2UI5. The example below shows the basic behavior — customize the handling once the scanning event fires:

```abap
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory(
        )->page(
            )->barcode_scanner_button(
                dialogtitle = `Barcode Scanner`
                scansuccess = client->_event(
                    val   = `SCAN_SUCCESS`
                    t_arg = VALUE #(
                        ( `${$parameters>/text}`   )
                        ( `${$parameters>/format}` ) ) ) ).

    client->view_display( view->stringify( ) ).

    IF client->get( )-event = `SCAN_SUCCESS`.

        DATA(lv_input)  = client->get_event_arg( 1 ).
        DATA(lv_format) = client->get_event_arg( 2 ).
        "custom processing...
        client->message_box_display( |Scan finished: { lv_input } { lv_format }| ).
    ENDIF.

ENDMETHOD.
```
To see barcode scanning in action, check the `Z2UI5_CL_DEMO_APP_124` sample app.

::: tip **UI5 Versions**
This feature works only with the UI5 framework, not with OpenUI5.
:::

#### Focus Handling
Most scanner devices emulate a keyboard. In that case, add an input field and move the focus from the backend — the scanned data flows into the input as if typed.

The example below moves focus from one field to the next after each Enter key press by firing the `set_focus` frontend event:

```abap
CLASS z2ui5_cl_sample_focus DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA one TYPE string.
    DATA two TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_focus IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DATA(page) = z2ui5_cl_xml_view=>factory( )->page( ).
      page->simple_form(
         )->content( ns = `form`
         )->label( `One`
         )->input(
              id     = `id1`
              value  = client->_bind_edit( one )
              submit = client->_event( `ONE_ENTER` )
         )->label( `Two`
         )->input(
              id     = `id2`
              value  = client->_bind_edit( two )
              submit = client->_event( `TWO_ENTER` ) ).

      client->view_display( page->stringify( ) ).
      RETURN.
    ENDIF.

    CASE client->get( )-event.
      WHEN `ONE_ENTER`.
        client->action( val   = client->cs_event-set_focus
                        t_arg = VALUE #( ( `id2` ) ) ).
      WHEN `TWO_ENTER`.
        client->action( val   = client->cs_event-set_focus
                        t_arg = VALUE #( ( `id1` ) ) ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

#### Play Sounds

Audio feedback is handy in some scenarios. Fire the `play_audio` frontend event with the URL of a sound file — for example a `.wav` from the SAP MIME repository at `/SAP/PUBLIC/BC/ABAP/mime_demo/bam.wav`:

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
                   value       = client->_bind_edit( company_code )
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
        client->action( val   = client->cs_event-play_audio
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
For a complete sound sample, see `Z2UI5_CL_DEMO_APP_304`.

#### Render Barcodes
To also render barcodes, use bwip-js, which ships with the js-libraries add-on. See [Add-ons](/advanced/addons) for details.
