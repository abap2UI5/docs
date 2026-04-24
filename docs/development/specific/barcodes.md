---
outline: [2, 4]
---
# Barcode Scanning

Barcode scanning is common in enterprise apps. With abap2UI5, you can:
- Scan barcodes
- Handle focus transitions
- Play sounds for user feedback
- Display barcodes

This section covers everything you need to get started.

#### Scanning

Since UI5 version 1.102, the `sap.ndc.BarcodeScannerButton` control is part of the UI5 library, making barcode scanning simple. Use it like any other UI5 control with abap2UI5. The example below shows basic behavior — customize the handling after the scanning event fires:

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
For barcode scanning in action, see the `Z2UI5_CL_DEMO_APP_124` sample application.

::: tip **UI5 Versions**
This feature works only with the UI5 framework, not with OpenUI5.
:::

#### Focus Handling
Most scanner devices emulate a keyboard. In those cases, create an input field and set focus correctly — the scanned data fills the input as if typed on a keyboard.

The key mechanism is the `_z2ui5()->focus()` custom control, which accepts a bound `focus_id` attribute. When the user presses Enter (firing the `submit` event), the backend updates `focus_id` to the next input field's ID and calls `view_model_update` — the framework then moves focus to the matching field on the frontend automatically:

An example that handles input focus and manages transitions between fields after scanning and pressing Enter:

```abap
CLASS z2ui5_cl_sample_focus DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA one      TYPE string.
    DATA two      TYPE string.
    DATA focus_id TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_focus IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      focus_id = `id1`.

      DATA(page) = z2ui5_cl_xml_view=>factory( )->page( ).
      page->simple_form(
         )->content( ns = `form`
         )->label( `One`
         )->input(
              id = `id1`
              value = client->_bind_edit( one )
              submit = client->_event( |one_enter| )
         )->label( `Two`
         )->input(
              id = `id2`
              value = client->_bind_edit( two )
              submit = client->_event( |two_enter| ) ).

      page->_z2ui5( )->focus( client->_bind( focus_id ) ).
      client->view_display( page->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.
      WHEN `one_enter`.
        focus_id = `id2`.
        client->view_model_update( ).
      WHEN `two_enter`.
        focus_id = `id1`.
        client->view_model_update( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

#### Play Sounds

Audio feedback helps in specific contexts. The example below plays a sound when a user fails to scan a value. The sound is a .wav file in the SAP MIME repository at `/SAP/PUBLIC/BC/ABAP/mime_demo/bam.wav`:

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
      view->_generic( name = `script`
                      ns   = `html` )->_cc_plain_xml(
                          |function playSound() \{ new Audio("/SAP/PUBLIC/BC/ABAP/mime_demo/bam.wav").play(); \}| ).

      DATA(vbox) = view->page( )->vbox( ).
      vbox->input( id          = `inputApp`
                   value       = client->_bind_edit( company_code )
                   type        = `Number`
                   placeholder = `Company Code`
                   submit      = client->_event( `CUSTOM_JS_FROM_EB` ) ).
      vbox->button( text  = `call custom JS from EB`
                    press = client->_event( `CUSTOM_JS_FROM_EB` ) ).

      client->view_display( view->stringify( ) ).
    ENDIF.

    IF client->get( )-event = `CUSTOM_JS_FROM_EB`.
      IF company_code IS INITIAL.
        client->follow_up_action( val = `playSound()` ).
        client->message_box_display( type = `error` text = `Input is empty!` ).
      ELSE.
        CLEAR company_code.
      ENDIF.
      client->view_model_update( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
For a sample with sounds in action, see `Z2UI5_CL_DEMO_APP_304`.

#### Display Barcodes
To also display barcodes, use bwip-js, which ships as part of the js-libraries add-on. See [Add-ons](/resources/addons) for details.
