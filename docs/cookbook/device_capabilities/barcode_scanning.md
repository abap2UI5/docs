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

## Scanning

Since UI5 version 1.102, the `sap.ndc.BarcodeScannerButton` control is part of the UI5 library, making barcode scanning easy. Use it like any other UI5 control with abap2UI5. The example below shows the basic behavior — customize the handling once the scanning event fires:

```abap
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
            )->a( n = `xmlns:ndc` v = `sap.ndc`

            )->ele( `Page`
                )->tag( n = `BarcodeScannerButton` ns = `ndc`
                    )->a( n = `dialogTitle` v = `Barcode Scanner`
                    )->a( n = `scanSuccess` v = client->_event(
                                                   val   = `SCAN_SUCCESS`
                                                   t_arg = VALUE #(
                                                       ( `${$parameters>/text}`   )
                                                       ( `${$parameters>/format}` ) ) ) ).

    client->view_display( view->stringify( ) ).

    IF client->get( )-event = `SCAN_SUCCESS`.

        DATA(lv_input)  = client->get_event_arg( ).
        DATA(lv_format) = client->get_event_arg( 2 ).
        "custom processing...
        client->message_box_display( |Scan finished: { lv_input } { lv_format }| ).
    ENDIF.

ENDMETHOD.
```

::: tip **UI5 Versions**
This feature works only with the UI5 framework, not with OpenUI5.
:::

## Focus Handling
Most scanner devices emulate a keyboard. In that case, add an input field and move the focus from the backend — the scanned data flows into the input as if typed.

The example below moves focus from one field to the next after each Enter key press by firing the `set_focus` frontend event:

```abap
CLASS z2ui5_cl_sample_focus DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA one TYPE string.
    DATA two TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_focus IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
              )->a( n = `xmlns:form` v = `sap.ui.layout.form`

              )->ele( `Page`
                  )->ele( n = `SimpleForm` ns = `form`
                      )->ele( n = `content` ns = `form`
                          )->tag( `Label`
                              )->a( n = `text` v = `One`
                          )->tag( `Input`
                              )->a( n = `id`     v = `id1`
                              )->a( n = `value`  v = client->_bind( one )
                              )->a( n = `submit` v = client->_event( `ONE_ENTER` )
                          )->tag( `Label`
                              )->a( n = `text` v = `Two`
                          )->tag( `Input`
                              )->a( n = `id`     v = `id2`
                              )->a( n = `value`  v = client->_bind( two )
                              )->a( n = `submit` v = client->_event( `TWO_ENTER` ) ).

      client->view_display( view->stringify( ) ).
      RETURN.
    ENDIF.

    CASE client->get( )-event.
      WHEN `ONE_ENTER`.
        client->follow_up_action( val   = client->cs_event-set_focus
                        t_arg = VALUE #( ( `id2` ) ) ).
      WHEN `TWO_ENTER`.
        client->follow_up_action( val   = client->cs_event-set_focus
                        t_arg = VALUE #( ( `id1` ) ) ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

## Play Sounds

Audio feedback is handy in some scenarios. Fire the `play_audio` frontend event with the URL of a sound file — for example a `.wav` from the SAP MIME repository at `/SAP/PUBLIC/BC/ABAP/mime_demo/bam.wav`:

```abap
CLASS z2ui5_cl_sample_sound DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA company_code TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_sound IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

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

## Render Barcodes
To also render barcodes, use bwip-js, which ships with the js-libraries add-on. See [Add-ons](/advanced/addons) for details.
