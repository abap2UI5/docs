---
outline: [2, 4]
---
# Barcode Scanning 

Since UI5 version 1.102, the `sap.ndc.BarcodeScannerButton` control is part of the UI5 library, making barcode scanning simple and straightforward.

#### Scanner Button

The barcode scanner control can be used like any other UI5 control with abap2UI5. Below is a snippet demonstrating the basic functionality. You can add your custom barcode handling after the scanning event is triggered:

```abap
METHOD z2ui5_if_app~main.

    data(lo_view) = z2ui5_cl_xml_view=>factory(
        )->page(
            )->barcode_scanner_button(
                dialogtitle = `Barcode Scanner`
                scansuccess = client->_event( 
                    val   = 'SCAN_SUCCESS' 
                    t_arg = VALUE #( 
                        ( `${$parameters>/text}`   )
                        ( `${$parameters>/format}` ) ) ) ).

    client->view_display( lo_view->stringify( ) ).


    IF client->get( )-event = 'SCAN_SUCCESS'.

        DATA(lv_input)  = client->get_event_arg( 1 ).
        DATA(lv_format) = client->get_event_arg( 2 ).
        "custom processing...
        client->message_box_display( |Scan finished: { lv_input } { lv_format }| ).

    ENDIF.

ENDMETHOD.
```
Check out `z2ui5_cl_demo_app_124` to see barcode scanning in action. If you also need to display barcodes, you can use tools like bwip-js, available as part of the js-libraries addon. More details can be found [here](/addons/ext_js).

::: tip **UI5 Versions**
Please note that this feature is only available when bootstrapping with the UI5 version and does not work with OpenUI5.
:::

#### Device
Most scanner devices have an integrated scanning function. In such cases, you can simply create an input field and ensure the focus is correctly set. The scanned data will populate the input field as if it were typed via a keyboard.
