---
outline: [2, 4]
---
# Popups, Popovers

UI5 offers functionality for displaying popups and popovers that overlay only specific areas of the view. Let’s see how this can be implemented using abap2UI5.

### Popup

#### General

To display a popup, use the method `client->popup_display` instead of `client->view_display`:
```abap
METHOD z2ui5_if_app~main.

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup(
        )->dialog( 'Popup - Info'
            )->text( 'this is an information shown in a popup' ).
    client->popup_display( lo_popup->stringify( ) ).

ENDMETHOD.
```

#### Flow Logic
A common flow for using popups typically involves displaying a normal view, then showing a popup, and finally closing it. Here’s how to structure this:
```abap
METHOD Z2UI5_if_app~main.

    IF client->check_on_init( ).
        DATA(lo_view) = z2ui5_cl_xml_view=>factory(
            )->page( 'abap2UI5 - Popups'
                )->button(
                    text  = 'popup rendering, no background rendering'
                    press = client->_event( 'POPUP_OPEN' ) ).
        client->view_display( lo_view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN 'POPUP_OPEN'.
        DATA(lo_popup) = Z2UI5_cl_xml_view=>factory_popup( 
            )->dialog( 'Popup'
                )->text( 'this is a text in a popup'
                )->button(
                    text  = 'close'
                    press = client->_event( 'POPUP_CLOSE' ) ).
        client->popup_display( lo_popup->stringify( ) ).

      WHEN 'POPUP_CLOSE'.
        client->popup_destroy( ).
        
    ENDCASE.

ENDMETHOD.
```

#### Separated App
For better source code structure, it's possible to encapsulate popups in separate classes and call them through [navigation](/development/navigation).

#### Call Stack
If you need to manage a stack of multiple popups, remember that abap2UI5 displays only one popup at a time on the frontend. However, you can maintain a popup stack in your backend logic and re-display the previous popup as needed. Check out `Z2UI5_CL_DEMO_APP_161`.

### Popover
To display a popover, use the method `client->popover_display` and specify the ID of the control where you want the popover to appear:
 ```abap
METHOD Z2UI5_if_app~main.

    IF client->check_on_init( ).
      DATA(view) = z2ui5_cl_xml_view=>factory( 
        )->shell(
            )->page( 'Popover Example'
                )->button(
                    text  = 'display popover'
                    press = client->_event( 'POPOVER_OPEN' )
                    id    = 'TEST' ).
      client->view_display( view->stringify( ) ).
    ENDIF.

    CASE client->get( )-event.

      WHEN 'POPOVER_OPEN'.
        DATA(popover) = Z2UI5_cl_xml_view=>factory_popup(
            )->popover( placement = 'Left'
                )->text( `this is a popover`
                )->button(
                    id    = `my_id`
                    text  = `close`
                    press = client->_event( `POPOVER_CLOSE` ) ).
        client->popover_display(
            xml   = view->stringify( )
            by_id = `my_id` ).

      WHEN 'POPOVER_CLOSE'.
        client->popover_destroy( ).
    ENDCASE.

ENDMETHOD.
 ```

### Built-in Popups
Several pre-built popup classes are available for specific scenarios:

* z2ui5_cl_pop_error
* z2ui5_cl_pop_file_dl
* z2ui5_cl_pop_file_ul
* z2ui5_cl_pop_get_range
* z2ui5_cl_pop_get_range_m
* z2ui5_cl_pop_html
* z2ui5_cl_pop_input_val
* z2ui5_cl_pop_itab_json_dl
* z2ui5_cl_pop_js_loader
* z2ui5_cl_pop_messages
* z2ui5_cl_pop_pdf
* z2ui5_cl_pop_table
* z2ui5_cl_pop_textedit
* z2ui5_cl_pop_to_confirm
* z2ui5_cl_pop_to_inform
* z2ui5_cl_pop_to_select

This collection can be further expanded to cover additional common use cases. Contributions are always welcome!
