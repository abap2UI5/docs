---
outline: [2, 4]
---
# Popups, Popovers

UI5 provides popups and popovers that overlay specific areas of the view. This section shows how to build them in abap2UI5.

### Popup

#### General

To display a popup, call `client->popup_display` instead of `client->view_display`:
```abap
  METHOD z2ui5_if_app~main.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup(
        )->dialog( `Popup - Info`
          )->text( `this is information shown in a popup` ).
    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.
```

#### Flow Logic
A common popup flow displays a normal view, shows a popup, and finally closes it. Here's how to structure it:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
        DATA(view) = z2ui5_cl_xml_view=>factory(
            )->page( `abap2UI5 - Popups`
                )->button(
                    text  = `popup rendering, no background rendering`
                    press = client->_event( `POPUP_OPEN` ) ).
        client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN `POPUP_OPEN`.
        DATA(popup) = z2ui5_cl_xml_view=>factory_popup(
            )->dialog( `Popup`
                )->text( `this is a text in a popup`
                )->button(
                    text  = `close`
                    press = client->_event( `POPUP_CLOSE` ) ).
        client->popup_display( popup->stringify( ) ).

      WHEN `POPUP_CLOSE`.
        client->popup_destroy( ).

    ENDCASE.

  ENDMETHOD.
```

#### Separated App
For cleaner source structure, encapsulate popups in separate classes and call them through [navigation](/development/navigation/navigation).

For example, check out the confirmation popup:
```abap
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        DATA(popup) = z2ui5_cl_pop_to_confirm=>factory(
                          i_question_text = `Can you confirm this?`
                          i_event_confirm = `CONFIRM`
                          i_event_cancel  = `CANCEL` ).
        client->nav_app_call( popup ).

      WHEN client->check_on_event( `CONFIRM` ).
        client->message_box_display( `the result is confirmed` ).

      WHEN client->check_on_event( `CANCEL` ).
        client->message_box_display( `the result is rejected` ).

    ENDCASE.

  ENDMETHOD.
```

To manage a stack of multiple popups, remember that abap2UI5 displays only one popup at a time on the frontend. But you can maintain a popup stack in your backend logic and re-display the previous popup as needed. Check out `Z2UI5_CL_DEMO_APP_161`.

### Popover
To display a popover, call `client->popover_display` and specify the ID of the control where you want the popover to appear:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      DATA(view) = z2ui5_cl_xml_view=>factory(
        )->shell(
            )->page( `Popover Example`
                )->button(
                    text  = `display popover`
                    press = client->_event( `POPOVER_OPEN` )
                    id    = `TEST` ).
      client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN `POPOVER_OPEN`.
        DATA(popover) = z2ui5_cl_xml_view=>factory_popup(
            )->popover( placement = `Left`
                )->text( `this is a popover`
                )->button(
                    id    = `my_id`
                    text  = `close`
                    press = client->_event( `POPOVER_CLOSE` ) ).
        client->popover_display(
            xml   = popover->stringify( )
            by_id = `TEST` ).

      WHEN `POPOVER_CLOSE`.
        client->popover_destroy( ).
    ENDCASE.

  ENDMETHOD.
```

### Built-in Popups
Several pre-built popup classes cover common cases:

- `Z2UI5_CL_POP_ERROR`
- `Z2UI5_CL_POP_FILE_DL`
- `Z2UI5_CL_POP_FILE_UL`
- `Z2UI5_CL_POP_GET_RANGE`
- `Z2UI5_CL_POP_GET_RANGE_M`
- `Z2UI5_CL_POP_HTML`
- `Z2UI5_CL_POP_INPUT_VAL`
- `Z2UI5_CL_POP_ITAB_JSON_DL`
- `Z2UI5_CL_POP_JS_LOADER`
- `Z2UI5_CL_POP_MESSAGES`
- `Z2UI5_CL_POP_PDF`
- `Z2UI5_CL_POP_TABLE`
- `Z2UI5_CL_POP_TEXTEDIT`
- `Z2UI5_CL_POP_TO_CONFIRM`
- `Z2UI5_CL_POP_TO_INFORM`
- `Z2UI5_CL_POP_TO_SELECT`

Help grow this collection to cover more use cases — contributions are always welcome!
