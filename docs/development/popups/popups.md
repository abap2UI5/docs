---
outline: [2, 4]
---
# Popups

UI5 offers popups that overlay specific parts of the view. This section walks through building them in abap2UI5.

## General

To show a popup, call `client->popup_display` instead of `client->view_display`:
```abap
  METHOD z2ui5_if_app~main.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup(
        )->dialog( `Popup - Info`
          )->text( `this is information shown in a popup` ).
    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.
```

## Flow Logic
A typical popup flow shows a normal view, opens a popup, and finally closes it. Structure it like this:
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

## Separated App
For a cleaner source layout, encapsulate popups in separate classes and call them via [navigation](/development/navigation/navigation).

An example with the confirmation popup:
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

To handle multiple stacked popups, note that abap2UI5 shows only one popup at a time on the frontend. But you can keep a popup stack in your backend logic and re-display the previous popup as needed. See `Z2UI5_CL_DEMO_APP_161`.
