---
outline: [2, 4]
---
# Popup

UI5 offers popups that overlay specific parts of the view. This section walks through building them in abap2UI5.

## General

To show a popup, call `client->popup_display` instead of `client->view_display`:
```abap
  METHOD z2ui5_if_app~main.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `FragmentDefinition` ns = `core`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:core` v = `sap.ui.core`

            )->ele( `Dialog`
                )->a( n = `title` v = `Popup - Info`

                )->tag( `Text`
                    )->a( n = `text` v = `this is information shown in a popup` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.
```

## Flow Logic
A typical popup flow shows a normal view, opens a popup, and finally closes it. Structure it like this:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`     v = `sap.m`
                )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

                )->ele( `Page`
                    )->a( n = `title` v = `abap2UI5 - Popups`

                    )->tag( `Button`
                        )->a( n = `text`  v = `popup rendering, no background rendering`
                        )->a( n = `press` v = client->_event( `POPUP_OPEN` ) ).

        client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN `POPUP_OPEN`.
        DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `FragmentDefinition` ns = `core`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:core` v = `sap.ui.core`

                )->ele( `Dialog`
                    )->a( n = `title` v = `Popup`

                    )->tag( `Text`
                        )->a( n = `text` v = `this is a text in a popup`
                    )->tag( `Button`
                        )->a( n = `text`  v = `close`
                        )->a( n = `press` v = client->_event( `POPUP_CLOSE` ) ).

        client->popup_display( popup->stringify( ) ).


      WHEN `POPUP_CLOSE`.
        client->popup_destroy( ).

    ENDCASE.

  ENDMETHOD.
```

The popup has the same lifecycle as the main view: `popup_display( )` renders the XML and `popup_destroy( )` closes it. Changed ABAP data needs neither — the framework pushes the delta into the already-rendered popup with the response. (`popup_model_update( )` used to be how you asked for that; it is a no-op now and is on the removal list.)


## Separated App
For a cleaner source layout, encapsulate popups in separate classes and call them via [navigation](/cookbook/event_navigation/navigation).

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

To handle multiple stacked popups, note that abap2UI5 shows only one popup at a time on the frontend. But you can keep a popup stack in your backend logic and re-display the previous popup as needed. See `Z2UI5_CL_SMP_APP_161`.
