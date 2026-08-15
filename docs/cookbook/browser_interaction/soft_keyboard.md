---
outline: [2, 4]
---
# Soft Keyboard

#### Hide Soft Keyboard

For UI5 input fields, the soft keyboard pops up automatically when an input receives focus. Sometimes — for example, in warehouses with small devices used mainly for barcode scanning — you don't want this behavior.

The `keyboard_set_mode` frontend event sets the HTML `inputmode` attribute on a UI5 input. Pass the control id and the desired mode (`none` hides the soft keyboard; `text`, `numeric`, `decimal`, `tel`, etc. restore it with the matching layout).

```abap
METHOD z2ui5_if_app~main.

    DATA input TYPE string.

    IF client->check_on_init( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
              )->a( n = `xmlns:form` v = `sap.ui.layout.form`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title`          v = `abap2UI5 - Softkeyboard on/off`
                      )->a( n = `navButtonPress` v = client->_event( `BACK` )
                      )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                      )->ele( n = `SimpleForm` ns = `form`
                          )->a( n = `editable` b = abap_true

                          )->ele( n = `content` ns = `form`
                              )->tag( `Title`
                                  )->a( n = `text` v = `Keyboard on/off`
                              )->tag( `Label`
                                  )->a( n = `text` v = `Input`
                              )->tag( `Input`
                                  )->a( n = `id`               v = `ZINPUT`
                                  )->a( n = `value`            v = client->_bind( input )
                                  )->a( n = `valueHelpRequest` v = client->_event( `CALL_KEYBOARD` )
                                  )->a( n = `valueHelpIconSrc` v = `sap-icon://keyboard-and-mouse`
                                  )->a( n = `showValueHelp`    b = abap_true ).

      client->view_display( view->stringify( ) ).

      RETURN.
    ENDIF.

    CASE client->get( )-event.
      WHEN `CALL_KEYBOARD`.
        client->follow_up_action( val   = client->cs_event-keyboard_set_mode
                        t_arg = VALUE #( ( `ZINPUT` ) ( `none` ) ) ).
      WHEN `BACK`.
        client->nav_app_leave( ).
    ENDCASE.

ENDMETHOD.
```

To re-enable the keyboard, fire the same event with a different mode (`text`, `numeric`, …).
