---
outline: [2, 4]
---
# Soft Keyboard

::: warning This page still shows the previous view builder
The examples below build views with `z2ui5_cl_xml_view`. That class is frozen:
it still runs, and your existing apps keep working — but it is no longer the
one to write new code against. The current builder is
`z2ui5_cl_ui5_view_builder`, and it has four verbs instead of a control per
method, which makes every UI5 control available rather than the curated set.

See [View → Definition](/cookbook/view/definition) for what the chain looks
like, and [Deprecations](/resources/deprecations) for the translation.
:::

#### Hide Soft Keyboard

For UI5 input fields, the soft keyboard pops up automatically when an input receives focus. Sometimes — for example, in warehouses with small devices used mainly for barcode scanning — you don't want this behavior.

The `keyboard_set_mode` frontend event sets the HTML `inputmode` attribute on a UI5 input. Pass the control id and the desired mode (`none` hides the soft keyboard; `text`, `numeric`, `decimal`, `tel`, etc. restore it with the matching layout).

```abap
METHOD z2ui5_if_app~main.

    DATA input TYPE string.

    IF client->check_on_init( ).

      DATA(page) = z2ui5_cl_xml_view=>factory( )->shell(
             )->page( title          = `abap2UI5 - Softkeyboard on/off`
                      navbuttonpress = client->_event( `BACK` )
                      shownavbutton  = client->check_app_prev_stack( )
             )->simple_form( editable = abap_true
                 )->content( `form`
                     )->title( `Keyboard on/off`
                     )->label( `Input`
                     )->input( id               = `ZINPUT`
                               value            = client->_bind( input )
                               showvaluehelp    = abap_true
                               valuehelprequest = client->_event( `CALL_KEYBOARD` )
                               valuehelpiconsrc = `sap-icon://keyboard-and-mouse` ).

      client->view_display( page->stringify( ) ).
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
