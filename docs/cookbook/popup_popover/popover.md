---
outline: [2, 4]
---
# Popover

::: warning This page still shows the previous view builder
The examples below build views with `z2ui5_cl_xml_view`. That class is frozen:
it still runs, and your existing apps keep working — but it is no longer the
one to write new code against. The current builder is
`z2ui5_cl_ui5_view_builder`, and it has four verbs instead of a control per
method, which makes every UI5 control available rather than the curated set.

See [View → Definition](/cookbook/view/definition) for what the chain looks
like, and [Deprecations](/resources/deprecations) for the translation.
:::

To show a popover, call `client->popover_display` and pass the ID of the control the popover should attach to:
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

Like popups, popovers support a data-only refresh: `popover_model_update( )` pushes changed ABAP values into the open popover without re-rendering its XML.
