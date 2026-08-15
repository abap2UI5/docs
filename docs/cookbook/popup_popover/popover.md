---
outline: [2, 4]
---
# Popover

To show a popover, call `client->popover_display` and pass the ID of the control the popover should attach to:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `Popover Example`

                      )->tag( `Button`
                          )->a( n = `id`    v = `TEST`
                          )->a( n = `text`  v = `display popover`
                          )->a( n = `press` v = client->_event( `POPOVER_OPEN` ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN `POPOVER_OPEN`.
        DATA(popover) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `FragmentDefinition` ns = `core`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:core` v = `sap.ui.core`

                )->ele( `Popover`
                    )->a( n = `placement` v = `Left`

                    )->tag( `Text`
                        )->a( n = `text` v = `this is a popover`
                    )->tag( `Button`
                        )->a( n = `id`    v = `my_id`
                        )->a( n = `text`  v = `close`
                        )->a( n = `press` v = client->_event( `POPOVER_CLOSE` ) ).

        client->popover_display(
            xml   = popover->stringify( )
            by_id = `TEST` ).

      WHEN `POPOVER_CLOSE`.
        client->popover_destroy( ).
    ENDCASE.

  ENDMETHOD.
```

Like popups, popovers support a data-only refresh: `popover_model_update( )` pushes changed ABAP values into the open popover without re-rendering its XML.
