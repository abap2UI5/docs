---
outline: [2, 4]
---
# Popover

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
