---
outline: [2, 4]
---
# Init

`client->check_on_init` returns `abap_true` on the first request. Use this branch to initialize app data and render the initial view.

```abap
  METHOD z2ui5_if_app~main.

    me->client = client.
    CASE abap_true.
      WHEN client->check_on_init( ).
        on_init( ).
        display_view( ).
    ENDCASE.

  ENDMETHOD.

  METHOD on_init.

    mv_value  = `value`.

  ENDMETHOD.

  METHOD display_view.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell( )->page(
        )->simple_form( title = `Form Title` editable = abap_true
                   )->content( `form`
                       )->title( `Input`
                       )->label( `value`
                       )->input( client->_bind_edit( mv_value )
                       )->button(
                           text  = `post`
                           press = client->_event( `BUTTON_POST` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
```
