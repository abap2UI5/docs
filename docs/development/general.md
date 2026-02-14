---
outline: [2, 4]
---
# Controller

abap2UI5 offers great flexibility in how you structure your apps. Most sample applications follow a pattern similar to the one below. You can use it as a starting point, but feel free to adapt it or build a wrapper on top of abap2UI5 for more customized behavior.

The basic idea: every request enters the `main` method, and you use `CASE` to distinguish between initialization, navigation returns, and user events:

```abap
CLASS z2ui5_cl_demo_app_001 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_value  TYPE string.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS display_view.
    METHODS on_event.
    METHODS on_navigated.

  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    CASE abap_true.
      WHEN client->check_on_init( ).
        on_init( ).
        display_view( ).
      WHEN client->check_on_navigated( ).
        on_navigated( ).
      WHEN client->check_on_event( ).
        on_event( ).
    ENDCASE.

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

  METHOD on_event.

    DATA(lt_arg) = client->get_event_arg( ).

    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
        client->message_toast_display( |{ mv_value } - send to the server| ).
    ENDCASE.

  ENDMETHOD.

  METHOD on_init.

    mv_value  = `value`.

  ENDMETHOD.

  METHOD on_navigated.

    DATA(lo_app_prev) = client->get_app_prev( ).

  ENDMETHOD.

ENDCLASS.
```
Refer to the specific sections of this development guide for more details on views, events, data binding, and navigation.
