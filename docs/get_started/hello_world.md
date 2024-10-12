# Create your first App

Create a new class and implement the abap2UI5 interface or just copy&paste this snippet:

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    CASE client->get( )-event.
      WHEN 'POST'.
        client->message_box_display( |Your name is { name }.| ).
    ENDCASE.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
         )->simple_form( )->content( ns = `form`
            )->title( 'Input here and send it to the server...'
            )->label( 'Name'
            )->input( client->_bind_edit( name )
            )->button( text = 'post' press = client->_event( 'POST' )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```