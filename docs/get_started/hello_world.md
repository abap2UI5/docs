---
outline: [2, 4]
---
# Hello World

### Basic Example
Every abap2UI5 app is an implementation of the `z2ui5_if_app` interface. Create a new class with the following code:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    client->message_box_display( |Hello World!| ).
  ENDMETHOD.
ENDCLASS.
```
Go back to the landing page in your browser and enter `z2ui5_cl_app_hello_world`. Your first app will then launch.

### View
Now, let's add our first view to display a simple form:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
         )->simple_form( )->content( ns = `form`
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Event Handling
Next, we extend the app with an event:
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
        client->message_box_display( |Hello World!| ).
        RETURN.
    ENDCASE.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
         )->simple_form( )->content( ns = `form`
            )->button( text = 'post' press = client->_event( 'POST' )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Data Exchange
Finally, we send data to the backend:
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
        RETURN.
    ENDCASE.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
         )->simple_form( )->content( ns = `form`
            )->label( 'Name'
            )->input( client->_bind_edit( name )
            )->button( text = 'post' press = client->_event( 'POST' )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
And that's it! Set a breakpoint and observe the communication and data updates. Now you can easily play around and further modify the view, event and data exchange.

::: tip **ABAP Language Versions**
While we need to distinguish between Standard ABAP and ABAP for Cloud in the HTTP Handler, the apps themselves are independent. You can decide whether to develop your apps compatible with ABAP Cloud or not.
:::