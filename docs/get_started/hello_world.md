---
outline: [2, 4]
---
# Hello World

### Basic Example
Every abap2UI5 app is an implementation of the `Z2UI5_IF_APP` interface. Create a new class with the following code:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->message_box_display( |Hello World!| ).
    
  ENDMETHOD.
ENDCLASS.
```
Go back to the landing page in your browser and enter `Z2UI5_CL_APP_HELLO_WORLD`. Your app will then launch.

### View Display
Now, let's add our first view to display a simple text:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( 'abap2UI5 - Hello World'
         )->text( text = `My Text`
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Event Handler
Next, we extend the app with a button and an event:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC CREATE PUBLIC.

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
          )->text( text = `My Text`
          )->button( text = 'post' press = client->_event( 'POST' )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Data Exchange
Finally, we add a public attribute and can send data to the backend:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC CREATE PUBLIC.

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
          )->text( text = `My Text`
          )->button( text = 'post' press = client->_event( 'POST' )
          )->input( client->_bind_edit( name )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
And that's it! Set a breakpoint and observe the communication and data updates. Now you can easily play around and further modify the view, event and data exchange.

::: tip **ABAP Language Versions**
While we need to distinguish between Standard ABAP and ABAP for Cloud in the HTTP Handler, the apps themselves are independent. You can decide whether to develop your apps compatible with ABAP Cloud or not.
:::