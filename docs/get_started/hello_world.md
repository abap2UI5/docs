---
outline: [2, 4]
---
# Hello World

### The Interface
Every abap2UI5 app implements the `Z2UI5_IF_APP` interface. It has a single method `main` with one parameter — `client` of type `Z2UI5_IF_CLIENT`:
```abap
INTERFACE z2ui5_if_app PUBLIC.
  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.
ENDINTERFACE.
```
The `client` object is your only access point to the framework. Use it to display views, handle events, exchange data, and navigate between apps.

### Basic Example
Create a new class with the following code:
```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->message_box_display( `Hello World` ).
    
  ENDMETHOD.
ENDCLASS.
```
Head back to the landing page in your browser and enter `ZCL_APP_HELLO_WORLD` to launch your app.

### View Display
Now let's add a view to display a simple text:
```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory(
      )->page( `abap2UI5 - Hello World`
      )->text( `My Text` ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Event Handler
The `main` method is called on every roundtrip — that is, on initialization and after every user interaction (button press, input submit, etc.):

```
┌─────────┐       ┌──────────┐       ┌─────────┐
│ Browser  │──────>│  main()  │──────>│ Browser  │
│ (Start)  │  HTTP │  init    │  HTTP │ (View)   │
└─────────┘       └──────────┘       └────┬─────┘
                                          │ user clicks
┌─────────┐       ┌──────────┐       ┌────┴─────┐
│ Browser  │<──────│  main()  │<──────│ Browser  │
│ (Update) │  HTTP │  event   │  HTTP │ (Event)  │
└─────────┘       └──────────┘       └──────────┘
```

To control what happens when, use `CASE abap_true` to distinguish between lifecycle events:

- `client->check_on_init( )` — first call when the app starts
- `client->check_on_event( )` — user triggered an event (e.g. button press)
- `client->check_on_navigated( )` — returned from another app via navigation

Each `check_*` method returns `abap_true` only for its specific phase, making `CASE abap_true` act as a dispatcher:
```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).

        DATA(view) = z2ui5_cl_xml_view=>factory(
          )->page( `abap2UI5 - Hello World`
          )->text( `My Text`
          )->button( text = `post` press = client->_event( `POST` ) ).
        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `POST` ).

        client->message_box_display( `Hello World!` ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

### Data Exchange
Finally, we add a public attribute and can send data to the backend:
```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).

        DATA(view) = z2ui5_cl_xml_view=>factory( 
          )->page( `abap2UI5 - Hello World` 
          )->text( `My Text` 
          )->button(
            text  = `post`
            press = client->_event( `POST` )
          )->input( client->_bind_edit( name ) ).
        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `POST` ).

        client->message_box_display( |Your name is { name }.| ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```
That's all you need. Set a breakpoint to observe the communication and data updates in action, then experiment with modifying the view, events, and data exchange.

::: tip **ABAP Language Versions**
While you need to distinguish between Standard ABAP and ABAP for Cloud in the HTTP Handler, the apps themselves are independent. You're free to decide whether to develop your apps with ABAP Cloud compatibility or not.
:::
