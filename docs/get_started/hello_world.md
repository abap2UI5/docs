---
outline: [2, 3]
---
# Hello World

This page walks through a minimal abap2UI5 app step by step — the interface, a view, an event, and data flow. By the end, you have all the pieces you need to start building.

## The Interface
Every abap2UI5 app implements the `z2ui5_if_app` interface. It has a single method, `main`, with one parameter: `client` of type `z2ui5_if_client`:
```abap
INTERFACE z2ui5_if_app PUBLIC.
  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.
ENDINTERFACE.
```
The `client` object is your only entry point into the framework. Use it to show views, handle events, share data, and navigate between apps.

## Basic Example
Build a class:
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
Go back to the landing page in your browser and enter `ZCL_APP_HELLO_WORLD` to launch your app.

## View Display
Let's add a view to show some text:
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

## Event Handler
The framework calls the `main` method on every roundtrip — on initialization and after every user interaction (button press, input submit, etc.):

```text
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

To distinguish between lifecycle events, use `CASE abap_true`:

- `client->check_on_init( )` — first call when the app starts
- `client->check_on_event( )` — user triggered an event (e.g., button press)
- `client->check_on_navigated( )` — returned from another app via navigation

Each `check_*` method returns `abap_true` only for its specific phase, so `CASE abap_true` acts as a dispatcher:
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

## Data Flow
Finally, add a public attribute to send data to the backend:
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
That's all you need. Set a breakpoint to watch the communication and data updates in action, then try changing the view, events, and data flow.

::: tip ABAP Language Versions
While the HTTP handler has to distinguish between Standard ABAP and ABAP for Cloud, the apps themselves are independent. You're free to choose whether to build your apps with ABAP Cloud compatibility.
:::

## Next Steps

- **[Sample Apps](/get_started/samples)** — 250+ ready-made examples covering tables, forms, popups, navigation, and more.
- **[Cookbook](/development/general)** — go deeper into the controller pattern, views, models, events, and navigation.
- **[What's Next?](/get_started/next)** — pointers for production-ready apps, configuration, and add-ons.
