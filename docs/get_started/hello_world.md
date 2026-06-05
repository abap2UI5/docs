---
outline: [2, 4]
---
# Hello World

Just copy the following class into your system:

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

::: tip **ABAP Language Versions**
While the HTTP handler has to distinguish between Standard ABAP and ABAP for Cloud, the apps themselves are independent. You're free to choose whether to build your apps with ABAP Cloud compatibility.
:::

### View Display
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

### Info
abap2UI5 follows a thin-frontend model: the browser only renders UI5 views, while all logic, state, and data handling stay in ABAP on the server. Three ideas to keep in mind before writing code:

- **One method, many calls.** The framework calls your app's `main` method on every roundtrip — on the initial start *and* after every user interaction (button press, input change, navigation).
- **State lives in your class.** Public attributes of your app class hold data between roundtrips; abap2UI5 serializes and restores them for you, so you don't manage sessions manually.
- **The `client` object is your only API.** Use it to display views, check which event fired, bind attributes to UI5 controls, and trigger navigation.

Every abap2UI5 app implements the `z2ui5_if_app` interface. It has a single method, `main`, with one parameter: `client` of type `z2ui5_if_client`:
```abap
INTERFACE z2ui5_if_app PUBLIC.
  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.
ENDINTERFACE.
```
The `client` object is your only entry point into the framework. Use it to show views, handle events, share data, and navigate between apps.

→ *For a deeper look at the lifecycle and framework internals, see [How It All Works](/technical/how_it_all_works) and [Concept](/technical/concept).*

### Events

Let's extend the code with some event handling:
```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

        DATA(view) = z2ui5_cl_xml_view=>factory(
          )->page( `abap2UI5 - Hello World`
          )->text( `My Text`
          )->button( text = `post` press = client->_event( `POST` ) ).
        client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POST` ).

        client->message_box_display( `Hello World!` ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

### Info
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

To distinguish between lifecycle events, check for the following events:

- `client->check_on_init( )` — first call when the app starts
- `client->check_on_event( )` — user triggered an event (e.g., button press)

Each `check_*` method returns `abap_true` only for its specific phase, so is acts as a dispatcher:

### Data Flow
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

### Jump into the Code
Press `Ctrl+F12` in any running app to open the source code, view, and model side by side:
![Source code viewer opened with Ctrl+F12 showing code, view, and model](/get_started/image-2.png)


