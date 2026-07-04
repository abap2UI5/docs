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
Open the abap2UI5 startup page in your browser (the same page as in the [Quickstart](/get_started/quickstart)), enter the class name `ZCL_APP_HELLO_WORLD` in the input field, and launch your app.

::: tip **ABAP Language Versions**
While the HTTP handler has to distinguish between Standard ABAP and ABAP for Cloud, the apps themselves are independent. You're free to choose whether to build your apps with ABAP Cloud compatibility.
:::

### How Apps Work
abap2UI5 follows a thin-frontend model: the browser only renders UI5 views, while all logic, state, and data handling stay in ABAP on the server. Three ideas to keep in mind before writing code:

- **One method, many calls.** The framework calls your app's `main` method on every roundtrip — on the initial start *and* after every user interaction (button press, input change, navigation).
- **State lives in your class.** Public attributes of your app class hold data between roundtrips; abap2UI5 serializes and restores them for you, so you don't manage sessions manually.
- **The `client` object is your only API.** Use it to display views, check which event fired, bind attributes to UI5 controls, and trigger navigation.

Every abap2UI5 app implements the `z2ui5_if_app` interface. It has a single method, `main`, with one parameter: `client` of type `z2ui5_if_client`. (The real interface also declares a few attributes that the framework manages for you — you can ignore them.)
```abap
INTERFACE z2ui5_if_app PUBLIC.
  METHODS main
    IMPORTING
      client TYPE REF TO z2ui5_if_client.
ENDINTERFACE.
```

→ *For a deeper look at the lifecycle and framework internals, see [How It All Works](/technical/how_it_all_works) and [Concept](/technical/concept).*

### View Display
Instead of a message box, let's render a view with some text:
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

### Events
Now add a button and react to its press event:
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
        )->button(
          text  = `post`
          press = client->_event( `POST` ) ).
      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POST` ).

      client->message_box_display( `Hello World!` ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

As introduced above, the framework calls `main` on every roundtrip. The diagram shows both phases — the initial load and a later user event:

```text
┌──────────┐       ┌──────────┐       ┌──────────┐
│ Browser  │──────>│  main()  │──────>│ Browser  │
│ (Start)  │  HTTP │  init    │  HTTP │ (View)   │
└──────────┘       └──────────┘       └────┬─────┘
                                           │ user clicks
┌──────────┐       ┌──────────┐       ┌────┴─────┐
│ Browser  │<──────│  main()  │<──────│ Browser  │
│ (Update) │  HTTP │  event   │  HTTP │ (Event)  │
└──────────┘       └──────────┘       └──────────┘
```

Use the lifecycle checks to tell these phases apart:

- `client->check_on_init( )` — first call when the app starts
- `client->check_on_event( )` — user triggered an event (e.g. a button press)

Each `check_*` method returns `abap_true` only for its own phase, so the `IF`/`ELSEIF` chain acts as a dispatcher.

### Data Flow
Finally, add a public attribute and bind it to an input field to send data back to the server. The attribute must be in the `PUBLIC SECTION` — the framework accesses it dynamically and silently ignores private or protected attributes (full rules on the [Binding](/cookbook/model/binding) page):
```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DATA(view) = z2ui5_cl_xml_view=>factory(
        )->page( `abap2UI5 - Hello World`
        )->text( `My Text`
        )->button(
          text  = `post`
          press = client->_event( `POST` )
        )->input( client->_bind_edit( name ) ).
      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POST` ).

      client->message_box_display( |Your name is { name }.| ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
That's all you need. Set a breakpoint to watch the communication and data updates in action, then try changing the view, events, and data flow.

### Jump into the Code
Press `Ctrl+F12` in any running app to open the source code, view, and model side by side:
![Source code viewer opened with Ctrl+F12 showing code, view, and model](/get_started/image-2.png)
