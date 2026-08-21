---
outline: [2, 4]
description: A button, a press event, and the abap2UI5 lifecycle behind them.
---
# Step 3: Events

The framework calls `main` on **every** roundtrip — on the initial start and
again after each user interaction. As soon as an app reacts to input, `main`
has to tell those calls apart. That job falls to the lifecycle checks, and
`main` becomes a dispatcher:

```abap
CLASS zcl_app_walkthrough DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_walkthrough IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `Walkthrough - Step 3`

                      )->tag( `Text`
                          )->a( n = `text` v = `Hello World`
                      )->tag( `Button`
                          )->a( n = `text`  v = `Say Hello`
                          )->a( n = `press` v = client->_event( `SAY_HELLO` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SAY_HELLO` ).

      client->message_toast_display( `Hello from abap2UI5!` ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## The Lifecycle

```text
┌──────────┐       ┌──────────┐       ┌──────────┐
│ Browser  │──────>│  main()  │──────>│ Browser  │
│ (Start)  │  HTTP │  view    │  HTTP │ (View)   │
└──────────┘       └──────────┘       └────┬─────┘
                                           │ user clicks
┌──────────┐       ┌──────────┐       ┌────┴─────┐
│ Browser  │<──────│  main()  │<──────│ Browser  │
│ (Toast)  │  HTTP │  event   │  HTTP │ (Event)  │
└──────────┘       └──────────┘       └──────────┘
```

- **`check_on_navigated( )`** is true when the app has to draw its screen —
  on the first start, and again whenever the user navigates back to it later.
  This branch displays the view.
- **`check_on_event( )`** is true when the user triggered an event. The
  argument names which one.
- ``client->_event( `SAY_HELLO` )`` wires the button: it returns the press
  handler that sends the event — with the name you chose — back to `main`.

Each `check_*` method is true only for its own phase, so the `IF`/`ELSEIF`
chain cleanly dispatches every roundtrip. The full picture is on the
[Life Cycle](/cookbook/event_navigation/life_cycle) page.

Next: data leaves the browser and reaches your class — without a single line
of transfer code.
