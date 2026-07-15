---
outline: [2, 4]
---
# Timer

Fire a backend event after a delay with the `start_timer` frontend event. Handy for dashboards, status monitors, or any case that needs a delayed or periodic update without user interaction.

Pass the event name to fire and the delay in milliseconds:

```abap
client->follow_up_action(
    val   = client->cs_event-start_timer
    t_arg = VALUE #( ( `REFRESH` ) ( `2000` ) ) ).
```

After 2 seconds the browser triggers a backend roundtrip with the event name `REFRESH`, which you handle via `check_on_event` like any other event.

#### Periodic Refresh

To get a repeating timer, simply re-arm it at the end of each handler:

```abap
CLASS z2ui5_cl_sample_timer DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA counter TYPE i.

ENDCLASS.

CLASS z2ui5_cl_sample_timer IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        client->view_display( z2ui5_cl_xml_view=>factory(
            )->page( `abap2UI5 - Timer`
                )->text( client->_bind( counter )
            )->stringify( ) ).
        client->follow_up_action( val   = client->cs_event-start_timer
                        t_arg = VALUE #( ( `TICK` ) ( `2000` ) ) ).

      WHEN client->check_on_event( `TICK` ).
        counter = counter + 1.
        client->view_model_update( ).
        client->follow_up_action( val   = client->cs_event-start_timer
                        t_arg = VALUE #( ( `TICK` ) ( `2000` ) ) ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

The counter increments every 2 seconds. To stop the loop, simply don't re-arm the timer in the handler.

#### One-Shot Timer

A single `start_timer` call fires once — perfect for a deferred action like opening a new tab after a short delay:

```abap
WHEN client->check_on_event( `BUTTON_OPEN_NEW_TAB` ).
  client->follow_up_action( val   = client->cs_event-start_timer
                  t_arg = VALUE #( ( `FIRE_OPEN_TAB` ) ( `500` ) ) ).

WHEN client->check_on_event( `FIRE_OPEN_TAB` ).
  client->follow_up_action( val   = client->cs_event-open_new_tab
                  t_arg = VALUE #( ( `https://www.google.com/search?q=abap2ui5` ) ) ).
```

#### Replacing a Pending Timer

There is one timer at a time. Calling `start_timer` again before the previous one fires replaces it — useful for a debounce, e.g. auto-saving an input field 500 ms after the last keystroke.

::: warning
Each timer tick causes a full backend roundtrip. Use sensible intervals (e.g. 2000 ms or more) to avoid heavy server load.
:::
