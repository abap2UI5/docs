---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_064
  - z2ui5_cl_smp_app_028
description: Raise a backend event on a timer - timer_set( ) declares a repeating timer the framework keeps running while the app has the screen; the start_timer frontend action fires once or repeats, with an optional flag that keeps the busy indicator down.
---
# Timer

Raise a backend event after a delay, or every few seconds, without user interaction. Handy for dashboards, status monitors, or any case that needs a delayed or periodic update. There are two ways:

- `client->timer_set( )` **declares** a repeating timer for the app. The framework keeps it running for as long as the app has the screen.
- The `start_timer` frontend action fires **once** after a delay, or repeats when asked to.

Each tick is an event like any other, handled with `check_on_event( )`.

## Periodic Refresh

Declare the timer once, with the event of every tick and the delay in milliseconds:

```abap
CLASS zcl_sample_timer DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA counter TYPE i.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_timer IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      view_display( ).
      client->timer_set( event = `TICK` delay_ms = 2000 ).
    ELSEIF client->check_on_navigated( ).
      view_display( ).
    ELSEIF client->check_on_event( `TICK` ).
      counter = counter + 1.
    ENDIF.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `abap2UI5 - Timer`

                )->tag( `Text`
                    )->a( n = `text` v = client->_bind( counter ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

The counter increments every 2 seconds. The declaration is stored with the app, in its draft, and the framework arms it whenever the app gets the screen: on its first display, when an app it called - a popup app included - leaves again, and on a browser Back/Forward restore or an app-state link. It stops when the app is left with `nav_app_call( )` or `nav_app_leave( )` and runs again when the app comes back, so the `check_on_navigated( )` branch has nothing to re-arm.

- **Stop it** with `client->timer_stop( )`. An empty event or a delay of 0 does the same.
- **Change it** by calling `timer_set( )` with other values; the new timer runs from this response on.
- **State it on every roundtrip** if that is simpler, for example from the method that renders the view: an unchanged timer that runs is not sent again, and a tick that leaves the app unchanged writes no new draft.

A user event between two ticks pauses the timer: the next tick comes the full delay after that event's response. A click that meets a tick in flight is not lost; it is sent once the tick's response is applied.

## One-Shot Timer

The `start_timer` frontend action fires once - perfect for a deferred action like opening a new tab after a short delay. Pass the event name and the delay in milliseconds:

```abap
ELSEIF client->check_on_event( `BUTTON_OPEN_NEW_TAB` ).
  client->follow_up_action( val   = client->cs_event-start_timer
                  t_arg = VALUE #( ( `FIRE_OPEN_TAB` ) ( `500` ) ) ).

ELSEIF client->check_on_event( `FIRE_OPEN_TAB` ).
  client->follow_up_action( val   = client->cs_event-open_new_tab
                  t_arg = VALUE #( ( `/sap/bc/z2ui5?app_start=z2ui5_cl_my_report` ) ) ).
```

There is one timer per app. Calling `start_timer` again before the previous one fires replaces it - useful for a debounce, e.g. auto-saving an input field 500 ms after the last keystroke. A `start_timer` also takes the timer over from a `timer_set( )` declaration and drops the declaration; call `timer_set( )` again to hand it back.

A tick can come due while another roundtrip is still running, for example a popup's own event or a Back/Forward restore. Such a tick is not dropped and does not abort that request. It waits for the response and fires right after it.

## Repeating Without a Declaration

A fourth argument `` `X` `` - the arguments are event, delay, the no-busy flag and the repeat flag - makes `start_timer` repeat: the browser arms the same tick again after every response, and a user event in between pauses it. It keeps running under a popup of the same app and ends with `cs_event-stop_timer`, a new `start_timer`, a navigation to another app or a Back/Forward restore:

```abap
client->follow_up_action( val   = client->cs_event-start_timer
                          t_arg = VALUE #( ( `TICK` ) ( `5000` ) ( `` ) ( `X` ) ) ).
```

`cs_event-stop_timer` cancels the pending tick and ends a repeating timer. Wired to a button, it stops the timer in the browser without a roundtrip. Prefer `timer_set( )` for a poll: it also comes back after a navigation.

## Without the Busy Indicator

Each tick is a backend roundtrip like any other, so when the backend takes longer than a second, the full-screen busy indicator comes up. For a tick nobody is waiting on - a background poll, or a carousel or ticker that moves on by itself - the overlay then flashes over a screen the user is only reading. Keep it down with `no_busy`:

```abap
client->timer_set( event    = `TICK`
                   delay_ms = 5000
                   no_busy  = abap_true ).
```

With `start_timer` it is the third argument, `` `X` ``:

```abap
client->follow_up_action( val   = client->cs_event-start_timer
                          t_arg = VALUE #( ( `TICK` ) ( `5000` ) ( `X` ) ) ).
```

Write it as the string `` `X` ``, not `abap_true`: `t_arg` is a string table, and abaplint's syntax check rejects a `c` value such as `abap_true` as one of its rows.

Only the overlay goes - the flag does for the tick what [`check_no_busy`](/resources/api#event) does for an `_event( )`. Leave it off for a tick the user is actually waiting for; there the overlay is what tells them the app is working.

::: warning
Each timer tick causes a full backend roundtrip. Use sensible intervals (e.g. 2000 ms or more) to avoid heavy server load.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Progress Indicator during a Backend Call (A) | [`Z2UI5_CL_SMP_APP_064`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_064.clas.abap) |
| Refresh the View Every n Seconds (A) | [`Z2UI5_CL_SMP_APP_028`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_028.clas.abap) |

<!-- samples:end -->
