---
outline: [2, 4]
---
# Timer, Auto-Refresh

abap2UI5 provides a custom control `z2ui5.Timer` that fires events after a specified delay. This helps with dashboards, status monitors, or any case that needs periodic data updates without user interaction.

Add the timer as a view element through `_z2ui5( )->timer( ... )`. These parameters apply:

| Parameter     | Description                                      |
|---------------|--------------------------------------------------|
| `finished`    | Event fired when the timer elapses               |
| `delayms`     | Delay in milliseconds before firing              |
| `checkactive` | Bind to an `abap_bool` to activate/deactivate    |
| `checkrepeat` | If `abap_true`, the timer repeats automatically  |

#### Basic Usage

Embed the timer in your view and control it through data binding. The example below increments a counter every 2 seconds:

```abap
CLASS z2ui5_cl_sample_timer DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA counter TYPE i.
    DATA check_timer_active TYPE abap_bool.

ENDCLASS.

CLASS z2ui5_cl_sample_timer IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        check_timer_active = abap_true.

        client->view_display( z2ui5_cl_xml_view=>factory(
            )->page( `abap2UI5 - Timer`
                )->text( client->_bind( counter )
                )->_z2ui5( )->timer(
                    checkactive = client->_bind( check_timer_active )
                    delayms     = `2000`
                    finished    = client->_event( `TIMER_FINISHED` )
                    checkrepeat = abap_true
            )->stringify( ) ).

      WHEN client->check_on_event( `TIMER_FINISHED` ).
        counter = counter + 1.
        client->view_model_update( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```
With `checkrepeat = abap_true`, the timer restarts after each event automatically — no manual re-trigger needed.

#### One-Shot Timer

For a single firing (e.g., to open a new browser tab), omit `checkrepeat` and control activation through `checkactive`:

```abap
CLASS z2ui5_cl_sample_timer_once DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_url TYPE string.
    DATA mv_check_timer_active TYPE abap_bool.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS display_view.

ENDCLASS.

CLASS z2ui5_cl_sample_timer_once IMPLEMENTATION.

  METHOD display_view.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    client->view_display( view->shell(
          )->page( title          = `abap2UI5 - Timer One-Shot`
                   navbuttonpress = client->_event( `BACK` )
                   shownavbutton  = client->check_app_prev_stack( )
             )->_z2ui5( )->timer(
                  checkactive = client->_bind( mv_check_timer_active )
                  finished    = client->_event_client(
                      val   = client->cs_event-open_new_tab
                      t_arg = VALUE #( ( `$` && client->_bind( mv_url ) ) ) )
              )->simple_form( title    = `Form Title`
                              editable = abap_true
                  )->content( `form`
                      )->button(
                          text  = `open new tab`
                          press = client->_event( `BUTTON_OPEN_NEW_TAB` )
           )->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      mv_check_timer_active = abap_false.
      display_view( ).
    ENDIF.

    CASE client->get( )-event.

      WHEN `BUTTON_OPEN_NEW_TAB`.
        mv_check_timer_active = abap_true.
        mv_url = `https://www.google.com/search?q=abap2ui5`.
        client->view_model_update( ).

      WHEN `BACK`.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```
The timer starts inactive. When the user clicks the button, the handler sets `mv_check_timer_active` to `abap_true` and the timer fires once, opening a new browser tab through `_event_client`.

#### Stopping a Repeating Timer

To stop a repeating timer, set the bound `checkactive` flag to `abap_false`:

```abap
  WHEN client->check_on_event( `TOGGLE_TIMER` ).
    IF check_timer_active = abap_true.
      check_timer_active = abap_false.
    ELSE.
      check_timer_active = abap_true.
    ENDIF.
    client->view_model_update( ).
```

::: warning
Each timer event triggers a full backend roundtrip. Use sensible intervals (e.g., 2000ms or more) to avoid heavy server load.
:::
