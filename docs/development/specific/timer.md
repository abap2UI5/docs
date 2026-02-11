# Timer, Auto-Refresh

abap2UI5 provides a timer functionality that triggers backend events after a specified interval. This is useful for dashboards, status monitors, or any scenario that requires periodic data updates without manual user interaction.

#### Basic Usage

Use the `client->timer_set` method to schedule a backend event after a given interval in milliseconds:

```abap
CLASS z2ui5_cl_sample_timer DEFINITION PUBLIC FINAL CREATE PUBLIC.

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

        client->timer_set(
            interval_ms    = 2000
            event_finished = `TIMER_FINISHED` ).

      WHEN client->check_on_event( `TIMER_FINISHED` ).
        counter = counter + 1.
        client->view_model_update( ).

        "set timer again for continuous refresh
        client->timer_set(
            interval_ms    = 2000
            event_finished = `TIMER_FINISHED` ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```
The timer fires once after the interval expires. To create a continuous auto-refresh loop, simply re-set the timer inside the event handler.

#### Dashboard Auto-Refresh

A common use case is refreshing dashboard data periodically. The following example fetches updated data from the database on every timer event:

```abap
CLASS z2ui5_cl_sample_dashboard DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA last_refresh TYPE string.
    DATA mt_orders TYPE STANDARD TABLE OF z2ui5_t_draft WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_dashboard IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        last_refresh = |{ sy-datum DATE = USER } { sy-uzeit TIME = USER }|.

        DATA(tab) = z2ui5_cl_xml_view=>factory(
            )->page( `Dashboard`
                )->header_content(
                    )->text( client->_bind( last_refresh )
                )->get_parent(
                )->table( client->_bind( mt_orders ) ).

        tab->columns(
            )->column( )->text( `ID` )->get_parent(
            )->column( )->text( `Status` ).

        tab->items( )->column_list_item( )->cells(
            )->text( `{UUID}`
            )->text( `{UUID_PREV}` ).

        client->view_display( tab->stringify( ) ).

        client->timer_set(
            interval_ms    = 5000
            event_finished = `REFRESH` ).

      WHEN client->check_on_event( `REFRESH` ).
        "refresh data
        last_refresh = |{ sy-datum DATE = USER } { sy-uzeit TIME = USER }|.
        client->view_model_update( ).

        client->timer_set(
            interval_ms    = 5000
            event_finished = `REFRESH` ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

#### Stopping the Timer

To stop an active timer, simply do not call `client->timer_set` again in the event handler. You can also add a toggle button for user control:

```abap
  WHEN client->check_on_event( `TOGGLE_TIMER` ).
    IF auto_refresh = abap_true.
      auto_refresh = abap_false.
    ELSE.
      auto_refresh = abap_true.
      client->timer_set(
          interval_ms    = 2000
          event_finished = `TIMER_FINISHED` ).
    ENDIF.
    client->view_model_update( ).

  WHEN client->check_on_event( `TIMER_FINISHED` ).
    counter = counter + 1.
    IF auto_refresh = abap_true.
      client->timer_set(
          interval_ms    = 2000
          event_finished = `TIMER_FINISHED` ).
    ENDIF.
    client->view_model_update( ).
```

::: warning
Keep in mind that each timer event triggers a full backend roundtrip. Use reasonable intervals (e.g., 2000ms or higher) to avoid excessive server load.
:::
