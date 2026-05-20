---
outline: [2, 4]
---
# Event

`client->check_on_event` returns `abap_true` when the request was triggered by a user event. Read the event name with `client->get( )-event` and dispatch to the matching handler.

```abap
  METHOD z2ui5_if_app~main.

    me->client = client.
    CASE abap_true.
      WHEN client->check_on_event( ).
        on_event( ).
    ENDCASE.

  ENDMETHOD.

  METHOD on_event.

    DATA(lt_arg) = client->get_event_arg( ).

    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
        client->message_toast_display( |{ mv_value } - send to the server| ).
    ENDCASE.

  ENDMETHOD.
```
