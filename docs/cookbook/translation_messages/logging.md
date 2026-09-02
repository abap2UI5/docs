---
outline: [2, 4]
---
# Logging

Logging is critical for business processes. In ABAP systems, the standard tool is the Business Application Log (BAL), available on-premise and in ABAP Cloud environments alike. With abap2UI5, use BAL like you would in classic ABAP — and to show a log, hand it straight to `client->message_box_display( )`.

That method takes **any** message source, not just a string: a BAL table, a
`cl_bali_log`, a `bapiret2` table, `sy`, an exception. Anything that is not a
plain string goes through the framework's message mapper, which flattens it
into the lines the box shows — so every example below is the same one call with
a different argument. See [Message](./message) for the other sources.

## BAL Tables
In classic ABAP, use the BAL function modules and hand the BAL table over as it is. In the `bal_t_msgr` structure, `msgno` is the message number within the message class (`msgid`), while `msgnumber` is the message's sequence number within the log:
```abap
METHOD z2ui5_if_app~main.

  DATA(lt_bal) = VALUE bal_t_msgr(
    ( msgid = `Z001` msgno = `001` msgty = `S` time_stmp = `21354` msgnumber = `01` )
    ( msgid = `Z001` msgno = `001` msgty = `S` time_stmp = `21354` msgnumber = `02` ) ).

  client->message_box_display( lt_bal ).

ENDMETHOD.
```

## ABAP Cloud
In ABAP Cloud, hand the logging object over the same way:
```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = cl_bali_log=>create( ).
  DATA(lo_msg) = cl_bali_message_setter=>create(
    severity   = if_bali_constants=>c_severity_status
    id         = `DEMO_LOG`
    number     = `002`
    variable_1 = `username` ).
  lo_log->add_item( lo_msg ).

  DATA(lo_bapi) = cl_bali_message_setter=>create_from_bapiret2(
    VALUE #(
      type       = `E`
      id         = `DEMO_LOG`
      number     = `002`
      message_v1 = `Dummy` ) ).
  lo_log->add_item( lo_bapi ).

  client->message_box_display( lo_log ).

ENDMETHOD.
```

## abap-logger
You can also use the open-source project [**abap-logger**](https://github.com/ABAP-Logger/ABAP-Logger). It simplifies work with BAL logs and pairs well with abap2UI5:
```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = zcl_logger_factory=>create_log( desc = `ABAP Logger` ).
  lo_log->e( `This is an error...` ).
  lo_log->s( `This is a success message...` ).

  client->message_box_display( lo_log ).

ENDMETHOD.
```

## BAL Logs
Unlike message classes, BAL logs carry more detail — like timestamps. The mapper reads them like any other message source, so the examples above work for BAL logs too:

```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = zcl_logger_factory=>create_log( desc = `ABAP Logger` ).
  lo_log->e( `This is an error...` ).

  client->message_box_display( lo_log ).

ENDMETHOD.
```

::: tip Something richer than a message box
A message box is a list of lines. For a sortable table of the log with its
severities, timestamps and long texts, either build the view yourself from the
same data — it is an internal table like any other, see
[Tables](/cookbook/model/tables) — or take the ready-made message dialog from
the [popups add-on](https://github.com/abap2UI5-addons/popups).
:::
