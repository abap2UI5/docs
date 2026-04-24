---
outline: [2, 4]
---
# Logging, BAL

Logging is essential for business processes. In ABAP systems, the standard tool is the Business Application Log (BAL), available both on-premise and in ABAP Cloud environments. With abap2UI5, use BAL just as in classic ABAP and display logs through the framework's built-in popups.

#### BAL Variables
In classic ABAP, use the BAL function modules and display the BAL table with the popup `Z2UI5_CL_POP_MESSAGES`:
```abap
METHOD z2ui5_if_app~main.

  DATA(lt_bal) = VALUE bal_t_msgr(
    ( msgid = `Z001` msgno = `001` msgty = `S` time_stmp = `21354` msgnumber = `01` )
    ( msgid = `Z001` msgno = `001` msgty = `S` time_stmp = `21354` msgnumber = `02` ) ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lt_bal ) ).

ENDMETHOD.
```

#### ABAP Cloud
In ABAP Cloud, pass the logging object directly into the popup:
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

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lo_log ) ).

ENDMETHOD.
```

#### abap-logger
You can also use the open-source project [**abap-logger**](https://github.com/ABAP-Logger/ABAP-Logger). It simplifies work with BAL logs and integrates smoothly with abap2UI5:
```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = zcl_logger_factory=>create_log( desc = `ABAP Logger` ).
  lo_log->e( `This is an error...` ).
  lo_log->s( `This is a success message...` ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lo_log ) ).

ENDMETHOD.
```

#### BAL Popup
Compared to message classes, BAL logs carry more detail — such as timestamps. Use the dedicated BAL log popup to display it. All the examples above work with the `Z2UI5_CL_POP_BAL` popup for richer output. Example for the abap-logger:

```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = zcl_logger_factory=>create_log( desc = `ABAP Logger` ).
  lo_log->e( `This is an error...` ).

  client->nav_app_call( z2ui5_cl_pop_bal=>factory( lo_log ) ).

ENDMETHOD.
```

::: tip
The BAL popup is still in its early stages and offers only basic functionality. If you've built BAL features with abap2UI5, please consider contributing to extend it.
:::
