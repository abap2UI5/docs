---
outline: [2, 6]
---
# Logging, BAL

Logging is essential for developing end-user business processes. In ABAP systems, the standard tool for this purpose is the Business Application Log (BAL), supported in both on-premise systems and ABAP Cloud (via new APIs). With abap2UI5, you can use BAL functions just as you would in classic development and create a table with the entries or use predefined popups provided by the framework.

##### BAL Variables
In ABAP classic, you can use the classic BAL function modules and display the BAL table with the popup `z2ui5_cl_pop_messages`:
```abap
METHOD z2ui5_if_app~main.

  DATA(lt_bal) = VALUE bal_t_msgr(
    ( msgid = 'Z001' msgno = '001' msgty = 'S' time_stmp = '21354' msgnumber = '01' )
    ( msgid = 'Z001' msgno = '001' msgty = 'S' time_stmp = '21354' msgnumber = '02' ) ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lt_bal ) ).

```

##### ABAP Cloud
In ABAP cloud, you can just import the logging object into the message popup:
```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = cl_bali_log=>create( ).
  DATA(lo_msg) = cl_bali_message_setter=>create(
    severity   = if_bali_constants=>c_severity_status
    id         = 'DEMO_LOG'
    number     = '002'
    variable_1 = `username` ).
  lo_log->add_item( lo_msg ).

  DATA(lo_bapi) = cl_bali_message_setter=>create_from_bapiret2( 
    VALUE #( 
      type       = 'E'
      id         = 'DEMO_LOG'
      number     = '002'
      message_v1 = 'Dummy' ) ).
  lo_log->add_item( lo_bapi ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lo_log ) ).

ENDMETHOD.
```

##### abap-logger
In on-premise systems, you also have the option to use the fantastic open-source project [**abap-logger**](https://github.com/ABAP-Logger/ABAP-Logger). This tool simplifies working with BAL logs and integrates seamlessly with abap2UI5. Here’s an example:
```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = zcl_logger_factory=>create_log( 
            object    = 'ZINTERFACES'
            subobject = 'ACCOUNTING'
            desc      = 'my abap-logger logger' ).
  log->e( 'There is an error...' ).

  client->nav_app_call( z2ui5_cl_pop_messages=>factory( lo_log ) ).

ENDMETHOD.
```

##### BAL Popup
Compared to T100 messages, BAL logs include more detailed information, such as timestamps. You can also use the BAL log popup to display this information. All the examples above can be used as is, but for the popup app, you should use the `z2ui5_cl_pop_bal` popup instead:

```abap
METHOD z2ui5_if_app~main.

  DATA(lo_log) = zcl_logger_factory=>create_log( 
            object    = 'ZINTERFACES'
            subobject = 'ACCOUNTING'
            desc      = 'my abap-logger logger' ).
  log->e( 'There is an error...' ).

  client->nav_app_call( z2ui5_cl_pop_bal=>factory( lo_log ) ).

ENDMETHOD.
```

::: tip
This popup is still in its early stages, offering basic functionality. If you’ve implemented BAL features with abap2UI5, consider sharing your work! Contributions and pull requests are always welcome.
:::

