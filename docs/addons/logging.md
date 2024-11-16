---
outline: [2, 6]
---
# Logging, BAL

<i class="fa-brands fa-github"></i> [Logging Addon on GitHub](https://github.com/abap2UI5-addons/logging)

Logging is essential for developing end-user business processes. In ABAP systems, the standard tool for this is the Business Application Log, which is supported in both on-premise systems and ABAP Cloud (via new APIs).

To display these messages for end-users in abap2UI5 apps, this addon centralizes BAL logging UIs for easy integration and collaborative expansion.

### Use cases 

#### BAL Cloud
In ABAP Cloud environments, the BAL includes a new API. Use this API to import logging objects directly into the class `z2ui5add_cl_bal_cl` to display messages:
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
        VALUE #( type       = 'E'
                 id         = 'DEMO_LOG'
                 number     = '002'
                 message_v1 = 'Dummy' ) ).

    lo_log->add_item( lo_bapi ).
    client->nav_app_call( z2ui5add_cl_bal_cl=>factory_popup( lo_log ) ).

ENDMETHOD.
```

#### BAL Classic
In on-premise ABAP systems, you can use the classic BAL APIs. As no specific logging object exists, simply import the BAL table into `z2ui5add_cl_bal_cl`:
```abap
METHOD z2ui5_if_app~main.

    DATA(lo_log) = cl_bali_log=>create( ).

    DATA(lo_msg) = cl_bali_message_setter=>create(
        severity   = if_bali_constants=>c_severity_status
        id         = 'DEMO_LOG'
        number     = '002'
        variable_1 = CONV #( cl_abap_context_info=>get_user_business_partner_id( ) ) ).
    lo_log->add_item( lo_msg ).

    " BAPIRET2
    DATA(lo_bapi) = cl_bali_message_setter=>create_from_bapiret2( VALUE #( type       = 'E'
                                                                           id         = 'DEMO_LOG'
                                                                           number     = '002'
                                                                           message_v1 = 'Dummy' ) ).
    lo_log->add_item( lo_bapi ).
    client->nav_app_call( z2ui5add_cl_bal_cl=>factory_popup( lo_log ) ).

ENDMETHOD.
```

#### ABAP-Logger
In on-premise systems, you also have the option to install the open-source project [**abap-logger**](https://github.com/ABAP-Logger/ABAP-Logger), which simplifies logging with BAL. Here’s an example of how to use it with abap2UI5:
```abap
CLASS z2ui5add_cl_abap_logger_sample DEFINITION PUBLIC FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5add_cl_abap_logger_sample IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    DATA: log TYPE REF TO zif_logger.
    log = zcl_logger_factory=>create_log( object = 'ZINTERFACES'
                                          subobject = 'ACCOUNTING'
                                          desc = 'Stuff imported from legacy systems' ).

    log->e( 'You see, what had happened was...' ).
    client->nav_app_call( z2ui5add_cl_abap_logger_ui=>display_popup( log ) ).

  ENDMETHOD.

ENDCLASS.
```

### BAL Cockpit
The concept of this app is to create a popup that shows all logs for a specific SLG1 logging object, designed to be non-SPA GUI and cloud-ready:
```abap
METHOD z2ui5_if_app~main.

    DATA(lo_log) = cl_bali_log=>create( ).

    DATA(lo_msg) = cl_bali_message_setter=>create(
        severity   = if_bali_constants=>c_severity_status
        id         = 'DEMO_LOG'
        number     = '002'
        variable_1 = CONV #( cl_abap_context_info=>get_user_business_partner_id( ) ) ).
    lo_log->add_item( lo_msg ).

    " BAPIRET2
    DATA(lo_bapi) = cl_bali_message_setter=>create_from_bapiret2( VALUE #( type       = 'E'
                                                                           id         = 'DEMO_LOG'
                                                                           number     = '002'
                                                                           message_v1 = 'Dummy' ) ).
    lo_log->add_item( lo_bapi ).
    client->nav_app_call( z2ui5add_cl_bal_cl=>factory_popup( lo_log ) ).

ENDMETHOD.
```

::: tip
This add-on is in its early stages, with basic functionality currently available. If you’ve implemented BAL functionality with abap2UI5, consider sharing your work! Contributions and pull requests are welcome. 
:::

