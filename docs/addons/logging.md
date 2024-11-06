---
outline: [2, 6]
---
# Logging, BAL

<i class="fa-brands fa-github"></i> [Logging Addon on GitHub](https://github.com/abap2UI5-addons/logging)

Developing end-user business processes is not possible without adequate logging. The standard method for this in ABAP systems is the integrated Business Application Log (BAL), which works in on-premise systems and, with the new APIs, in ABAP Cloud. You might also consider open-source projects like the excellent [abap-logger](https://github.com/ABAP-Logger/ABAP-Logger).<br>

Ultimately, we need a way to display these messages for end users in abap2UI5 apps. This add-on centralizes BAL logging UIs for abap2UI5 apps.<br>

Instead of creating multiple BAL popups individually, the goal of this add-on is to consolidate BAL functionality here for collaborative expansion. Currently, only basic functionality is available. Have you already implemented BAL functionality with abap2UI5? Consider sharing it — PRs are welcome!

### 1. BAL Messages (classic)

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

### 2. BAL Messages (cloud)

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

### 3. BAL Cockpit
In ABAP for Cloud 

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


### 4. ABAP-Logger

UI for the Open Source Project [**abap-logger**](https://github.com/ABAP-Logger/ABAP-Logger)

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

