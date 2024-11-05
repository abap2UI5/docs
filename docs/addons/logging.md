# Logging

<i class="fa-brands fa-github"></i> [GitHub](https://github.com/abap2UI5-addons/logging)

ABAP development is nearly impossible without eventually encountering BAL logging. Whether you like using the standard API or prefer working with open-source projects like abap-logger, in the end, we need a way to display these messages in an abap2UI5 app or a popup. This add-on collects all functionality related to BAL logging. Instead of building various new BAL popups, we can gather the functionality here and develop it collaboratively. Currently, only basic functionality is available, but PRs are welcome!

### 1. UI for Business Application Log (classic)

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

### 2. UI for Business Application Log (cloud)

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


### 4. UI for abap-logger

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

