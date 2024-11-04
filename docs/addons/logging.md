# Logging

## Use Cases

### 1. UI for abap-logger

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

### 2. POPUP for BAL Messages

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

<br>
<img width="800" alt="SAPGUI 2024-09-05 08 00 51" src="https://github.com/user-attachments/assets/02c6a23a-e4cc-4439-afa0-7842897c8468">
