---
outline: [2, 4]
---
# Life Cycle

Every request to an abap2UI5 app enters the `main` method. The recommended pattern uses `CASE abap_true` together with `client->check_on_init`, `client->check_on_event`, and `client->check_on_navigated` to dispatch to the matching handler method.

```abap
CLASS z2ui5_cl_demo_app_001 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_value  TYPE string.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS display_view.
    METHODS on_event.
    METHODS on_navigated.

  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    CASE abap_true.
      WHEN client->check_on_init( ).
        on_init( ).
        display_view( ).
      WHEN client->check_on_event( ).
        on_event( ).
      WHEN client->check_on_navigated( ).
        on_navigated( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
```

See the dedicated sections of this development guide for full details on views, events, data binding, and navigation.
