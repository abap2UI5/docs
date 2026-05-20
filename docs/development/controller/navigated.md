---
outline: [2, 4]
---
# Navigated

`client->check_on_navigated` returns `abap_true` when the user returns from another app. Use `client->get_app_prev` to access the previous app instance and read any data it returns.

```abap
  METHOD z2ui5_if_app~main.

    me->client = client.
    CASE abap_true.
      WHEN client->check_on_navigated( ).
        on_navigated( ).
    ENDCASE.

  ENDMETHOD.

  METHOD on_navigated.

    DATA(lo_app_prev) = client->get_app_prev( ).

  ENDMETHOD.
```
