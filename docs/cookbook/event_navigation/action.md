---
outline: [2, 4]
---
# Action

Sometimes you need to call a backend function and then act on the frontend right afterward. The follow-up action event covers this:
```abap
METHOD z2ui5_if_app~main.

    client->follow_up_action( client->_event_client(
        val   = client->cs_event-open_new_tab
        t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ) ).

ENDMETHOD.
```
See sample `Z2UI5_CL_DEMO_APP_180` for a complete example.
