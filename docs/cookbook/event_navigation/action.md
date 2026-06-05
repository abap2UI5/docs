---
outline: [2, 4]
---
# Action

Sometimes you need to call a backend function and then act on the frontend right afterward. The `action->gen` method covers this:
```abap
METHOD z2ui5_if_app~main.

    client->action->gen(
        val   = client->cs_event-open_new_tab
        t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ).

ENDMETHOD.
```
`val` is one of the frontend events from `z2ui5_if_client=>cs_event` (see [Frontend](./frontend.md)); `t_arg` carries the arguments the event expects.

See sample `Z2UI5_CL_DEMO_APP_180` for a complete example.
