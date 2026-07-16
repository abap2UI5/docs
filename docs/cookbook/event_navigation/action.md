---
outline: [2, 4]
---
# Action (Obsolete)

::: warning Do Not Use Anymore
`client->action->gen( )` is obsolete. Use
[`client->follow_up_action( )`](/cookbook/expert_more/follow_up_action) instead —
it takes the same frontend event and arguments and now covers this case
directly.
:::

## What Changed

`action->gen( )` used to schedule a frontend event by name with typed arguments:

```abap
" obsolete — do not use
client->action->gen(
    val   = client->cs_event-open_new_tab
    t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ).
```

`follow_up_action( )` now accepts the same `val` and `t_arg`, so the migration is
a plain rename:

```abap
client->follow_up_action(
    val   = client->cs_event-open_new_tab
    t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ).
```

`val` is one of the frontend events from `z2ui5_if_client=>cs_event` (see
[Frontend](./frontend.md)); `t_arg` carries the arguments the event expects. See
[Follow-up Action](/cookbook/expert_more/follow_up_action) for the full details
and sample `Z2UI5_CL_DEMO_APP_180` for a complete example.
