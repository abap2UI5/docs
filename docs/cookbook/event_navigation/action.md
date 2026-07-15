---
outline: [2, 4]
---
# Action (Obsolete)

::: warning Do Not Use Anymore
`client->action->gen( )` is obsolete. Use
[`client->follow_up_action( )`](/cookbook/expert_more/follow_up_action) instead.
It schedules the same frontend event, called from the backend.
:::

## What Changed

`action->gen( )` used to schedule a frontend event by name with typed arguments:

```abap
" obsolete — do not use
client->action->gen(
    val   = client->cs_event-open_new_tab
    t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ).
```

The replacement passes the same frontend event as a `follow_up_action` string.
`.eF('<EVENT>'<, args>)` calls the event; each argument is a quoted string:

```abap
client->follow_up_action( |.eF('OPEN_NEW_TAB', 'https://github.com/abap2UI5')| ).
```

`val` was one of the frontend events from `z2ui5_if_client=>cs_event` (see
[Frontend](./frontend.md)) — the event id is simply its upper-case name
(`cs_event-open_new_tab` → `OPEN_NEW_TAB`). `t_arg` carried the arguments the
event expects; they now follow the event name in the `.eF( )` call.

See sample `Z2UI5_CL_DEMO_APP_180` for a complete example.
