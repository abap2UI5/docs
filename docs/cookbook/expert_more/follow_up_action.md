---
outline: [2, 4]
---
# Follow-up Action

Sometimes, once your backend event handler has finished, you want to trigger an
action that runs on the frontend — set the browser title, move focus, scroll,
copy to the clipboard, and so on. `client->follow_up_action( )` schedules such a
frontend action; it runs in the browser right after the response arrives.

## Frontend event + arguments

The usual way: pass a built-in frontend event as the first parameter `val` and
its arguments in `t_arg`. The framework assembles the frontend call for you.

```abap
METHOD z2ui5_if_app~main.

    client->follow_up_action(
        val   = client->cs_event-set_title
        t_arg = VALUE #( ( `Invoice 4711` ) ) ).

ENDMETHOD.
```

`val` is one of the frontend events from `z2ui5_if_client=>cs_event` (see
[Frontend](/cookbook/event_navigation/frontend)) — for example `set_title`,
`set_focus`, `scroll_to`, `start_timer`, `download_b64_file`, `play_audio`.
`t_arg` carries the arguments the event expects; events without arguments need
no `t_arg`:

```abap
client->follow_up_action( client->cs_event-history_back ).
```

See the dedicated cookbook pages under
[Browser Interaction](/cookbook/browser_interaction/title) and
[Device Capabilities](/cookbook/device_capabilities/upload_download) for the
argument list of each event.

## Raw JavaScript

The second way: pass a raw JavaScript expression as `val` (without `t_arg`). It
runs as-is in the browser. This is only needed for hand-written custom scripts
and is **discouraged** — see [Custom JS](/cookbook/expert_more/custom_js) for
the security implications.

```abap
client->follow_up_action( `myFunction()` ).
```

`follow_up_action( )` decides which way applies from the content of `val`: a
plain event name (only `A-Z`, `a-z`, `0-9`, `_`) becomes a frontend event call,
anything containing JavaScript syntax runs verbatim.

## Calling a control method

The whitelisted control calls — `cs_event-control_by_id`, `control_global` and
`binding_call` — are frontend events too, so `follow_up_action( )` can invoke a
method on a control once the backend response arrives. Their `t_arg` is
positional (see [Frontend → Calling control methods](/cookbook/event_navigation/frontend#calling-control-methods-on-the-frontend)):

```abap
" after backend processing, advance a wizard step
client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `wiz` ) ( `setNextStep` ) ( `STEP2` ) ) ).
```

For `control_by_id`, the control is resolved by id. A separate `view` parameter
(default `cs_view-main`, which resolves the id across all open views) scopes the
lookup to a single view — pass `cs_view-popup` / `cs_view-popover` / … for a
control hosted in a popup or popover:

```abap
client->follow_up_action(
    val   = client->cs_event-control_by_id
    view  = client->cs_view-popup
    t_arg = VALUE #( ( `NavCon` ) ( `to` ) ( `detail` ) ) ).
```

See sample `Z2UI5_CL_DEMO_APP_180` for a complete example.
