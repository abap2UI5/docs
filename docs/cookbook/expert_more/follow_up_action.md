---
outline: [2, 4]
---
# Follow-up Action (Obsolete)

::: warning Do Not Use Anymore
`client->follow_up_action( )` is obsolete. Use `client->action->gen( )` instead. It works the same way, but **does not allow sending arbitrary JavaScript to the frontend** — which is exactly why `follow_up_action` was deprecated. See [Custom JS](/cookbook/expert_more/custom_js) for the full reasoning behind removing direct JS execution.
:::

## What Changed

`follow_up_action` used to take a free-text string and run it as JavaScript in the browser:

```abap
" obsolete — do not use
client->follow_up_action( `myFunction()` ).
```

This pattern made it possible to inject arbitrary JavaScript from the backend, with all the security risks described on the [Custom JS](/cookbook/expert_more/custom_js) page (XSS, bypassed output encoding, CSP breakage, no sandboxing).

The replacement, `client->action->gen( )`, calls a **predefined frontend event** by name and passes typed arguments — no raw JavaScript travels from ABAP to the browser:

```abap
" recommended
client->action->gen( val   = client->cs_event-set_title
                t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

The available events are exposed via `client->cs_event-...` — for example `set_title`, `set_focus`, `scroll_to`, `clipboard_copy`, `open_new_tab`, `start_timer`, `download_b64_file`, `play_audio`. See the [Custom Controls (Obsolete)](/cookbook/expert_more/custom_controls) page for the full mapping.

## Migration

Replace each `follow_up_action` call with the equivalent `action` call:

```abap
" before
client->follow_up_action( `myFunction()` ).

" after
client->action->gen( val   = client->cs_event-<event_name>
                t_arg = VALUE #( ( `arg1` ) ( `arg2` ) ) ).
```

If you cannot find a built-in event that covers your case, prefer building a real [Custom Control](/advanced/extensibility/custom_control) over reaching for JavaScript injection. See [Custom JS](/cookbook/expert_more/custom_js) for why injecting JavaScript from the backend is strongly discouraged.
