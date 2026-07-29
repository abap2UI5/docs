---
outline: [2, 4]
---
# Keyboard Shortcuts

The `keyboard_shortcut` frontend event binds a key combination to a **named backend event** — the declarative equivalent of a `sap.ui.core.CommandExecution` shortcut, which needs a controller method and therefore has no place in a controller-less abap2UI5 app. The binding is pure data: the key combination and the name of the backend event it fires.

## Registering a Shortcut

`t_arg` is positional — the key combination and the backend event name:

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).

    client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                              t_arg = VALUE #( ( `Ctrl+S` )
                                               ( `SAVE` ) ) ).

    client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                              t_arg = VALUE #( ( `Ctrl+D` )
                                               ( `DELETE` ) ) ).
    view_display( ).
  ENDIF.

  CASE client->get( )-event.
    WHEN `SAVE`.
      client->message_toast_display( `Saved via Ctrl+S` ).
    WHEN `DELETE`.
      client->message_toast_display( `Deleted via Ctrl+D` ).
  ENDCASE.

ENDMETHOD.
```

Pressing the combination fires the backend event exactly like a button press would — and the browser default for the combination is suppressed.

## Behavior

- **Spelling** follows the UI5 convention: `Ctrl+S`, `Ctrl+Shift+D`, `F2`. Common aliases are normalized (`Cmd`/`Command` → `Meta`, `Control` → `Ctrl`, `Esc` → `Escape`, `Return` → `Enter`, …), so registration and keypress match for any spelling.
- **One registration per combination.** Registering the same combination again rebinds it — no need to unregister first.
- **Unregister** a combination by sending it with an empty event name:

```abap
client->follow_up_action( val   = client->cs_event-keyboard_shortcut
                          t_arg = VALUE #( ( `Ctrl+S` )
                                           ( `` ) ) ).
```

- **Lifetime:** the registry lives in the frontend and survives every roundtrip until the app is left. Navigating to another app starts from an empty set.

The same registration also works without a backend roundtrip via `client->_event_client( )` with the identical `t_arg`.

::: tip
See demo app 471 in the [samples repository](https://github.com/abap2UI5/samples) for a complete example.
:::

For controlling the *soft keyboard* on mobile devices, see [Soft Keyboard](/cookbook/browser_interaction/soft_keyboard).
