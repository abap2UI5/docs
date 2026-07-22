---
outline: [2, 4]
---
# Frontend

If you don't want to handle the event in the backend, fire actions directly on the frontend. The difference between the two event styles:

- **`client->_event( )`** — causes a backend roundtrip; the event runs in the `main` method
- **`client->_event_client( )`** — runs an action directly in the browser; no backend call

Use `_event_client` on a UI5 control property (like `press`) when the response should happen entirely in the browser. To fire a frontend event **after** backend processing has finished, use [`client->follow_up_action`](/cookbook/expert_more/follow_up_action) instead — it schedules the same frontend event but is called from the backend.

The following frontend events are available:
```abap
  CONSTANTS:
    BEGIN OF cs_event,

      "Framework
      popup_close               TYPE string VALUE `POPUP_CLOSE`,
      popover_close             TYPE string VALUE `POPOVER_CLOSE`,
      set_size_limit            TYPE string VALUE `SET_SIZE_LIMIT`,
      set_odata_model           TYPE string VALUE `SET_ODATA_MODEL`,
      cross_app_nav_to_ext      TYPE string VALUE `CROSS_APP_NAV_TO_EXT`,
      cross_app_nav_to_prev_app TYPE string VALUE `CROSS_APP_NAV_TO_PREV_APP`,

      "Actions
      clipboard_copy            TYPE string VALUE `CLIPBOARD_COPY`,
      clipboard_app_state       TYPE string VALUE `CLIPBOARD_APP_STATE`,
      set_title                 TYPE string VALUE `SET_TITLE`,
      set_title_launchpad       TYPE string VALUE `SET_TITLE_LAUNCHPAD`,
      set_focus                 TYPE string VALUE `SET_FOCUS`,
      scroll_to                 TYPE string VALUE `SCROLL_TO`,
      scroll_into_view          TYPE string VALUE `SCROLL_INTO_VIEW`,
      start_timer               TYPE string VALUE `START_TIMER`,
      keyboard_set_mode         TYPE string VALUE `KEYBOARD_SET_MODE`,
      open_new_tab              TYPE string VALUE `OPEN_NEW_TAB`,
      location_reload           TYPE string VALUE `LOCATION_RELOAD`,
      system_logout             TYPE string VALUE `SYSTEM_LOGOUT`,
      download_b64_file         TYPE string VALUE `DOWNLOAD_B64_FILE`,
      urlhelper                 TYPE string VALUE `URLHELPER`,
      history_back              TYPE string VALUE `HISTORY_BACK`,
      store_data                TYPE string VALUE `STORE_DATA`,
      play_audio                TYPE string VALUE `PLAY_AUDIO`,

      "Control calls (whitelisted, positional t_arg)
      control_by_id             TYPE string VALUE `CONTROL_BY_ID`,
      control_global            TYPE string VALUE `CONTROL_GLOBAL`,
      binding_call              TYPE string VALUE `BINDING_CALL`,

    END OF cs_event.
```
For example, to open a new tab directly from a button press (no backend involved):
```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button(
            text  = `open new tab`
            press = client->_event_client(
                val   = client->cs_event-open_new_tab
                t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) )
        )->stringify( ) ).

ENDMETHOD.
```

## Calling control methods on the frontend

The last three constants — `control_by_id`, `control_global` and `binding_call` — are frontend events too, but instead of a fixed built-in action they invoke a *whitelisted* method on a control, a global object or a binding. Their arguments are **positional**: an empty argument between two filled ones keeps its slot as `` `` ``.

| Event            | `t_arg` (positional)                                                                 |
| ---------------- | ------------------------------------------------------------------------------------ |
| `control_by_id`  | `id`, `method`, `params…` — call a whitelisted method on a control resolved by id     |
| `control_global` | `object`, `method`, `params…` — `MESSAGE_TOAST`, `MESSAGE_BOX`, `BUSY_INDICATOR`, `THEMING` |
| `binding_call`   | `path`, `method`, `params…` — e.g. `filter` (operator, value1, value2) or `sort` (path, descending, group) |

```abap
" toggle a MessagePopover open, anchored to the pressing button, no roundtrip
press = client->_event_client(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `msgPopover` ) ( `toggleBy` ) ( `${$source>/id}` ) ) )
```

The same events also work from the backend with `client->follow_up_action( )` using the identical `t_arg`.

### The `view` parameter

For `control_by_id`, the control is looked up by id. Both `_event_client( )` and `follow_up_action( )` take a separate `view` parameter (default `cs_view-main`) that scopes this lookup:

- omit it (or pass `cs_view-main`) — the id is resolved across all open views;
- pass `cs_view-popup` / `cs_view-popover` / `cs_view-nested` / … — the lookup is scoped to a control hosted in that view (e.g. a control living inside a popup).

```abap
" call a method on a control that lives inside the popup view
press = client->_event_client(
    val   = client->cs_event-control_by_id
    view  = client->cs_view-popup
    t_arg = VALUE #( ( `NavCon` ) ( `to` ) ( `${$parameters>/selectedKey}` ) ) )
```

::: tip Migrated from a positional view slot
The view used to be the second entry of `t_arg` (`id`, `view`, `method`, …). It is now the dedicated `view` importing parameter, so main-view calls simply omit it. Older examples that still pass `` `MAIN` `` as the second `t_arg` element continue to work.
:::

`control_global` and `binding_call` are not resolved by id and ignore `view`.
