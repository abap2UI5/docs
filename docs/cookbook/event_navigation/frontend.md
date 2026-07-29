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
      keyboard_shortcut         TYPE string VALUE `KEYBOARD_SHORTCUT`,
      open_new_tab              TYPE string VALUE `OPEN_NEW_TAB`,
      location_reload           TYPE string VALUE `LOCATION_RELOAD`,
      nav_to_route              TYPE string VALUE `NAV_TO_ROUTE`,
      system_logout             TYPE string VALUE `SYSTEM_LOGOUT`,
      download_b64_file         TYPE string VALUE `DOWNLOAD_B64_FILE`,
      urlhelper                 TYPE string VALUE `URLHELPER`,
      history_back              TYPE string VALUE `HISTORY_BACK`,
      store_data                TYPE string VALUE `STORE_DATA`,
      play_audio                TYPE string VALUE `PLAY_AUDIO`,
      wizard_set_next_step      TYPE string VALUE `WIZARD_SET_NEXT_STEP`,

      "Control calls (positional t_arg)
      control_by_id             TYPE string VALUE `CONTROL_BY_ID`,
      control_global            TYPE string VALUE `CONTROL_GLOBAL`,
      binding_call              TYPE string VALUE `BINDING_CALL`,
      bind_element              TYPE string VALUE `BIND_ELEMENT`,

      "Smart controls (sap.ui.comp)
      smart_variant_init        TYPE string VALUE `SMART_VARIANT_INIT`,
      filter_bar_variant_init   TYPE string VALUE `FILTER_BAR_VARIANT_INIT`,

    END OF cs_event.
```
Some of these events have their own pages: [`keyboard_shortcut`](/cookbook/browser_interaction/keyboard_shortcuts) binds key combinations to backend events, [`nav_to_route`](/cookbook/event_navigation/routing) navigates by hash route, and [`smart_variant_init` / `filter_bar_variant_init`](/cookbook/expert_more/smart_controls) wire variant management for smart controls.
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

The control-call constants — `control_by_id`, `control_global`, `binding_call` and `bind_element` — are frontend events too, but instead of a fixed built-in action they operate on a control, a global object, a binding or a whole view slot. Their arguments are **positional**: an empty argument between two filled ones keeps its slot as `` `` ``.

| Event            | `t_arg` (positional)                                                                 |
| ---------------- | ------------------------------------------------------------------------------------ |
| `control_by_id`  | `id`, `method`, `params…` — call a method on a control resolved by id                 |
| `control_global` | `object`, `method`, `params…` — `MESSAGE_TOAST`, `MESSAGE_BOX`, `BUSY_INDICATOR`, `THEMING` |
| `binding_call`   | `id`, `aggregation`, `method`, `params…` — e.g. `filter` (path, operator, value1, value2) or `sort` (path, descending, group) on the aggregation's binding |
| `bind_element`   | `index`, `_bind( table )` — element-bind a whole view slot to a table row, see below  |

For `control_by_id`, any public control method is callable as long as it is not on the framework's **denylist**: methods that would break abap2UI5's own invariants (destroying views, re-rendering, detaching the framework's handlers, …) are blocked, ordinary setters and toggles (`setVisible`, `toggleBy`, `enablePostButton`, …) simply work. A small set of methods is additionally special-cased for typed arguments. `control_global` and `binding_call` remain strict whitelists — only the listed global objects and the binding methods `filter` / `sort` are callable.

```abap
" toggle a MessagePopover open, anchored to the pressing button, no roundtrip
press = client->_event_client(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `msgPopover` ) ( `toggleBy` ) ( `${$source>/id}` ) ) )
```

The same events also work from the backend with `client->follow_up_action( )` using the identical `t_arg`.

### Element-binding a view slot: `bind_element`

`bind_element` binds a whole view slot (popup, popover, main, …) to one row of a bound table — the abap2UI5 equivalent of `oControl.bindElement( )`. All *relative* bindings in that slot (`{NAME}`, `{CATEGORY}`, nested aggregations) then resolve against the selected row, so a detail popup needs no data copied into event arguments:

```abap
" element-bind the popup slot to row <index> of t_product
client->follow_up_action(
    val   = client->cs_event-bind_element
    view  = client->cs_view-popup
    t_arg = VALUE #( ( index ) ( client->_bind( t_product ) ) ) ).
```

The `view` parameter selects the slot to bind; `t_arg` carries the row index and the table's binding path. See demo app 470 in the [samples repository](https://github.com/abap2UI5/samples) for a complete example.

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

::: warning Migrated from a positional view slot
The view used to be the second entry of `t_arg` (`id`, `view`, `method`, …). It is now the dedicated `view` importing parameter, and the framework injects it into the argument list itself. Older examples that still pass `` `MAIN` `` as the second `t_arg` element **no longer work** — the extra entry shifts every argument by one and the call fails on the frontend. Drop the positional view entry and use the `view` parameter instead.
:::

`control_global` ignores `view` (it is not resolved by id), and `binding_call` always resolves its id across all open views. For `bind_element`, `view` selects the slot to element-bind (see above).
