---
outline: [2, 4]
---
# Frontend

If you don't want to handle the event in the backend, fire actions directly on the frontend. The difference between the two event styles:

- **`client->_event( )`** — causes a backend roundtrip; the event runs in the `main` method
- **`client->_event_client( )`** — runs an action directly in the browser; no backend call

Use `_event_client` on a UI5 control property (like `press`) when the response should happen entirely in the browser. To fire a frontend event **after** backend processing has finished, use [`client->action->gen`](./action.md) instead — it schedules the same frontend event but is called from the backend.

The following frontend events are available:
```abap
  CONSTANTS:
    BEGIN OF cs_event,
      popup_close               TYPE string VALUE `POPUP_CLOSE`,
      open_new_tab              TYPE string VALUE `OPEN_NEW_TAB`,
      popover_close             TYPE string VALUE `POPOVER_CLOSE`,
      location_reload           TYPE string VALUE `LOCATION_RELOAD`,
      nav_container_to          TYPE string VALUE `NAV_CONTAINER_TO`,
      nest_nav_container_to     TYPE string VALUE `NEST_NAV_CONTAINER_TO`,
      nest2_nav_container_to    TYPE string VALUE `NEST2_NAV_CONTAINER_TO`,
      cross_app_nav_to_ext      TYPE string VALUE `CROSS_APP_NAV_TO_EXT`,
      cross_app_nav_to_prev_app TYPE string VALUE `CROSS_APP_NAV_TO_PREV_APP`,
      popup_nav_container_to    TYPE string VALUE `POPUP_NAV_CONTAINER_TO`,
      download_b64_file         TYPE string VALUE `DOWNLOAD_B64_FILE`,
      set_size_limit            TYPE string VALUE `SET_SIZE_LIMIT`,
      set_title                 TYPE string VALUE `SET_TITLE`,
      set_title_launchpad       TYPE string VALUE `SET_TITLE_LAUNCHPAD`,
      set_focus                 TYPE string VALUE `SET_FOCUS`,
      scroll_to                 TYPE string VALUE `SCROLL_TO`,
      scroll_into_view          TYPE string VALUE `SCROLL_INTO_VIEW`,
      start_timer               TYPE string VALUE `START_TIMER`,
      keyboard_set_mode         TYPE string VALUE `KEYBOARD_SET_MODE`,
      clipboard_copy            TYPE string VALUE `CLIPBOARD_COPY`,
      clipboard_app_state       TYPE string VALUE `CLIPBOARD_APP_STATE`,
      history_back              TYPE string VALUE `HISTORY_BACK`,
    END OF cs_event.
```
For example, to open a new tab directly from a button press (no backend involved):
```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button(
            text  = `post`
            press = client->_event_client(
                val   = client->cs_event-open_new_tab
                t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) )
        )->stringify( ) ).

ENDMETHOD.
```
