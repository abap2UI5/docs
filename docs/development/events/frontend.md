---
outline: [2, 4]
---
# Frontend

If you don't want to handle the event in the backend, fire actions directly on the frontend with `client->_event_client`. The difference between the two methods:

- **`client->_event( )`** — causes a backend roundtrip; the event runs in the `main` method
- **`client->_event_client( )`** — runs an action directly in the browser; no backend call

To use a frontend event on a UI5 control property (like `press`), wrap `_event_client` inside `_event`. To fire a frontend event after backend processing, pass `_event_client` to `client->follow_up_action`.

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
    END OF cs_event.
```
For example, to open a new tab with the matching event:
```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button(
            text = `post`
            press = client->_event( client->_event_client(
                 val   = client->cs_event-open_new_tab
                 t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ) )
        )->stringify( ) ).

ENDMETHOD.
```
