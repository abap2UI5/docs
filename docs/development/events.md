---
outline: [2, 4]
---
# Events

UI5 control properties can both display data and trigger events. This section covers backend events, frontend events, and follow-up actions.

### Backend
To trigger backend processing when an event occurs, use the `client->_event` method.

#### Basic
As an example, we use the `press` property of a button. To trigger events in the backend, assign the result of `client->_event( 'MY_EVENT_NAME' )` to the relevant UI5 control property. The backend can then retrieve the event details with `client->get( )-event`.

```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button(
            text  = `post`
            press = client->_event( `BUTTON_POST` )
        )->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( `The button was pressed` ).
    ENDCASE.

ENDMETHOD.
```
If the backend needs additional information about the specific event, use the `t_arg` parameter to include extra details. Three special prefixes are available:

- **`$source`** — the UI5 control that triggered the event (e.g., `${$source>/text}` returns the button text)
- **`$parameters`** — the event parameters as defined by the UI5 control (e.g., `${$parameters>/id}` returns the element ID)
- **`$event`** — the UI5 event object itself (e.g., `$event>sId` returns the event type like `press`)

For more details, see the [UI5 documentation on event handler arguments](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe) and sample `Z2UI5_CL_DEMO_APP_167`.

#### Source
Send properties of the event source control to the backend. The syntax `${$source>/text}` reads the `text` property from the UI5 control that fired the event — here the button itself, yielding the button's label (`post`):

```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` press = client->_event(
            val = `BUTTON_POST`
            "reads button text → result: "post"
            t_arg = VALUE #( ( `${$source>/text}` ) ) )
        )->stringify( ) ).

    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
          client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.
```

#### Parameters
Retrieve parameters of the event. The syntax `${$parameters>/id}` reads the `id` parameter from the event's parameter map — UI5 generates a qualified ID like `mainView--button_id`:
```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` id = `button_id` press = client->_event(
            val = `BUTTON_POST`
            "reads event parameter 'id' → result: "mainView--button_id"
            t_arg = VALUE #( ( `${$parameters>/id}` ) ) )
        )->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( |The button id is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.
```

#### Event
Retrieve specific properties of the event object. The syntax `$event>sId` accesses the `sId` attribute of the UI5 event — here it returns the event type name (`press`). Note: no `${...}` wrapper here because `$event` directly references the event object:
```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->button( text = `post` press = client->_event(
            val = `BUTTON_POST`
            "reads event object attribute → result: "press"
            t_arg = VALUE #( ( `$event>sId` ) ) )
        )->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( |The event id is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.
```
::: warning
You can access any object attribute, but only use public and released attributes to avoid compatibility issues with future UI5 versions.
:::

#### Model Properties
Retrieve model properties associated with the event:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
         )->input( client->_bind_edit( name )
        )->button( text = `post` press = client->_event(
            val   = `BUTTON_POST`
            t_arg = VALUE #( ( `$` && client->_bind_edit( name ) ) ) )
        )->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( |The name is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.

ENDCLASS.
```

::: tip
This is a demonstration. In this case, accessing `name` directly would be easier, since the framework updates it automatically.
:::

### Frontend
If you don't want to process the event in the backend, trigger actions directly on the frontend using `client->_event_client`. The difference between the two methods:

- **`client->_event( )`** — triggers a backend roundtrip; the event is processed in the `main` method
- **`client->_event_client( )`** — executes an action directly in the browser; no backend call

To use a frontend event on a UI5 control property (like `press`), wrap `_event_client` inside `_event`. To execute a frontend event after backend processing, pass `_event_client` to `client->follow_up_action`.

The following frontend events exist:
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
For example, to open a new tab with the corresponding event:
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

### Follow-up Action
Sometimes you must call a backend function and then immediately act on the frontend. The follow-up action event handles this:
```abap
METHOD z2ui5_if_app~main.

    client->follow_up_action( client->_event_client(
        val   = client->cs_event-open_new_tab
        t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ) ).

ENDMETHOD.
```
See sample `Z2UI5_CL_DEMO_APP_180` for a working example.
