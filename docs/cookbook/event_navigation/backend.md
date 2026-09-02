---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_167
  - z2ui5_cl_smp_app_197
---
# Backend

UI5 control properties can both display data and fire events. To run backend logic when an event fires, use the `client->_event` method.

## Basic
As an example, we use the `press` property of a button. To fire events to the backend, assign the result of `client->_event( 'MY_EVENT_NAME' )` to the matching UI5 control property. The backend can then read the event details with `client->get( )-event`.

```abap
METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->tag( `Button`
                    )->a( n = `text`  v = `post`
                    )->a( n = `press` v = client->_event( `BUTTON_POST` ) ).

    client->view_display( view->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( `The button was pressed` ).
    ENDCASE.

ENDMETHOD.
```
If the backend needs more details about the event, use the `t_arg` parameter to add extra info. Three prefixes are available:

- **`$source`** — the UI5 control that fired the event (e.g., `${$source>/text}` returns the button text)
- **`$parameters`** — the event parameters defined by the UI5 control (e.g., `${$parameters>/id}` returns the element ID)
- **`$event`** — the UI5 event object itself (e.g., `$event>sId` returns the event type like `press`). Note: unlike the other two prefixes, `$event` is written without the `${...}` wrapper and without a leading `/` — see the [Event](#event) section below.

For details, see the [UI5 docs on event handler arguments](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe) and sample `Z2UI5_CL_SMP_APP_167`.

## Source
Send properties of the event source control to the backend. The syntax `${$source>/text}` reads the `text` property of the UI5 control that fired the event — here, the button itself, returning the button's label (`post`):

```abap
METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->tag( `Button`
                    )->a( n = `text`  v = `post`
                    )->a( n = `press` v = client->_event(
                                          val   = `BUTTON_POST`
                                          " reads the button text → result: "post"
                                          t_arg = VALUE #( ( `${$source>/text}` ) ) ) ).

    client->view_display( view->stringify( ) ).

    CASE client->get( )-event.
      WHEN `BUTTON_POST`.
          client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.
```

## Parameters
Read parameters of the event. The syntax `${$parameters>/id}` reads the `id` parameter out of the event's parameter map — UI5 builds a qualified ID like `mainView--button_id`:
```abap
METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->tag( `Button`
                    )->a( n = `id`    v = `button_id`
                    )->a( n = `text`  v = `post`
                    )->a( n = `press` v = client->_event(
                                          val   = `BUTTON_POST`
                                          " reads the event parameter 'id' → result: "mainView--button_id"
                                          t_arg = VALUE #( ( `${$parameters>/id}` ) ) ) ).

    client->view_display( view->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( |The button id is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.
```

## Event
Read specific properties of the event object. The syntax `$event>sId` reads the `sId` attribute of the UI5 event — here it returns the event type name (`press`). Note: there's no `${...}` wrapper because `$event` directly references the event object:
```abap
METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->tag( `Button`
                    )->a( n = `text`  v = `post`
                    )->a( n = `press` v = client->_event(
                                          val   = `BUTTON_POST`
                                          " reads an event-object attribute → result: "press"
                                          t_arg = VALUE #( ( `$event>sId` ) ) ) ).

    client->view_display( view->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( |The event id is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.
```
::: warning
You can read any object attribute, but use only public and released attributes to avoid compatibility issues with future UI5 versions.
:::

## Model Properties
Read model properties bound to the event:
```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->tag( `Input`
                    )->a( n = `value` v = client->_bind( name )
                )->tag( `Button`
                    )->a( n = `text`  v = `post`
                    )->a( n = `press` v = client->_event(
                                          val   = `BUTTON_POST`
                                          t_arg = VALUE #( ( `$` && client->_bind( name ) ) ) ) ).

    client->view_display( view->stringify( ) ).

    CASE client->get( )-event.
        WHEN `BUTTON_POST`.
            client->message_box_display( |The name is { client->get_event_arg( ) }| ).
    ENDCASE.

ENDMETHOD.

ENDCLASS.
```

::: tip
This is just a demo. Reading `name` directly would be easier — the framework updates it automatically.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Extra Arguments with t_arg | [`Z2UI5_CL_SMP_APP_167`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_167.clas.abap) |
| Control Objects in t_arg (FacetFilter) | [`Z2UI5_CL_SMP_APP_197`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_197.clas.abap) |

<!-- samples:end -->
