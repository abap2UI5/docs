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
If the backend needs more details about the event, use the `arg` parameter to add extra info. Three prefixes are available:

- **`$source`** — the UI5 control that fired the event (e.g., `${$source>/text}` returns the button text)
- **`$parameters`** — the event parameters defined by the UI5 control (e.g., `${$parameters>/id}` returns the element ID)
- **`$event`** — the UI5 event object itself (e.g., `$event>sId` returns the event type like `press`). Note: unlike the other two prefixes, `$event` is written without the `${...}` wrapper and without a leading `/` — see the [Event](#event) section below.

`arg` carries one value, which is what most events need. For two or more, use `t_arg` instead and pass them as a table — ``t_arg = VALUE #( ( `${$source>/text}` ) ( `${$parameters>/id}` ) )`` — and read them back in the same order with `client->get_event_arg( 2 )`, `( 3 )` and so on. The two are the same wire: `arg = x` is exactly ``t_arg = VALUE #( ( x ) )``.

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
                                          val = `BUTTON_POST`
                                          " reads the button text → result: "post"
                                          arg = `${$source>/text}` ) ).

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
                                          val = `BUTTON_POST`
                                          " reads the event parameter 'id' → result: "mainView--button_id"
                                          arg = `${$parameters>/id}` ) ).

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
                                          val = `BUTTON_POST`
                                          " reads an event-object attribute → result: "press"
                                          arg = `$event>sId` ) ).

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

## A Control in Another View Slot

The forms above read the control that fired, the event object, or a bound
model property. None of them reaches a **different** control — an id is local
to the view or fragment it was written in, and a `${...}` is a binding path,
so it addresses data and never a control.

`$controller.slotValue( slot, id, getter )` reads what a control in a named
view slot currently holds. The classic case is a dialog whose value the button
that closes it has to carry:

```abap
)->a( n = `press` v = client->_event(
                        val = `SAVE`
                        arg = `$controller.slotValue('POPUP','myEditor','getImagePngDataURL')` ) ).
```

```abap
CASE client->get( )-event.
    WHEN `SAVE`.
        DATA(image) = client->get_event_arg( ).
        client->popup_destroy( ).
ENDCASE.
```

The slot is one of the `cs_view` keys — `MAIN`, `NEST`, `NEST2`, `POPUP`,
`POPOVER` — and an **empty** slot searches every open one and then the global
registry, the same resolution `cs_event-control_by_id` uses for
`cs_view-main`.

The getter takes no arguments on purpose. To *call* a control, use
`cs_event-control_by_id`, which has a method whitelist in front of it; this is
a read.

::: tip It never throws, it logs
A closed slot, an unknown id, a method the control does not have, or a getter
that raises — each is written to the browser console and sent as the empty
string. That is deliberate: an argument expression is evaluated while UI5
dispatches the handler, so an expression that throws loses the whole **event**,
leaving a button that does nothing and an app with no way to tell.
:::

`$controller.slotById( slot, id )` hands the control itself over, for a
null-tolerant reader such as `$controller.textPath( )`. It answers `null` on a
miss — and a method call on that `null` is exactly the throw the tip above
describes, so prefer `slotValue( )` whenever you want a value.

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
                                          val = `BUTTON_POST`
                                          arg = `$` && client->_bind( name ) ) ).

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

Complete apps from the [sample catalog](https://abap2ui5.github.io/playground/samples/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Extra Arguments with t_arg | [`Z2UI5_CL_SMP_APP_167`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_167.clas.abap) |
| Control Objects in t_arg (FacetFilter) | [`Z2UI5_CL_SMP_APP_197`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_197.clas.abap) |

<!-- samples:end -->
