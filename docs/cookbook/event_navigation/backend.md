---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_167
  - z2ui5_cl_smp_app_197
description: Backend events in abap2UI5 - client->_event( ) wires a control event to main( ), with arguments from the source control, the event parameters or the model, and the per-wire options in s_ctrl.
---
# Backend

UI5 control properties can both display data and fire events. To run backend
logic when an event fires, use the `client->_event` method.

## Basic
As an example, we use the `press` property of a button. To fire events to the
backend, assign the result of ``client->_event( `MY_EVENT_NAME` )`` to the
matching UI5 control property. The backend can then read the event details with
`client->get( )-event`.

```abap
METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->tag( `Button`
                      )->a( n = `text`  v = `post`
                      )->a( n = `press` v = client->_event( `BUTTON_POST` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `BUTTON_POST` ).
      client->message_box_display( `The button was pressed` ).

    ENDIF.

ENDMETHOD.
```
If the backend needs more details about the event, use the `arg` parameter to
add extra info. Three prefixes are available:

- **`$source`** — the UI5 control that fired the event (e.g., `${$source>/text}`
  returns the button text)
- **`$parameters`** — the event parameters defined by the UI5 control (e.g.,
  `${$parameters>/id}` returns the element ID)
- **`$event`** — the UI5 event object itself (e.g., `$event>sId` returns the
  event type like `press`). Note: unlike the other two prefixes, `$event` is
  written without the `${...}` wrapper and without a leading `/` — see the
  [Event](#event) section below.

`arg` carries one value, which is what most events need. For two or more, use
`t_arg` instead and pass them as a table — ``t_arg = VALUE #( (
`${$source>/text}` ) ( `${$parameters>/id}` ) )`` — and read them back in the
same order with `client->get_event_arg( 2 )`, `( 3 )` and so on. The two are the
same wire: `arg = x` is exactly ``t_arg = VALUE #( ( x ) )``.

For details, see the
[UI5 docs on event handler arguments](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe)
and sample `Z2UI5_CL_SMP_APP_167`.

## Source
Send properties of the event source control to the backend. The syntax
`${$source>/text}` reads the `text` property of the UI5 control that fired the
event — here, the button itself, returning the button's label (`post`):

```abap
METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

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

    ELSEIF client->check_on_event( `BUTTON_POST` ).
      client->message_box_display( |The button text is { client->get_event_arg( ) }| ).

    ENDIF.

ENDMETHOD.
```

## Parameters
Read parameters of the event. The syntax `${$parameters>/id}` reads the `id`
parameter out of the event's parameter map — UI5 builds a qualified ID like
`mainView--button_id`:
```abap
METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

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

    ELSEIF client->check_on_event( `BUTTON_POST` ).
      client->message_box_display( |The button id is { client->get_event_arg( ) }| ).

    ENDIF.

ENDMETHOD.
```

## Event
Read specific properties of the event object. The syntax `$event>sId` reads the
`sId` attribute of the UI5 event — here it returns the event type name
(`press`). Note: there's no `${...}` wrapper because `$event` directly
references the event object:
```abap
METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

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

    ELSEIF client->check_on_event( `BUTTON_POST` ).
      client->message_box_display( |The event id is { client->get_event_arg( ) }| ).

    ENDIF.

ENDMETHOD.
```
::: warning
You can read any object attribute, but use only public and released attributes
to avoid compatibility issues with future UI5 versions.
:::

## More Than One Argument

`get_event_arg( )` without a value reads the first argument, which is the only
one a wire written with `arg =` carries. A wire written with `t_arg` carries
as many as the table has rows, in that order, and `get_event_arg( n )` reads
position `n`; a position that does not exist answers the empty string, and the
whole list is `client->get( )-t_event_arg`:

```abap
)->a( n = `press` v = client->_event(
                        val   = `ROW_PRESS`
                        t_arg = VALUE #( ( `${$source>/title}` )
                                         ( `${$parameters>/id}` )
                                         ( `$event>sId` ) ) ) )
```

```abap
CASE client->get( )-event.
    WHEN `ROW_PRESS`.
        DATA(lv_title) = client->get_event_arg( ).
        DATA(lv_id)    = client->get_event_arg( 2 ).
        DATA(lv_type)  = client->get_event_arg( 3 ).
ENDCASE.
```

An argument that starts with `$` or `{` is written into the view **raw**, as a
live UI5 expression — that is how `${$source>/KEY}` reaches the handler as the
row's value. The flip side is that data which may start with those characters
(text a user typed, a key from a foreign system) would be *evaluated* rather
than passed. A wire that carries data rather than bindings sets
`s_ctrl-check_arg_literal`, which quotes every argument of that wire as a
string; see the next section.

## The Per-Wire Options: `s_ctrl`

`_event( )` takes a third parameter, `s_ctrl`, a structure of flags that are
baked into the wire when the view is built. Each one answers a question a
plain `press` never asks:

| Flag | What it decides |
|---|---|
| `check_queue_last` | While a roundtrip is in flight, the **last** event fired on this wire is kept and dispatched once the response has landed, instead of being dropped. One roundtrip at a time, order preserved, and the backend ends on the control's current value. This is the flag for a per-keystroke wire — `liveChange`, `liveSearch`, `sliderChange` — which without it loses every keystroke typed while a roundtrip runs, the last one included |
| `check_no_busy` | Keeps the global busy indicator down for this wire. The roundtrip, the busy *state* and the guard are unchanged; only the full-screen overlay is not shown. For a per-keystroke wire the overlay is the wrong feedback — the user is looking at the field, not waiting for a screen — and from the second character on it flashes over the field. Pair it with `check_queue_last`: nothing is lost and nothing blinks |
| `check_prevent_default` | Cancels the control's built-in default for this event before the roundtrip (`oEvent.preventDefault( )`) — a `sap.tnt` `NavigationListItem` press that must not select the item, a `Link` that must not follow its `href`. The event is still sent, so the backend decides what happens instead |
| `prevent_default_expr` | The same veto decided **per firing** instead of per wire: a client expression evaluated when the event fires, which cancels the default only when it is truthy, e.g. `${$parameters>/column}.getId().indexOf('COL_DATE') >= 0` — one wire protects one column and lets the rest through. Wins over the flag when both are set |
| `check_arg_literal` | Quotes **every** argument of this wire as a string. The form for a wire whose arguments are data rather than bindings — see [More Than One Argument](#more-than-one-argument) above |

The live search is the case that needs two of them at once:

```abap
)->tag( `SearchField`
    )->a( n = `value`      v = client->_bind( mv_search )
    )->a( n = `liveChange` v = client->_event(
                                 val    = `SEARCH`
                                 s_ctrl = VALUE #( check_queue_last = abap_true
                                                   check_no_busy    = abap_true ) ) )
```

Without the first flag the busy guard drops every keystroke that lands on a
roundtrip already in flight, and the list stays at the value of the last
*completed* roundtrip until the user pauses and types again. Without the
second one the overlay flashes over the field while they type. Sample
`Z2UI5_CL_SMP_APP_059` on [Tables](/cookbook/model/tables) is the live search
written out in full.

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

`$controller.textPath( control )` is the other helper that addresses the live
control tree rather than data: it answers the ancestor-text breadcrumb of the
control that fired — the titles and labels on the way up from it — so a
handler can tell which panel or tab a press came from without an id of its
own: ``arg = `$controller.textPath( ${$parameters>/item} )` ``.

## Model Properties
Read model properties bound to the event:
```abap
CLASS zcl_sample_event_model DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_event_model IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

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

    ELSEIF client->check_on_event( `BUTTON_POST` ).
      client->message_box_display( |The name is { client->get_event_arg( ) }| ).

    ENDIF.

ENDMETHOD.

ENDCLASS.
```

::: tip
This is just a demo. Reading `name` directly would be easier — the framework
updates it automatically.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Extra Arguments with t_arg | [`Z2UI5_CL_SMP_APP_167`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_167.clas.abap) |
| Control Objects in t_arg (FacetFilter) | [`Z2UI5_CL_SMP_APP_197`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_197.clas.abap) |

<!-- samples:end -->
