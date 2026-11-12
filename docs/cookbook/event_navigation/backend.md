---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_167
  - z2ui5_cl_smp_app_197
description: Backend events in abap2UI5 - client->_event( ) attaches an event name to a control and main( ) handles it, with arguments from the source control, the event parameters or the model, and the options per event in s_ctrl.
---
# Backend

An event is what the user did: a button press, a row click, a changed field.
`client->_event( )` attaches an event name to a control, and
`client->check_on_event( )` asks for that name in `main`. Each event is one
roundtrip.

## Basic

Assign the result of ``client->_event( `MY_EVENT_NAME` )`` to the event
property of the control, here the `press` of a button. When the user presses
it, `main` runs and the branch with the same name is taken:

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

## Sending a Value with the Event {#source}

Often the backend needs to know more than the event name: which row, which
text, which key. The parameter `arg` sends one value along, and
`client->get_event_arg( )` reads it in the handler. Only two lines of the
example above change:

```abap
)->a( n = `press` v = client->_event(
                        val = `BUTTON_POST`
                        arg = `${$source>/text}` ) ).   " the text of the button
```

```abap
ELSEIF client->check_on_event( `BUTTON_POST` ).
  client->message_box_display( |The button text is { client->get_event_arg( ) }| ).
```

What `arg` contains decides where the value comes from:

| You write in `arg` | The handler receives | Example result |
|---|---|---|
| `${$source>/text}` | a property of the control that fired the event | `post` |
| `${$parameters>/id}` | a parameter of the UI5 event | `mainView--button_id` |
| `$event>sId` | an attribute of the UI5 event object | `press` |
| `${PRODUCT}` | a field of the table row the control stands in | the `PRODUCT` of that row |
| `` `$` && client->_bind( name ) `` | the current value of a bound attribute — see [Model Properties](#model-properties) | what the user typed |

### Parameters

`${$parameters>/id}` reads the parameter `id` of the event. Every UI5 control
documents the parameters of its events in the UI5 API reference; a `press`
has the `id` of the control, which UI5 qualifies with the view, for example
`mainView--button_id`.

### Event

`$event>sId` reads the attribute `sId` of the UI5 event object, which is the
name of the event (`press`). `$event` is written differently from the other
two: without the `${...}` around it and without a leading `/`, because it
names the event object itself.

::: warning
You can read any attribute of the event object, but use only public and
released ones. Others can change with the next UI5 version.
:::

For details, see the
[UI5 docs on event handler arguments](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe)
and sample `Z2UI5_CL_SMP_APP_167`.

## More Than One Argument

`arg` sends one value, which is what most events need. For two or more, use
`t_arg` and pass them as a table. `arg = x` is the short form of
``t_arg = VALUE #( ( x ) )``. In the handler, `get_event_arg( )` reads the
first value and `get_event_arg( n )` reads the value at position `n`. A
position that does not exist returns an empty string. The whole list is in
`client->get( )-t_event_arg`:

```abap
)->a( n = `press` v = client->_event(
                        val   = `ROW_PRESS`
                        t_arg = VALUE #( ( `${$source>/title}` )
                                         ( `${$parameters>/id}` )
                                         ( `$event>sId` ) ) ) )
```

```abap
IF client->check_on_event( `ROW_PRESS` ).
    DATA(lv_title) = client->get_event_arg( ).
    DATA(lv_id)    = client->get_event_arg( 2 ).
    DATA(lv_type)  = client->get_event_arg( 3 ).
ENDIF.
```

An argument that starts with `$` or `{` is written into the view **raw**, as a
live UI5 expression. That is how `${$source>/KEY}` reaches the handler as the
row's value. The same rule is a risk for plain data: a text a user typed, or a
key from a foreign system, may start with one of those characters and would
then be *evaluated* instead of passed. An event that sends data, not bindings,
sets `s_ctrl-check_arg_literal`. It quotes every argument of that event as a
string; see the next section.

## Options per Event: `s_ctrl` {#the-per-wire-options-s-ctrl}

`_event( )` takes a further parameter, `s_ctrl`. It is a structure of flags
that belong to this one `_event( )` call, the *event binding*, and are fixed
when the view is built. Each flag answers a question a plain `press` never
asks:

| Flag | What it decides |
|---|---|
| `check_queue_last` | While a roundtrip is in flight, the **last** event fired by this event binding is kept and dispatched once the response has landed, instead of being dropped. One roundtrip at a time, order preserved, and the backend ends on the control's current value. This is the flag for an event that fires on every keystroke — `liveChange`, `liveSearch`, `sliderChange` — which without it loses every keystroke typed while a roundtrip runs, the last one included |
| `check_no_busy` | Keeps the global busy indicator down for this event. The roundtrip, the busy *state* and the guard are unchanged; only the full-screen overlay is not shown. For an event on every keystroke the overlay is the wrong feedback — the user is looking at the field, not waiting for a screen — and from the second character on it flashes over the field. Pair it with `check_queue_last`: nothing is lost and nothing blinks |
| `check_prevent_default` | Cancels the control's built-in default for this event before the roundtrip (`oEvent.preventDefault( )`) — a `sap.tnt` `NavigationListItem` press that must not select the item, a `Link` that must not follow its `href`. The event is still sent, so the backend decides what happens instead |
| `prevent_default_expr` | The same veto decided **each time the event fires**, not once for the event binding: a client expression evaluated when the event fires, which cancels the default only when it is truthy, e.g. `${$parameters>/column}.getId().indexOf('COL_DATE') >= 0` — one event binding protects one column and lets the rest through. Wins over the flag when both are set |
| `check_arg_literal` | Quotes **every** argument of this event as a string. The form for an event whose arguments are data, not bindings — see [More Than One Argument](#more-than-one-argument) above |

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
IF client->check_on_event( `SAVE` ).
    DATA(image) = client->get_event_arg( ).
    client->popup_destroy( ).
ENDIF.
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
Send the current value of a bound attribute with the event. `_bind( )` returns
the binding path in braces, and the `$` in front turns it into an argument:
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
