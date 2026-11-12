---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_472
  - z2ui5_cl_smp_app_470
  - z2ui5_cl_smp_app_448
  - z2ui5_cl_smp_app_088
  - z2ui5_cl_smp_app_202
  - z2ui5_cl_smp_app_449
  - z2ui5_cl_smp_app_465
  - z2ui5_cl_smp_app_513
  - z2ui5_cl_smp_app_514
  - z2ui5_cl_smp_app_515
  - z2ui5_cl_smp_app_518
description: Frontend actions in abap2UI5 - client->follow_up_action( ) runs a browser-side action without a roundtrip when written into the view, or after the response when called in main( ); the control calls and the view parameter.
---
# Frontend Actions

A frontend action is something the browser does by itself: close a popup, set
the focus, copy a text, open a tab. The app asks for it with
`client->follow_up_action( )`. Where you write the call decides when the
browser runs it:

| Where you write it | When the browser runs it | Roundtrip |
|---|---|---|
| in a view attribute, e.g. as the value of `press` | when the user clicks | none: `main` is not called |
| as a statement in `main` | after the response of this roundtrip has arrived | the one that is already running |

Compare this with `client->_event( )`, which always sends the click to the
backend and runs `main`: see [Handling Events in ABAP](/cookbook/event_navigation/backend).

![Three timelines: _event( ) sends the click to main( ) and gets a response; follow_up_action( ) in a view attribute runs in the browser only, with no roundtrip; follow_up_action( ) as a statement in main( ) runs in the browser after the response](/cookbook/event_navigation/frontend-action.svg)

## The two positions

The call is the same in both positions: same `val`, same `t_arg`, same `view`.
There is one method to learn.

**In a view attribute**, the result of the call is the value of the
attribute. The action is attached to the control, and the browser runs it
when the user clicks. The backend is not called:

```abap
)->tag( `Button`
    )->a( n = `text`  v = `close`
    )->a( n = `press` v = client->follow_up_action( client->cs_event-popup_close ) )
```

(An action that takes no arguments needs no `t_arg`, and `val` can be passed
without its name, as here.)

**As a statement** in `main`, the same call is scheduled. The browser runs
the action once the response arrives, so *after* your backend work is done.
This is the follow-up in the name: finish the event handler, then let the
frontend do one more thing.

```abap
IF client->check_on_event( `SAVE` ).
  save( ).
  client->message_toast_display( `saved` ).
  client->follow_up_action(               " runs in the browser after this response
      val   = client->cs_event-set_title
      t_arg = VALUE #( ( |Invoice { mv_id }| ) ) ).
ENDIF.
```

Every action in the list below works in both positions. Choose the position
by *when* the action has to happen, not by what it does.


## The Frontend Actions {#the-frontend-events}

Every frontend action is a constant of `z2ui5_if_client=>cs_event`, and the
generated [client API reference](/resources/api) lists all of them with their
arguments, straight from the interface on `main`. The families: the two
`*_close` events for the popup and the popover, the browser actions
(`clipboard_copy`, `set_title`, `set_favicon`, `open_new_tab`,
`download_b64_file`, `system_logout`, …), the view actions (`set_focus`,
`scroll_to`, `scroll_into_view`, `set_size_limit`, `set_odata_model`), the
control calls (`control_by_id`, `control_global`, `binding_call`,
`bind_element`), the smart-control handshakes, the launchpad calls
(`cross_app_nav_to_ext`, `cross_app_nav_to_prev_app`,
`cross_app_nav_get_links`, `set_title_launchpad`, `set_app_full_width`), the
`hash_*` and `app_state_*` family, `start_timer`, `keyboard_shortcut`,
`store_data`, `play_audio`, `urlhelper` and `offline_queue`.

Two of them take an argument that is easy to miss.

`start_timer` raises a backend event once after a delay (`t_arg` = event
name, milliseconds). An optional **third** argument `X` keeps the global busy
indicator off for that tick. It is the `check_no_busy` of `_event( )` for a
timer: a poll whose backend call takes longer than a second no longer flashes
the overlay.

`offline_queue` (experimental) names the events of this app that wait in the
browser while the backend cannot be reached. Without it they fail. `t_arg` is
the toast text shown when an event was kept (`&1` is how many wait; empty for
no toast), then the event names. The events are kept with their model changes
across a reload and sent in order once the network is back. The page itself
still needs the network to load.

Some of these actions have their own pages:

- [`keyboard_shortcut`](/cookbook/browser_interaction/keyboard_shortcuts)
  binds key combinations to backend events
- [the `hash_*` family](/cookbook/event_navigation/navigation/hash) lets the
  router or the app itself control the URL fragment
- [`smart_variant_init` / `filter_bar_variant_init`](/cookbook/expert_more/smart_controls)
  set up variant management for smart controls

The pages under [Browser Interaction](/cookbook/browser_interaction/title) and
[Device Capabilities](/cookbook/device_capabilities/upload_download) carry the
argument list of each action.

For example, to open a new tab directly from a button press (no backend
involved):
```abap
METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->tag( `Button`
                      )->a( n = `text`  v = `open new tab`
                      )->a( n = `press` v = client->follow_up_action(
                                               val   = client->cs_event-open_new_tab
                                               t_arg = VALUE #( ( `/sap/bc/z2ui5?app_start=zcl_my_report` ) ) ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

ENDMETHOD.
```

The URL has to be on the app's own origin; the frontend refuses any other
with an error message. The same holds for `location_reload`, which navigates
the current tab to the URL in its one argument. For an external site, use
`urlhelper` with `REDIRECT`. It takes the target as a JSON argument and
accepts any `http` or `https` address:

```abap
client->follow_up_action(
    val   = client->cs_event-urlhelper
    t_arg = VALUE #( ( `REDIRECT` )
                     ( `{"URL":"https://github.com/abap2UI5","NEW_WINDOW":true}` ) ) ).
```

## Calling control methods on the frontend

The control-call constants — `control_by_id`, `control_global`, `binding_call`
and `bind_element` — are frontend actions too, but instead of a fixed built-in
action they operate on a control, a global object, a binding or a whole view
slot. Their arguments are **positional**: an empty argument between two filled
ones keeps its slot as ``` `` ```.

| Event            | `t_arg` (positional)                                                                 |
| ---------------- | ------------------------------------------------------------------------------------ |
| `control_by_id`  | `id`, `method`, `params…` — call a method on a control resolved by id                 |
| `control_global` | `object`, `method`, `params…` — `MESSAGE_TOAST`, `MESSAGE_BOX`, `BUSY_INDICATOR`, `THEMING`, `POPUP`, `INVISIBLE_MESSAGE`, `FORMATTING`, `ICON_POOL` |
| `binding_call`   | `id`, `aggregation`, `method`, `params…` — e.g. `filter` (path, operator, value1, value2) or `sort` (path, descending, group) on the aggregation's binding |
| `bind_element`   | `index`, `_bind( table )` — element-bind a whole view slot to a table row, see below  |

For `control_by_id`, any public control method can be called unless it is on
the framework's **denylist**. The denylist blocks methods that would break
abap2UI5 itself: destroying views, re-rendering, detaching the framework's
handlers. Ordinary setters and toggles (`setVisible`, `toggleBy`,
`enablePostButton`, …) simply work. A small set of methods also gets typed
arguments.

`control_global` and `binding_call` are strict allow lists: only the listed
global objects and the binding methods `filter` / `sort` can be called. Three
of those objects are less obvious than the rest:

- `POPUP-setWithinArea` confines every popup to one control instead of to the
  window (UI5 &ge; 1.89; an empty argument releases it again)
- `INVISIBLE_MESSAGE-announce` reads a text out to a screen reader without
  rendering it (UI5 &ge; 1.78; `t_arg` = text, mode)
- `FORMATTING-setCustomCurrencies` / `-addCustomCurrencies` register currency
  codes the standard `sap.ui.model.type.Currency` does not know (UI5 &ge;
  1.120). `set…` REPLACES the whole registration, `add…` MERGES codes into
  it.

```abap
" toggle a MessagePopover open, anchored to the pressing button, no roundtrip
press = client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `msgPopover` ) ( `toggleBy` ) ( `${$source>/id}` ) ) )
```

A `t_arg` argument that starts with a brace travels as **real JSON** rather than
as a string. That is how `MESSAGE_TOAST` and `MESSAGE_BOX` take their UI5
options — the object is the last argument of the call and is the option object
of `sap.m.MessageToast.show( )` / `sap.m.MessageBox.<type>( )` 1:1, with the box
type as the method:

```abap
client->follow_up_action(
    val   = client->cs_event-control_global
    t_arg = VALUE #( ( `MESSAGE_BOX` ) ( `error` ) ( `Not saved.` )
                     ( `{"contentWidth":"30rem","icon":"WARNING"}` ) ) ).
```

Which options belong here and which stay on `client->message_box_display( )` is
the subject of [Message](/cookbook/translation_messages/message).

The same events also work as a **statement** in your `main` method, with the
identical `t_arg` — then the browser runs them after the response arrives, once
your backend work is done:

```abap
" after backend processing, let the wizard advance: setNextStep is the
" STEP's method, so the current step is the control addressed
client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `STEP1` ) ( `setNextStep` ) ( `STEP2` ) ) ).
```


### Element-binding a view slot: `bind_element`

`bind_element` binds a whole view slot (popup, popover, main, …) to one row of a
bound table — the abap2UI5 equivalent of `oControl.bindElement( )`. All
*relative* bindings in that slot (`{NAME}`, `{CATEGORY}`, nested aggregations)
then resolve against the selected row, so a detail popup needs no data copied
into event arguments:

```abap
" element-bind the popup slot to row <index> of t_product
client->follow_up_action(
    val   = client->cs_event-bind_element
    view  = client->cs_view-popup
    t_arg = VALUE #( ( index ) ( client->_bind( t_product ) ) ) ).
```

The `view` parameter selects the slot to bind; `t_arg` carries the row index and
the table's binding path. See `Z2UI5_CL_SMP_APP_470` in the
[samples repository](https://github.com/abap2UI5/samples) for a complete
example.

### The `view` parameter

For `control_by_id`, the control is looked up by id. `follow_up_action( )` takes
a separate `view` parameter (default `cs_view-main`) that scopes this lookup:

- omit it (or pass `cs_view-main`) — the id is resolved across all open views;
- pass `cs_view-popup` / `cs_view-popover` / `cs_view-nested` / … — the lookup
  is scoped to a control hosted in that view (e.g. a control living inside a
  popup).

```abap
" call a method on a control that lives inside the popup view
press = client->follow_up_action(
    val   = client->cs_event-control_by_id

    view  = client->cs_view-popup
    t_arg = VALUE #( ( `NavCon` ) ( `to` ) ( `${$parameters>/selectedKey}` ) ) )
```

The slot is only ever the `view` parameter — a `` `MAIN` `` written into `t_arg`
shifts every argument by one and the call fails on the frontend; see
[Deprecations](/resources/deprecations#the-view-slot-of-a-control-call).

`control_global` ignores `view` (it is not resolved by id), and `binding_call`
always resolves its id across all open views. For `bind_element`, `view` selects
the slot to element-bind (see above).

## Raw JavaScript

The backend cannot send JavaScript to the browser. A `val` that is not one of
the `cs_event` names is not run: it arrives as an unknown action and nothing
happens. Everything a script was typically used for has a `cs_event-*`
replacement:
`control_global` for the UI5 globals (`MessageToast`, `MessageBox`,
`BusyIndicator`, …), `control_by_id` for a control method, `hash_back` for
`history.back( )`. Frontend code of the app's own ships as a
[Custom Control](/advanced/extensibility/custom_control) in the customer
frontend BSP, with a defined interface and reviewable code; the removed form
is on [Deprecations](/resources/deprecations#the-z2ui5-frontend-global).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Link with preventDefault | [`Z2UI5_CL_SMP_APP_472`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_472.clas.abap) |
| Element Binding to the Selected Row (A) | [`Z2UI5_CL_SMP_APP_470`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_470.clas.abap) |
| Expand a Panel by ID (setExpanded) (A) | [`Z2UI5_CL_SMP_APP_448`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_448.clas.abap) |
| Switch NavContainer Page by ID (A) | [`Z2UI5_CL_SMP_APP_088`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_088.clas.abap) |
| Wizard with Steps (A) | [`Z2UI5_CL_SMP_APP_202`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_202.clas.abap) |
| Open the PDF Viewer by ID (A) | [`Z2UI5_CL_SMP_APP_449`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_449.clas.abap) |
| Toggle by ID (toggleBy) (A) | [`Z2UI5_CL_SMP_APP_465`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_465.clas.abap) |
| Inline CSS on a Control (css) (A) | [`Z2UI5_CL_SMP_APP_513`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_513.clas.abap) |
| Aggregation Item by Index (A) | [`Z2UI5_CL_SMP_APP_514`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_514.clas.abap) |
| The Global Busy Indicator (A) | [`Z2UI5_CL_SMP_APP_515`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_515.clas.abap) |
| Register an Icon Font (A) | [`Z2UI5_CL_SMP_APP_518`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_518.clas.abap) |

<!-- samples:end -->
