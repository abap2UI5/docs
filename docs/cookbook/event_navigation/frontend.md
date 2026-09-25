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
---
# Frontend

If you don't want to handle the event in the backend, fire actions directly on the frontend. The difference between the two event styles:

- **`client->_event( )`** — causes a backend roundtrip; the event runs in the `main` method
- **`client->follow_up_action( )`** — runs an action in the browser; no backend call

## The two positions

`follow_up_action( )` is **one method in two positions**, and the position is
what decides *when* the browser runs the action. The call is the same either
way — same `val`, same `t_arg`, same `view` — so there is one method to learn
and not two.

**In a view attribute**, its result is a string that is consumed as the
attribute's value, and the action is wired to the control. The browser runs it
when the user presses, with no roundtrip at all: the backend never hears about
the click.

```abap
)->tag( `Button`
    )->a( n = `text`  v = `reload`
    )->a( n = `press` v = client->follow_up_action( client->cs_event-location_reload ) )
```

(An event that takes no arguments needs no `t_arg` at all, and `val` can be
passed positionally, as here.)

**As a statement** in `main`, the same call is scheduled instead of wired, and
the browser runs it once the response arrives — i.e. *after* your backend work
is done. This is the follow-up in the name: finish the event handler, then have
the frontend do one more thing.

```abap
CASE client->get( )-event.
  WHEN `SAVE`.
    save( ).
    client->message_toast_display( `saved` ).
    client->follow_up_action(               " runs in the browser after this response
        val   = client->cs_event-set_title
        t_arg = VALUE #( ( |Invoice { mv_id }| ) ) ).
ENDCASE.
```

Neither form is a special case of the other, and the same events are available
in both: everything in the table below can be wired to a control *or* scheduled
after a roundtrip. Pick the position by when the action has to happen, not by
what it does.


## The frontend events

The frontend events, as `z2ui5_if_client=>cs_event` carries them:

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
      set_title                 TYPE string VALUE `SET_TITLE`,
      set_title_launchpad       TYPE string VALUE `SET_TITLE_LAUNCHPAD`,
      set_favicon               TYPE string VALUE `SET_FAVICON`,
      set_focus                 TYPE string VALUE `SET_FOCUS`,
      scroll_to                 TYPE string VALUE `SCROLL_TO`,
      scroll_into_view          TYPE string VALUE `SCROLL_INTO_VIEW`,
      start_timer               TYPE string VALUE `START_TIMER`,
      keyboard_shortcut         TYPE string VALUE `KEYBOARD_SHORTCUT`,
      open_new_tab              TYPE string VALUE `OPEN_NEW_TAB`,
      location_reload           TYPE string VALUE `LOCATION_RELOAD`,
      system_logout             TYPE string VALUE `SYSTEM_LOGOUT`,
      download_b64_file         TYPE string VALUE `DOWNLOAD_B64_FILE`,
      urlhelper                 TYPE string VALUE `URLHELPER`,
      store_data                TYPE string VALUE `STORE_DATA`,
      play_audio                TYPE string VALUE `PLAY_AUDIO`,

      "Control calls (positional t_arg)
      control_by_id             TYPE string VALUE `CONTROL_BY_ID`,
      control_global            TYPE string VALUE `CONTROL_GLOBAL`,
      binding_call              TYPE string VALUE `BINDING_CALL`,
      bind_element              TYPE string VALUE `BIND_ELEMENT`,

      "Smart controls (sap.ui.comp)
      smart_variant_init        TYPE string VALUE `SMART_VARIANT_INIT`,
      filter_bar_variant_init   TYPE string VALUE `FILTER_BAR_VARIANT_INIT`,

      "URL and app state - the hash_* family is named after UI5's
      "sap/ui/core/routing/HashChanger
      hash_set                  TYPE string VALUE `SET_PUSH_STATE`,
      hash_replace              TYPE string VALUE `HASH_REPLACE`,
      hash_back                 TYPE string VALUE `HASH_BACK`,
      hash_attach_changed       TYPE string VALUE `HASH_ATTACH_CHANGED`,
      hash_routing              TYPE string VALUE `SET_NAV_ROUTING`,
      app_state_set_active      TYPE string VALUE `SET_APP_STATE_ACTIVE`,

    END OF cs_event.
```

Some of these events have their own pages: [`keyboard_shortcut`](/cookbook/browser_interaction/keyboard_shortcuts) binds key combinations to backend events, [the `hash_*` family](/cookbook/event_navigation/navigation/hash) puts the URL fragment under the router's control or the app's own, and [`smart_variant_init` / `filter_bar_variant_init`](/cookbook/expert_more/smart_controls) wire variant management for smart controls. The dedicated cookbook pages under [Browser Interaction](/cookbook/browser_interaction/title) and [Device Capabilities](/cookbook/device_capabilities/upload_download) carry the argument list of each event.

For example, to open a new tab directly from a button press (no backend involved):
```abap
METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->tag( `Button`
                    )->a( n = `text`  v = `open new tab`
                    )->a( n = `press` v = client->follow_up_action(
                                             val   = client->cs_event-open_new_tab
                                             t_arg = VALUE #( ( `https://github.com/abap2UI5` ) ) ) ).

    client->view_display( view->stringify( ) ).


ENDMETHOD.
```

## Calling control methods on the frontend

The control-call constants — `control_by_id`, `control_global`, `binding_call` and `bind_element` — are frontend events too, but instead of a fixed built-in action they operate on a control, a global object, a binding or a whole view slot. Their arguments are **positional**: an empty argument between two filled ones keeps its slot as `` `` ``.

| Event            | `t_arg` (positional)                                                                 |
| ---------------- | ------------------------------------------------------------------------------------ |
| `control_by_id`  | `id`, `method`, `params…` — call a method on a control resolved by id                 |
| `control_global` | `object`, `method`, `params…` — `MESSAGE_TOAST`, `MESSAGE_BOX`, `BUSY_INDICATOR`, `THEMING`, `POPUP`, `INVISIBLE_MESSAGE`, `FORMATTING`, `ICON_POOL` |
| `binding_call`   | `id`, `aggregation`, `method`, `params…` — e.g. `filter` (path, operator, value1, value2) or `sort` (path, descending, group) on the aggregation's binding |
| `bind_element`   | `index`, `_bind( table )` — element-bind a whole view slot to a table row, see below  |

For `control_by_id`, any public control method is callable as long as it is not on the framework's **denylist**: methods that would break abap2UI5's own invariants (destroying views, re-rendering, detaching the framework's handlers, …) are blocked, ordinary setters and toggles (`setVisible`, `toggleBy`, `enablePostButton`, …) simply work. A small set of methods is additionally special-cased for typed arguments. `control_global` and `binding_call` remain strict whitelists — only the listed global objects and the binding methods `filter` / `sort` are callable. Three of those objects are less obvious than the rest: `POPUP-setWithinArea` confines every popup to one control instead of to the window (UI5 &ge; 1.89; an empty argument releases it again), `INVISIBLE_MESSAGE-announce` reads a text out to a screen reader without rendering it (UI5 &ge; 1.78; `t_arg` = text, mode), and `FORMATTING-setCustomCurrencies` / `-addCustomCurrencies` register currency codes the standard `sap.ui.model.type.Currency` does not know (UI5 &ge; 1.120) — `set…` REPLACES the whole registration, `add…` MERGES codes into it.

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

The same events also work as a **statement** in your `main` method, with the identical `t_arg` — then the browser runs them after the response arrives, once your backend work is done:

```abap
" after backend processing, let the wizard advance: setNextStep is the
" STEP's method, so the current step is the control addressed
client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `STEP1` ) ( `setNextStep` ) ( `STEP2` ) ) ).
```


### Element-binding a view slot: `bind_element`

`bind_element` binds a whole view slot (popup, popover, main, …) to one row of a bound table — the abap2UI5 equivalent of `oControl.bindElement( )`. All *relative* bindings in that slot (`{NAME}`, `{CATEGORY}`, nested aggregations) then resolve against the selected row, so a detail popup needs no data copied into event arguments:

```abap
" element-bind the popup slot to row <index> of t_product
client->follow_up_action(
    val   = client->cs_event-bind_element
    view  = client->cs_view-popup
    t_arg = VALUE #( ( index ) ( client->_bind( t_product ) ) ) ).
```

The `view` parameter selects the slot to bind; `t_arg` carries the row index and the table's binding path. See `Z2UI5_CL_SMP_APP_470` in the [samples repository](https://github.com/abap2UI5/samples) for a complete example.

### The `view` parameter

For `control_by_id`, the control is looked up by id. `follow_up_action( )` takes a separate `view` parameter (default `cs_view-main`) that scopes this lookup:

- omit it (or pass `cs_view-main`) — the id is resolved across all open views;
- pass `cs_view-popup` / `cs_view-popover` / `cs_view-nested` / … — the lookup is scoped to a control hosted in that view (e.g. a control living inside a popup).

```abap
" call a method on a control that lives inside the popup view
press = client->follow_up_action(
    val   = client->cs_event-control_by_id

    view  = client->cs_view-popup
    t_arg = VALUE #( ( `NavCon` ) ( `to` ) ( `${$parameters>/selectedKey}` ) ) )
```

::: warning Migrated from a positional view slot
The view used to be the second entry of `t_arg` (`id`, `view`, `method`, …). It is now the dedicated `view` importing parameter, and the framework injects it into the argument list itself. Older examples that still pass `` `MAIN` `` as the second `t_arg` element **no longer work** — the extra entry shifts every argument by one and the call fails on the frontend. Drop the positional view entry and use the `view` parameter instead.
:::

`control_global` ignores `view` (it is not resolved by id), and `binding_call` always resolves its id across all open views. For `bind_element`, `view` selects the slot to element-bind (see above).

## Raw JavaScript

There is no third way. `follow_up_action( )` used to take a raw JavaScript
expression as `val` and run it in the browser; that form is gone. A `val` the
frontend does not know as an event name is not run - it arrives as an unknown
event and nothing happens. What the raw form was reached for has a
`cs_event-*` equivalent: `control_global` for the UI5 globals (`MessageToast`,
`MessageBox`, `BusyIndicator`, …), `control_by_id` for a control method,
`hash_back` for `history.back( )`. Frontend code of the app's own ships as a
[Custom Control](/advanced/extensibility/custom_control) in the customer
frontend BSP, with a defined interface and reviewable code - not as a string
assembled in ABAP, which is what the raw form was: a `<script>` from the
backend, executed with the user's privileges, outside UI5's output encoding and
against any strict Content Security Policy.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/playground/samples/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Link with preventDefault (A) | [`Z2UI5_CL_SMP_APP_472`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_472.clas.abap) |
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
