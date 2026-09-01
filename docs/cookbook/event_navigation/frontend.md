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
      clipboard_app_state       TYPE string VALUE `CLIPBOARD_APP_STATE`,
      set_title                 TYPE string VALUE `SET_TITLE`,
      set_title_launchpad       TYPE string VALUE `SET_TITLE_LAUNCHPAD`,
      set_favicon               TYPE string VALUE `SET_FAVICON`,
      set_focus                 TYPE string VALUE `SET_FOCUS`,
      scroll_to                 TYPE string VALUE `SCROLL_TO`,
      scroll_into_view          TYPE string VALUE `SCROLL_INTO_VIEW`,
      start_timer               TYPE string VALUE `START_TIMER`,
      keyboard_set_mode         TYPE string VALUE `KEYBOARD_SET_MODE`,
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

      "URL and app state
      set_app_state_active      TYPE string VALUE `SET_APP_STATE_ACTIVE`,
      set_push_state            TYPE string VALUE `SET_PUSH_STATE`,
      set_nav_routing           TYPE string VALUE `SET_NAV_ROUTING`,

    END OF cs_event.
```

The interface carries a few more that are obsolete and are not listed here —
they still dispatch, so old code keeps running, and what replaced each one is
on [Deprecations](/resources/deprecations).
Some of these events have their own pages: [`keyboard_shortcut`](/cookbook/browser_interaction/keyboard_shortcuts) binds key combinations to backend events, [`set_nav_routing`](/cookbook/event_navigation/navigation/hash) switches hash routing on, and [`smart_variant_init` / `filter_bar_variant_init`](/cookbook/expert_more/smart_controls) wire variant management for smart controls. The dedicated cookbook pages under [Browser Interaction](/cookbook/browser_interaction/title) and [Device Capabilities](/cookbook/device_capabilities/upload_download) carry the argument list of each event.

::: tip These used to be invisible custom controls
Earlier versions of abap2UI5 needed an invisible custom UI5 control for each of
these interactions — title, focus, scrolling, timer, soft keyboard. Every one of
them is a built-in event now. The full old-control-to-event list is in
[Deprecations](/resources/deprecations#invisible-custom-controls).
:::

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
| `control_global` | `object`, `method`, `params…` — `MESSAGE_TOAST`, `MESSAGE_BOX`, `BUSY_INDICATOR`, `THEMING`, `POPUP`, `INVISIBLE_MESSAGE`, `FORMATTING` |
| `binding_call`   | `id`, `aggregation`, `method`, `params…` — e.g. `filter` (path, operator, value1, value2) or `sort` (path, descending, group) on the aggregation's binding |
| `bind_element`   | `index`, `_bind( table )` — element-bind a whole view slot to a table row, see below  |

For `control_by_id`, any public control method is callable as long as it is not on the framework's **denylist**: methods that would break abap2UI5's own invariants (destroying views, re-rendering, detaching the framework's handlers, …) are blocked, ordinary setters and toggles (`setVisible`, `toggleBy`, `enablePostButton`, …) simply work. A small set of methods is additionally special-cased for typed arguments. `control_global` and `binding_call` remain strict whitelists — only the listed global objects and the binding methods `filter` / `sort` are callable. Three of those objects are less obvious than the rest: `POPUP-setWithinArea` confines every popup to one control instead of to the window (UI5 &ge; 1.89; an empty argument releases it again), `INVISIBLE_MESSAGE-announce` reads a text out to a screen reader without rendering it (UI5 &ge; 1.78; `t_arg` = text, mode), and `FORMATTING-setCustomCurrencies` / `-addCustomCurrency` register currency codes the standard `sap.ui.model.type.Currency` does not know (UI5 &ge; 1.120) — `set…` REPLACES the whole registration, `add…` adds one code.

```abap
" toggle a MessagePopover open, anchored to the pressing button, no roundtrip
press = client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `msgPopover` ) ( `toggleBy` ) ( `${$source>/id}` ) ) )
```

The same events also work as a **statement** in your `main` method, with the identical `t_arg` — then the browser runs them after the response arrives, once your backend work is done:

```abap
" after backend processing, advance a wizard step
client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `wiz` ) ( `setNextStep` ) ( `STEP2` ) ) ).
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

The second way to call `follow_up_action( )`: pass a raw JavaScript expression as
`val` (without `t_arg`). It runs as-is in the browser.

```abap
client->follow_up_action( `myFunction()` ).
```

`follow_up_action( )` decides which way applies from the content of `val`: a
plain event name (only `A-Z`, `a-z`, `0-9`, `_`) becomes a frontend event call,
anything containing JavaScript syntax runs verbatim.

::: warning Not Recommended
This is still available, but its use is **strongly discouraged**. Injecting
arbitrary JavaScript from the backend into the frontend introduces serious
security risks. Only use it if you fully understand the consequences and have no
alternative.
:::


### Why It Is a Security Risk

Custom JS works by sending a JavaScript string from the ABAP backend to the frontend, where it is injected into the DOM as an HTML `<script>` tag and executed in the user's browser. This pattern is essentially a **self-inflicted Cross-Site Scripting (XSS) vector** and breaks several security assumptions UI5 normally protects you from:

- **Bypasses output encoding.** UI5 escapes model data by default to prevent XSS. Raw `<script>` injection sidesteps that protection entirely.
- **Executes with full user privileges.** The injected code runs in the same origin as your app and can read cookies, session tokens, the UI5 model, and any data the user has access to — and send it anywhere.
- **Dynamic content is dangerous.** If any part of the injected JavaScript is built from user input, database values, translations, or other non-static sources, an attacker who controls that source can execute arbitrary code in every user's browser.
- **Breaks Content Security Policy (CSP).** A strict CSP — one of the most effective defenses against XSS — typically forbids inline scripts. Custom JS forces you to weaken or disable CSP, removing that protection for the whole app.
- **Hard to audit.** JavaScript assembled in ABAP strings is not covered by frontend linters, static analysis, or code review tools that normally catch dangerous patterns.
- **No sandboxing.** The script has the same DOM and network access as the rest of the app. There is no isolation boundary.

### Safer Alternatives

Before reaching for raw JavaScript, consider:

- Use the **built-in frontend events** above — most browser interactions are already covered.
- Use the **standard UI5 controls and APIs**.
- Build a proper **[Custom Control](/advanced/extensibility/custom_control)** with a defined interface and reviewable frontend code.
- Use the dedicated cookbook pages for [Clipboard](/cookbook/browser_interaction/clipboard), [Focus](/cookbook/browser_interaction/focus), [Scrolling](/cookbook/browser_interaction/scrolling), [Timer](/cookbook/browser_interaction/timer), [URL Handling](/cookbook/browser_interaction/url_handling), and similar.

### How It Works (If You Still Need It)

If you accept the risks and decide to use it anyway, the idea is: send the JavaScript function with the view to the frontend, then call it later when an event fires.

The `_generic` method creates a custom XML/HTML element — here an HTML `<script>` tag (namespace `html`). The `_cc_plain_xml` method inserts raw content into that element — in this case, the JavaScript function definition. On the backend, `follow_up_action( )` then runs the function by name on the frontend.

```abap
  METHOD z2ui5_if_app~main.

  IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`      v = `sap.m`
              )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
              )->a( n = `xmlns:core` v = `sap.ui.core`

              " the script travels as the CONTENT of a core:HTML control - the
              " builder re-escapes it on stringify, so the literal markup is
              " written here
              )->tag( n = `HTML` ns = `core`
                  )->a( n = `content` v = |<script>function myFunction() \{ console.log( `Hello World` ); \}</script>|

              )->ele( `Page`
                  )->tag( `Button`
                      )->a( n = `text`  v = `call custom JS`
                      )->a( n = `press` v = client->_event( `CUSTOM_JS` ) ).

      client->view_display( view->stringify( ) ).

  ENDIF.

  IF client->get( )-event = `CUSTOM_JS`.
      client->follow_up_action( `myFunction()` ).
  ENDIF.

ENDMETHOD.
```

::: danger Never Inject Untrusted Input
If you must use this, ensure the JavaScript content is **entirely static and hardcoded**. Never concatenate user input, database values, translatable texts, or any other dynamic data into the script string — doing so turns the feature into a direct XSS vulnerability.
:::

### Embedding JavaScript Directly in an XML View

::: warning Also Not Recommended
The same security considerations apply: any `<script>` element embedded in an XML view runs with full app privileges and bypasses UI5's output encoding. Prefer a [Custom Control](/advanced/extensibility/custom_control) or one of the built-in events instead.
:::

If you want to look at — or hand-craft — the raw XML view that abap2UI5 produces, a `<script>` tag is placed in the `html` namespace alongside the regular UI5 controls. The view stringified by `z2ui5_cl_ui5_view_builder=>factory( )` ends up looking like this:

```xml
<mvc:View
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    xmlns:html="http://www.w3.org/1999/xhtml">
  <html:script>
    function myFunction() { console.log("Hello World"); }
  </html:script>
  <Page>
    <Button text="call custom JS" press="..." />
  </Page>
</mvc:View>
```

The browser parses the `html:script` element and executes its content as JavaScript at view render time. The function becomes available globally and can then be triggered from the backend via `follow_up_action( )` — or, ideally, replaced entirely with a built-in frontend event (e.g. `SET_TITLE`, `SCROLL_TO`) instead of hand-written JavaScript.

This is exactly what the `_generic` + `_cc_plain_xml` helpers shown above produce; the two approaches are equivalent. Both ship raw JavaScript from the backend to the browser, and both carry the security risks described above. Use neither unless there is genuinely no alternative.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Link with preventDefault (A) | [`Z2UI5_CL_SMP_APP_472`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_472.clas.abap) |
| Element Binding to the Selected Row (A) | [`Z2UI5_CL_SMP_APP_470`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_470.clas.abap) |
| Expand a Panel by ID (setExpanded) (A) | [`Z2UI5_CL_SMP_APP_448`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_448.clas.abap) |
| Switch NavContainer Page by ID (A) | [`Z2UI5_CL_SMP_APP_088`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_088.clas.abap) |
| Wizard with Steps (A) | [`Z2UI5_CL_SMP_APP_202`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_202.clas.abap) |
| Open the PDF Viewer by ID (A) | [`Z2UI5_CL_SMP_APP_449`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_449.clas.abap) |
| Toggle by ID (toggleBy) (A) | [`Z2UI5_CL_SMP_APP_465`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_465.clas.abap) |

<!-- samples:end -->
