---
outline: [2, 4]
---
# Follow-up Action

Sometimes, once your backend event handler has finished, you want to trigger an
action that runs on the frontend — set the browser title, move focus, scroll,
copy to the clipboard, and so on. `client->follow_up_action( )` schedules such a
frontend action; it runs in the browser right after the response arrives.

It is also the method to reach for when an older app calls `_event_client( )` or
wraps a browser interaction in a custom control — both are obsolete, see
[Deprecations](/resources/deprecations).

## Frontend event + arguments

The usual way: pass a built-in frontend event as the first parameter `val` and
its arguments in `t_arg`. The framework assembles the frontend call for you.

```abap
METHOD z2ui5_if_app~main.

    client->follow_up_action(
        val   = client->cs_event-set_title
        t_arg = VALUE #( ( `Invoice 4711` ) ) ).

ENDMETHOD.
```

`val` is one of the frontend events from `z2ui5_if_client=>cs_event` (see
[Frontend](/cookbook/event_navigation/frontend)) — for example `set_title`,
`set_focus`, `scroll_to`, `start_timer`, `download_b64_file`, `play_audio`.
`t_arg` carries the arguments the event expects; events without arguments need
no `t_arg`:

```abap
client->follow_up_action( client->cs_event-popup_close ).
```

See the dedicated cookbook pages under
[Browser Interaction](/cookbook/browser_interaction/title) and
[Device Capabilities](/cookbook/device_capabilities/upload_download) for the
argument list of each event.

::: tip Replacing a custom control
Earlier versions of abap2UI5 needed an invisible custom UI5 control for each of
these interactions — title, focus, scrolling, timer, soft keyboard. Every one of
them is a built-in event now. The full old-control-to-event list is in
[Deprecations](/resources/deprecations#invisible-custom-controls).
:::

## Calling a control method

The control calls — `cs_event-control_by_id`, `control_global`, `binding_call`
and `bind_element` — are frontend events too, so `follow_up_action( )` can invoke a
method on a control once the backend response arrives. For `control_by_id`, any
public control method works unless it is on the framework's denylist;
`control_global` and `binding_call` are strict whitelists. Their `t_arg` is
positional (see [Frontend → Calling control methods](/cookbook/event_navigation/frontend#calling-control-methods-on-the-frontend)):

```abap
" after backend processing, advance a wizard step
client->follow_up_action(
    val   = client->cs_event-control_by_id
    t_arg = VALUE #( ( `wiz` ) ( `setNextStep` ) ( `STEP2` ) ) ).
```

For `control_by_id`, the control is resolved by id. A separate `view` parameter
(default `cs_view-main`, which resolves the id across all open views) scopes the
lookup to a single view — pass `cs_view-popup` / `cs_view-popover` / … for a
control hosted in a popup or popover:

```abap
client->follow_up_action(
    val   = client->cs_event-control_by_id
    view  = client->cs_view-popup
    t_arg = VALUE #( ( `NavCon` ) ( `to` ) ( `detail` ) ) ).
```

See demo apps 470 (element binding) and 471 (keyboard shortcuts) in the [samples repository](https://github.com/abap2UI5/samples) for complete `follow_up_action` examples.

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

::: tip `cs_event-z2ui5` is the older form of the same thing
The constant calls a function you registered as a `z2ui5.*` global
(``follow_up_action( val = cs_event-z2ui5 t_arg = VALUE #( ( `myFunction` ) ) )``).
It still works and is still dispatched, but it sits with the obsolete constants
in `z2ui5_if_client`: passing the expression directly, as above, is the same
call without the indirection. If the global is missing the frontend logs
`Z2UI5: 'z2ui5.myFunction' is not a function` rather than failing silently.
:::

#### Why It Is a Security Risk

Custom JS works by sending a JavaScript string from the ABAP backend to the frontend, where it is injected into the DOM as an HTML `<script>` tag and executed in the user's browser. This pattern is essentially a **self-inflicted Cross-Site Scripting (XSS) vector** and breaks several security assumptions UI5 normally protects you from:

- **Bypasses output encoding.** UI5 escapes model data by default to prevent XSS. Raw `<script>` injection sidesteps that protection entirely.
- **Executes with full user privileges.** The injected code runs in the same origin as your app and can read cookies, session tokens, the UI5 model, and any data the user has access to — and send it anywhere.
- **Dynamic content is dangerous.** If any part of the injected JavaScript is built from user input, database values, translations, or other non-static sources, an attacker who controls that source can execute arbitrary code in every user's browser.
- **Breaks Content Security Policy (CSP).** A strict CSP — one of the most effective defenses against XSS — typically forbids inline scripts. Custom JS forces you to weaken or disable CSP, removing that protection for the whole app.
- **Hard to audit.** JavaScript assembled in ABAP strings is not covered by frontend linters, static analysis, or code review tools that normally catch dangerous patterns.
- **No sandboxing.** The script has the same DOM and network access as the rest of the app. There is no isolation boundary.

#### Safer Alternatives

Before reaching for raw JavaScript, consider:

- Use the **built-in frontend events** above — most browser interactions are already covered.
- Use the **standard UI5 controls and APIs**.
- Build a proper **[Custom Control](/advanced/extensibility/custom_control)** with a defined interface and reviewable frontend code.
- Use the dedicated cookbook pages for [Clipboard](/cookbook/browser_interaction/clipboard), [Focus](/cookbook/browser_interaction/focus), [Scrolling](/cookbook/browser_interaction/scrolling), [Timer](/cookbook/browser_interaction/timer), [URL Handling](/cookbook/browser_interaction/url_handling), and similar.

#### How It Works (If You Still Need It)

If you accept the risks and decide to use it anyway, the idea is: send the JavaScript function with the view to the frontend, then call it later when an event fires.

The `_generic` method creates a custom XML/HTML element — here an HTML `<script>` tag (namespace `html`). The `_cc_plain_xml` method inserts raw content into that element — in this case, the JavaScript function definition. On the backend, `follow_up_action( )` then runs the function by name on the frontend.

```abap
  METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
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

#### Embedding JavaScript Directly in an XML View

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
