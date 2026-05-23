---
outline: [2, 4]
---
# Custom JS

::: warning Not Recommended
This functionality is still available, but its use is **strongly discouraged**. Injecting arbitrary JavaScript from the backend into the frontend introduces serious security risks. Only use it if you fully understand the consequences and have no alternative.
:::

## Why It Is a Security Risk

Custom JS works by sending a JavaScript string from the ABAP backend to the frontend, where it is injected into the DOM as an HTML `<script>` tag and executed in the user's browser. This pattern is essentially a **self-inflicted Cross-Site Scripting (XSS) vector** and breaks several security assumptions UI5 normally protects you from:

- **Bypasses output encoding.** UI5 escapes model data by default to prevent XSS. Raw `<script>` injection sidesteps that protection entirely.
- **Executes with full user privileges.** The injected code runs in the same origin as your app and can read cookies, session tokens, the UI5 model, and any data the user has access to — and send it anywhere.
- **Dynamic content is dangerous.** If any part of the injected JavaScript is built from user input, database values, translations, or other non-static sources, an attacker who controls that source can execute arbitrary code in every user's browser.
- **Breaks Content Security Policy (CSP).** A strict CSP — one of the most effective defenses against XSS — typically forbids inline scripts. Custom JS forces you to weaken or disable CSP, removing that protection for the whole app.
- **Hard to audit.** JavaScript assembled in ABAP strings is not covered by frontend linters, static analysis, or code review tools that normally catch dangerous patterns.
- **No sandboxing.** The script has the same DOM and network access as the rest of the app. There is no isolation boundary.

## Safer Alternatives

Before reaching for Custom JS, consider:

- Use the **standard UI5 controls and APIs** — most browser interactions are already covered.
- Build a proper **[Custom Control](/advanced/extensibility/custom_control)** with a defined interface and reviewable frontend code.
- Use the dedicated cookbook pages for [Clipboard](/cookbook/browser_interaction/clipboard), [Focus](/cookbook/browser_interaction/focus), [Scrolling](/cookbook/browser_interaction/scrolling), [Timer](/cookbook/browser_interaction/timer), [URL Handling](/cookbook/browser_interaction/url_handling), and similar.

## How It Works (If You Still Need It)

If you accept the risks and decide to use it anyway, the idea is: send the JavaScript function with the view to the frontend, then call it later when an event fires.

The `_generic` method creates a custom XML/HTML element — here an HTML `<script>` tag (namespace `html`). The `_cc_plain_xml` method inserts raw content into that element — in this case, the JavaScript function definition. On the backend, `client->follow_up_action` then runs the function by name on the frontend:

```abap
  METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).
      DATA(view) = z2ui5_cl_xml_view=>factory( ).
      view->_generic( name = `script` ns = `html`
        )->_cc_plain_xml(
          |function myFunction() \{ console.log( `Hello World` ); \}|
        ).
      view->page(
        )->button( text  = `call custom JS`
                   press = client->_event( `CUSTOM_JS` ) ).
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
