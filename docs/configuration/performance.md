---
outline: [2, 4]
---
# Performance

abap2UI5 is fast. Almost all processing runs on the ABAP backend, which is much faster than the browser.

abap2UI5 keeps frontend logic minimal: no business logic runs in the browser. Everything goes straight to the UI5 framework, which focuses only on UI rendering.

We've tested abap2UI5 with tables holding large numbers of entries and columns, so you can build your app with confidence — performance shouldn't be a concern.

## Call `view_display( )` once

The biggest optimization is **not** rebuilding the view on every event:

- **`client->view_display( )`** — sends a new XML view and model to the frontend. UI5 destroys the current view and creates a new one from scratch. Use this on initialization, and when the view *structure* changes.
- **Change your bound attribute and return.** The framework compares the model before and after the roundtrip and pushes what changed by itself. UI5 refreshes the existing view via data binding, re-rendering only the affected controls, and the UI keeps its state — scroll position, focus, everything.

```abap
METHOD z2ui5_if_app~main.

  CASE abap_true.

    WHEN client->check_on_init( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `My App`

                  )->tag( `Text`
                      )->a( n = `text` v = client->_bind( mv_text )
                  )->tag( `Button`
                      )->a( n = `text`  v = `update`
                      )->a( n = `press` v = client->_event( `UPDATE` ) ).

      client->view_display( view->stringify( ) ).

    WHEN client->check_on_event( `UPDATE` ).
      " no view_display( ), and nothing else either - the changed value is
      " pushed with the response
      mv_text = `new value`.

  ENDCASE.

ENDMETHOD.
```

::: tip You may see `client->view_model_update( )` in older code
It used to be the way to ask for that push. It is a **no-op** now — the
framework does it unconditionally — and it is scheduled for removal. Delete
the call; nothing replaces it.
:::


## Suggestions
Want to tune your app further? A few tips:
- Call `client->view_display` only when needed — on initialization and when the view structure changes. For a pure data change, set the attribute and return; the framework pushes the delta and UI5 re-renders only the controls that changed.

- Bind data with `client->_bind` — the framework sends only the paths the user actually edited back to ABAP (a delta), so read-only and untouched fields cost nothing on the return trip. (`_bind_edit` is an obsolete alias of `_bind`.)
- Declare public attributes in your app class only for variables shown on the frontend. This keeps the framework from reading unused values.
- Follow standard ABAP best practices, like cutting loops and choosing sorted tables, just like in any other ABAP project.

## Performance Issues?
If you hit performance issues, build a sample and submit a pull request to the samples repository. We're glad to analyze it and see whether abap2UI5 can be made even faster.
