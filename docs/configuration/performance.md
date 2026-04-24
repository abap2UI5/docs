---
outline: [2, 4]
---
# Performance

abap2UI5 is fast. Almost all processing happens on the ABAP backend, which runs much faster than the browser.

abap2UI5 keeps frontend logic minimal: no business logic runs in the browser. Everything goes directly to the UI5 framework, which focuses only on UI rendering.

We've tested abap2UI5 with tables containing large numbers of entries and columns, so you can build your app with confidence — performance shouldn't be a concern.

### `view_display` vs. `view_model_update`

The biggest optimization is choosing the right update method:

- **`client->view_display( )`** — sends a new XML view and model to the frontend. UI5 destroys the current view and creates a new one from scratch. Use this only on initialization or when the view structure changes.
- **`client->view_model_update( )`** — sends only updated model data. UI5 refreshes the existing view via data binding, re-rendering only the changed controls. This preserves UI state (scroll position, focus, etc.) and is much faster.

```abap
METHOD z2ui5_if_app~main.

  CASE abap_true.

    WHEN client->check_on_init( ).
      DATA(view) = z2ui5_cl_xml_view=>factory(
        )->page( `My App`
        )->text( client->_bind( mv_text )
        )->button( text = `update` press = client->_event( `UPDATE` ) ).
      client->view_display( view->stringify( ) ).

    WHEN client->check_on_event( `UPDATE` ).
      mv_text = `new value`.
      client->view_model_update( ).

  ENDCASE.

ENDMETHOD.
```

### Suggestions
Want to tune your app further? Here are a few tips:
- Call `client->view_display` only when necessary. Prefer `client->view_model_update` so the UI5 framework only re-renders controls that have changed.
- Prefer `client->_bind`; use `client->_bind_edit` only when users need to make changes the backend processes. Otherwise, you'll trigger unnecessary data transfers.
- Declare public attributes in your app class only for variables displayed on the frontend. This prevents the framework from accessing unused values.
- Follow standard ABAP best practices, like reducing loops and using sorted tables, as you would in any other ABAP project.

### Performance Issues?
If you hit performance issues, create a sample and submit a pull request to the samples repository. We're happy to analyze it and see whether we can make abap2UI5 even faster.
