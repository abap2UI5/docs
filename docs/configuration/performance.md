---
outline: [2, 4]
---

# Performance

abap2UI5 is fast. Almost all processing happens on the ABAP backend, which is significantly faster than client-side or browser-based processing.

Frontend logic is kept to a minimum: no business logic runs in the browser. Everything is passed directly to the UI5 framework, which focuses solely on UI rendering.

abap2UI5 has been successfully tested with tables containing large numbers of entries and columns. So, you can confidently develop your app — performance shouldn't be a concern.

### view_display vs view_model_update

The most impactful optimization is choosing the right update method:

- **`client->view_display( )`** — sends a new XML view and model to the frontend. UI5 destroys the current view and creates a new one from scratch. Use this only on initialization or when the view structure changes.
- **`client->view_model_update( )`** — sends only updated model data. UI5 refreshes the existing view via data binding — only the changed controls are re-rendered. This preserves UI state (scroll position, focus, etc.) and is significantly faster.

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
Want to optimize your app even more? Here are a few tips:
* Only call `client->view_display` when necessary. Prefer `client->view_model_update` so the UI5 framework only re-renders controls that have actually changed
* Prefer `client->_bind` and use `client->_bind_edit` only when users need to make changes that are processed in the backend. Otherwise, it leads to unnecessary data transfers
* Declare public attributes in your app class only for variables displayed in the frontend. This helps prevent the framework from accessing unused values
* Follow standard ABAP best practices, such as reducing loops and using sorted tables, as you would in any other ABAP development project

### Performance Issues?
If you encounter performance issues, try creating a sample and submitting a pull request to the samples repository. We'd be happy to analyze it and see if we can make abap2UI5 even faster.
