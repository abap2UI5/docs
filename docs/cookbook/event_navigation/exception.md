---
outline: [2, 4]
---
# Exception

Beyond plain messages, abap2UI5 ships dedicated popups and fallbacks for handling exceptions and unexpected failures.

#### Error Popup
To display full details of your exception:
```abap
METHOD z2ui5_if_app~main.

  TRY.
    DATA(lv_val) = 1 / 0.
  CATCH cx_root INTO DATA(lx).
    client->nav_app_call( z2ui5_cl_pop_error=>factory( lx ) ).
  ENDTRY.

ENDMETHOD.
```

#### Uncaught Errors
When your code doesn't catch exceptions, the framework catches them and displays the standard error popup. Try this:

```abap
METHOD z2ui5_if_app~main.

    RAISE EXCEPTION NEW cx_sy_itab_line_not_found( ).

ENDMETHOD.
```

#### Uncatchable Exceptions / Short Dumps
What happens if your code raises uncatchable exceptions? The default HTTP handler exception output appears. Processing halts, and the user has to reload the browser. Reserve this for unexpected cases:

```abap
METHOD z2ui5_if_app~main.

    ASSERT 1 = `This is an error message!`.

ENDMETHOD.
```

## Common Failure Modes

Not every problem raises an ABAP exception. Many failures surface only in the browser, or fail silently. The sections below describe what to look for in three frequent cases.

#### Binding-Path Mismatch

When a `_bind` / `_bind_edit` path does not resolve against the JSON model on the frontend — typically because the public attribute was renamed, the data was never sent, or the path is mistyped — UI5 does **not** raise an ABAP exception. The control simply renders empty or with a default value.

Where to look:
- **Browser console.** UI5 logs a warning like `Binding "/path/to/field" was not found in model` from `sap.ui.model.json.JSONModel`. Open the browser DevTools console and filter by `sap.ui.model`.
- **Network tab.** Inspect the abap2UI5 response payload — the JSON model is included verbatim. If the field is absent or named differently than your binding path, you have your answer.
- **ABAP side.** Nothing. The backend never learns the binding failed. Asserting "no console warnings" is the only way to catch this in tests.

Two-way binding (`_bind_edit`) silently drops the write-back when the path is invalid — your ABAP attribute keeps its old value after the next event. Cross-check the attribute value in the debugger if data is mysteriously not updating.

#### Malformed XML

`Z2UI5_CL_XML_VIEW` produces XML; UI5 parses it on the frontend. A typo in a control name, an unclosed tag, or an aggregation that contains an invalid child can break parsing entirely.

Where the error surfaces depends on what went wrong:
- **Pure XML syntax errors** (unclosed tag, bad escape) — the XML parser fails and UI5 logs a `Parse error` in the browser console. The page renders blank or up to the broken element.
- **Unknown UI5 controls / namespaces** — UI5 logs `failed to load 'sap.m.NotAControl'` (or similar) in the console; the surrounding view may render partially.
- **Wrong aggregation / wrong child type** — see the warning on the [View Definition](/cookbook/view/definition) page. UI5 may log an `aggregation … does not contain` warning or silently drop the child. Layouts can render in unexpected ways without any error.
- **ABAP side** — none of these surface as ABAP exceptions. `view_display( )` accepts any string. The response goes out, and only the browser notices.

When something looks wrong on screen, **always check the browser console first** before re-reading the ABAP code.

For EML-specific failure handling (`FAILED` / `REPORTED`, transactional behavior, `cx_abap_behv`, `cx_abap_lock_failure`, defensive `TRY/CATCH` patterns), see the [EML](/cookbook/eml_cds_sql/eml) page.
