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
What happens if your code raises uncatchable exceptions? The backend dumps, and the frontend shows its unified **fatal-error overlay**: a dialog that extracts and displays the server's error details, with a **Details** view, a **Copy** button for the error text and **Refresh / Logout** actions to restart. Processing halts until the user restarts the app from that dialog. Reserve this for unexpected cases:

```abap
METHOD z2ui5_if_app~main.

    ASSERT 1 = 2. " always fails → short dump

ENDMETHOD.
```

For non-exception failures (binding-path mismatches, malformed XML, and other silent frontend issues), see [Common Failures](/cookbook/troubleshooting/common_failures).

For EML-specific failure handling (`FAILED` / `REPORTED`, transactional behavior, `cx_abap_behv`, `cx_abap_lock_failure`, defensive `TRY/CATCH` patterns), see the [EML](/cookbook/eml_cds_sql/eml) page.
