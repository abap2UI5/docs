---
outline: [2, 4]
---
# Binding

In abap2UI5, there are two ways to share data between your ABAP code and the UI5 frontend.

#### One-Way Binding
Use one-way binding to show data on the frontend without allowing edits. The `client->_bind` method sends data to the frontend and binds it to the view:

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( `abap2UI5 - Hello World`
          )->text( `My Text`
          )->text( client->_bind( name )
      )->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
This method works with tables, trees, and other nested data structures. For details, see the table and tree samples in the samples repository.

#### Two-Way Binding
When users need to edit data, use two-way binding to keep it in sync with the ABAP backend. Call the `client->_bind_edit` method — after an event, the framework syncs the data back to your ABAP class:

```abap
CLASS z2ui5_cl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
      )->page( `abap2UI5 - Hello World`
          )->text( `Enter your name`
          )->input( client->_bind_edit( name )
          )->button( text = `post` press = client->_event( `POST` )
      )->stringify( ) ).

    CASE client->get( )-event.
      WHEN `POST`.
        client->message_box_display( |Your name is { name }.| ).
        RETURN.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

::: warning **Bound Attributes Must Be Public**
`_bind( )` and `_bind_edit( )` access your class attributes from outside the controller via dynamic ASSIGN. This only works for attributes in the `PUBLIC SECTION` — `PROTECTED` and `PRIVATE` attributes are not visible to the framework and silently fail to bind: the view renders empty for one-way binding, and edits never sync back for two-way binding. There is no compile-time or runtime error.

Always declare bound data in `PUBLIC SECTION`. This resembles the PAI/PBO logic, where data lived in global variables. See also [Life Cycle → Lifecycle Pitfalls](/cookbook/event_navigation/life_cycle#lifecycle-pitfalls).
:::

#### Known Limitations

::: warning No Documented Data-Type Mapping
ABAP and UI5 do not share a type system. When ABAP values cross to the frontend they are serialized to JSON and then read by UI5 controls — and the exact coercion rules into the inputs UI5 expects (`string`, `number`, `Date`, …) are **not formally documented**. The table below summarizes the behavior most apps rely on; treat it as a starting point and verify in the browser.

| ABAP type            | On the wire        | Typical UI5 use                                        | Notes                                                                |
| -------------------- | ------------------ | ------------------------------------------------------ | -------------------------------------------------------------------- |
| `string`, `c LENGTH n` | JSON string        | `Input`, `Text`                                        | Works without a formatter.                                           |
| `i`                  | JSON number        | `Input type="Number"`, `Text`                          | Returned as string from inputs; cast back if you need an integer.    |
| `p LENGTH n DECIMALS m` | JSON string       | `Input`, `Text` + `sap.ui.model.type.Float`/`Currency` | Locale formatting needs an explicit type — see [Formatter](/cookbook/model/formatter). |
| `n LENGTH n`         | JSON string of digits | `Input` + `sap.ui.model.odata.type.String` with `isDigitSequence: true` | Without the constraint, leading zeros render literally.              |
| `d`                  | 8-char string `YYYYMMDD` | `DatePicker` + `sap.ui.model.type.Date` with `pattern: 'yyyyMMdd', source: { pattern: 'yyyyMMdd' }` | Not an ISO date — a formatter is required for parsing and display.   |
| `t`                  | 6-char string `HHMMSS` | `TimePicker` + `sap.ui.model.type.Time`                | Same pattern as `d`.                                                 |
| `abap_bool` (`X`/` `) | JSON string `"X"` / `""` | Compare with `"X"` in expression binding, or convert to `abap_true`/`abap_false` in a formatter | UI5's `CheckBox` expects `true`/`false`, not `"X"` — adapt explicitly. |
| `timestamp`, `timestampl` | JSON number (packed) | `DateTimePicker` + custom formatter                   | No built-in UI5 type matches; convert in ABAP or write a JS formatter. |

When a value looks wrong, the fix is almost always a UI5-side `type` (e.g. `sap.ui.model.type.Date`, `sap.ui.model.type.Float`) or an abap2UI5 [Formatter](/cookbook/model/formatter). Verify behavior in the browser with the actual control rather than assuming the default coercion is correct.
:::
