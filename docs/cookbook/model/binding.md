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

#### Binding to Structures

When the bound attribute is a structure, refer to a field directly with the `-` component selector. The framework generates one model path per field — the structure name and the component name, in upper case, joined by `/`:

```abap
TYPES: BEGIN OF ts_order,
         customer TYPE string,
         material TYPE string,
         quantity TYPE i,
       END OF ts_order.

DATA edit_row TYPE ts_order.

...

)->input( client->_bind_edit( edit_row-customer ) )  " resolves to {/XX/EDIT_ROW/CUSTOMER}
)->input( client->_bind_edit( edit_row-material ) )
)->input( client->_bind_edit( edit_row-quantity ) )
```

Nested structures follow the same rule recursively (`edit_row-address-city` → `/XX/EDIT_ROW/ADDRESS/CITY`). Internal tables of structures use one row context per item — see [Tables](/cookbook/model/tables).

#### Data-Type Mapping

ABAP and UI5 do not share a type system. When ABAP values cross to the frontend they are serialized to JSON and then read by UI5 controls. The table below is the reference for how each ABAP type travels on the wire and which UI5 binding it pairs with. For the controls that need an explicit `type:` or formatter, the linked sections in [Formatter](/cookbook/model/formatter) show the full binding-string pattern.

| ABAP type            | On the wire        | Typical UI5 use                                        | Notes                                                                |
| -------------------- | ------------------ | ------------------------------------------------------ | -------------------------------------------------------------------- |
| `string`, `c LENGTH n` | JSON string        | `Input`, `Text`                                        | Works without a formatter.                                           |
| `i`, `int8`, `b`, `s` | JSON number       | `Input type="Number"`, `Text`                          | Returned as string from inputs; cast back if you need an integer.    |
| `p LENGTH n DECIMALS m`, `decfloat16`, `decfloat34` | JSON string | `Input`, `Text` + `sap.ui.model.type.Float`/`Currency` | Sent as a string to preserve precision. Locale formatting needs an explicit type — see [Currency](/cookbook/model/formatter#currency). |
| `f` (binary float)   | JSON number        | `Input`, `Text` + `sap.ui.model.type.Float`            | Binary float — prefer `p` or `decfloat34` for monetary values to avoid rounding drift. |
| `n LENGTH n`         | JSON string of digits | `Input` + `sap.ui.model.odata.type.String` with `isDigitSequence: true` | Without the constraint, leading zeros render literally — see [Digit Sequence](/cookbook/model/formatter#digit-sequence). |
| `d`                  | 8-char string `YYYYMMDD` | `DatePicker` + `sap.ui.model.type.Date`             | Not an ISO date — a formatter is required for explicit locale or pattern control. See [Date](/cookbook/model/formatter#date). |
| `t`                  | 6-char string `HHMMSS` | `TimePicker` + `sap.ui.model.type.Time`                | Same pattern as `d` — see [Time](/cookbook/model/formatter#time).    |
| `abap_bool` (`X`/` `) | JSON string `"X"` / `""` | `CheckBox` with expression binding or ABAP-side conversion | UI5's `CheckBox` expects `true`/`false`, not `"X"` — see [Boolean](/cookbook/model/formatter#boolean). |
| `timestamp`, `timestampl`, `utclong` | JSON string (packed digits for `timestamp`/`timestampl`; ISO-like for `utclong`) | `DateTimePicker` + ABAP-side conversion or custom formatter | No built-in UI5 type reads them directly. Split into `d` + `t` or convert to a `yyyyMMddHHmmss` string — see [Timestamp](/cookbook/model/formatter#timestamp). |
| `xstring`            | binary — must be base64-encoded in ABAP before binding | `Image`, `FileUploader`, `pdf_viewer`                  | The framework does not auto-encode. Convert with `cl_web_http_utility=>encode_x_base64( )` (or `cl_http_utility=>if_http_utility~encode_x_base64( )` on older releases) — see [PDF](/cookbook/device_capabilities/pdf) and [Upload / Download](/cookbook/device_capabilities/upload_download). |
| structure            | JSON object         | Bind individual fields with `struct-field`             | One model path per field — see [Binding to Structures](#binding-to-structures). |
| internal table       | JSON array          | `Table`, `List`, `Tree`                                | One row context per item — see [Tables](/cookbook/model/tables) and [Trees](/cookbook/model/trees). |

When a value looks wrong, the fix is almost always a UI5-side `type` (e.g. `sap.ui.model.type.Date`, `sap.ui.model.type.Float`) or an abap2UI5 [Formatter](/cookbook/model/formatter). The shape is always the same — build a JSON binding string with `parts` and `type`, using `path = abap_true` on `_bind_edit` to inject the raw model path:

```abap
)->input(
    |\{ parts: [ `{ client->_bind_edit( val = amount   path = abap_true ) }`,
                 `{ client->_bind_edit( val = currency path = abap_true ) }` ],
        type: 'sap.ui.model.type.Currency' \}| )
```

See [Formatter](/cookbook/model/formatter) for the full example with `formatOptions`, `constraints`, and read-only display variants.
