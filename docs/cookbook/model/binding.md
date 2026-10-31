---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_494
  - z2ui5_cl_smp_app_047
  - z2ui5_cl_smp_app_166
  - z2ui5_cl_smp_app_144
  - z2ui5_cl_smp_app_061
description: Data binding in abap2UI5 - client->_bind( ) shares ABAP variables with the UI5 view, for displaying and editing data, binding to structures, and the data-type mapping.
---
# Binding

In abap2UI5 you share data between your ABAP code and the UI5 frontend with
`client->_bind( )`. There is only one binding, and it works **in both
directions**: when the value is changed in an editable control, the framework
writes it back to your ABAP attribute before the next event handler runs. Only
the paths the user actually edited are transported back (a delta), so read-only
data costs nothing on the way back.

## Displaying Data
Bind an attribute to a display-only control (e.g. `text`) — nothing is editable
there, so nothing syncs back:

```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      name = `World`.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `abap2UI5 - Hello World`

                  )->tag( `Text`
                      )->a( n = `text` v = `My Text`
                  )->tag( `Text`
                      )->a( n = `text` v = client->_bind( name ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```
This method works with tables, trees, and other nested data structures — see
[Tables](/cookbook/model/tables) and [Trees](/cookbook/model/trees).

## Editing Data
Bind an attribute to an editable control (e.g. `input`). After an event, the
framework has already synced the user's changes back to your ABAP attribute:

```abap
CLASS zcl_app_hello_world DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_app_hello_world IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `abap2UI5 - Hello World`

                  )->tag( `Text`
                      )->a( n = `text` v = `Enter your name`
                  )->tag( `Input`
                      )->a( n = `value` v = client->_bind( name )
                  )->tag( `Button`
                      )->a( n = `text`  v = `post`
                      )->a( n = `press` v = client->_event( `POST` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POST` ).
      client->message_box_display( |Your name is { name }.| ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

The view is built once, in the `check_on_navigated( )` branch; the `POST`
branch only reads `name`, which the framework has already written back. It
does not call `view_display( )` again — the view on screen is still right,
only the data changed. See [Life Cycle](/cookbook/event_navigation/life_cycle)
for why every app is shaped this way.


::: warning **Bound Attributes Must Be Public**
`_bind( )` resolves your class attributes from outside the controller via
dynamic ASSIGN, and it only sees the `PUBLIC SECTION`. Passing a `PROTECTED` or
`PRIVATE` attribute — or a local variable — is **not** silent: the framework
finds no attribute for the reference and raises

```
BINDING_ERROR - No class attribute for binding found -
Please check if the bound values are public attributes of your class
```

which reaches the browser as a 500 and is shown in the error view. The
compiler cannot catch it (the call takes `TYPE data`), but the runtime says
exactly what is wrong.

Always declare bound data in `PUBLIC SECTION`. This resembles the PAI/PBO logic,
where data lived in global variables. See also
[Life Cycle → Lifecycle Pitfalls](/cookbook/event_navigation/life_cycle#lifecycle-pitfalls).
:::

## Binding to Structures

When the bound attribute is a structure, refer to a field directly with the `-`
component selector. The framework generates one model path per field — the
structure name and the component name, in upper case, joined by `/`:

```abap
TYPES: BEGIN OF ts_order,
         customer TYPE string,
         material TYPE string,
         quantity TYPE i,
       END OF ts_order.

DATA edit_row TYPE ts_order.

...

)->tag( `Input`
    )->a( n = `value` v = client->_bind( edit_row-customer )   " resolves to {/EDIT_ROW/CUSTOMER}
)->tag( `Input`
    )->a( n = `value` v = client->_bind( edit_row-material )
)->tag( `Input`
    )->a( n = `value` v = client->_bind( edit_row-quantity )
```

Nested structures follow the same rule recursively (`edit_row-address-city` →
`/EDIT_ROW/ADDRESS/CITY`). Internal tables of structures use one row context per
item — see [Tables](/cookbook/model/tables).

## `_bind( )` Options

`client->_bind( name )` is the whole call in most places. The optional
parameters cover the cases where the default shape of the model is not what
the control needs:

| Parameter | What it does |
|---|---|
| `path = abap_true` | returns the model **path** of the attribute (`/NAME`) instead of the binding (`{/NAME}`) — what a `parts:` array, a `binding_call` filter or sorter and `bind_element` need. `_bind_path( )` is the same thing under a readable name, see below |
| `tab` / `tab_index` | bind **one cell** of an internal table: pass the table as `tab`, the row number (1-based, the ABAP way) as `tab_index`, and the row's component itself as `val` — ``_bind( val = mt_emp[ 1 ]-name tab = mt_emp tab_index = 1 )`` renders `{/MT_EMP/0/NAME}`. The cell is found by **reference**, so a copy of the value is refused with `BINDING_ERROR_TAB_CELL_LEVEL`. For a repeating aggregation bind the table itself and keep the template's fields relative |
| `omit_initial` | leaves **initial** fields out of the serialized model instead of sending them as `""`, `0` or `false`. An ABAP field is never absent, so by default every field reaches the client as an explicit value, which overrides the UI5 property default — and an enum-typed property rejects the empty string outright. Set it when the rows of a bound template fill different subsets of the same properties |
| `omit_initial_paths` | the same omission scoped to the listed fields (upper-case column names). For the case where the blanket flag is too coarse: an `abap_false` that must reach the client is itself initial, so list the numeric and enum columns and leave the booleans |
| `json` | the bound string already **contains JSON** — it is spliced into the model as a node instead of being sent as a quoted string. For a control property that needs an object no typed ABAP value can be, such as a `sap.ui.integration` card manifest whose keys (`sap.app`, `sap.card`) are not valid ABAP field names. Outbound only |
| `switch_default_model` | writes the binding against the named `http` model — where abap2UI5's own data lives once `view_display( switch_default_model_path = … )` has made an OData service the view's default model, see [Smart Controls](/cookbook/expert_more/smart_controls) |

`custom_mapper` and `custom_filter` still compile but are obsolete — see
[Deprecations](/resources/deprecations).

### `_bind_path( )`

`client->_bind_path( val )` is `_bind( val = val path = abap_true )` byte for
byte, delegating to it rather than repeating it. It exists because the two
forms of `_bind( )` read nothing alike: `_bind( t_products )` says what it
does, the path form needs a named `val` and a boolean whose name means nothing
to a reader who does not know the method. It takes one parameter on purpose —
the moment a second one is needed (`tab`, `omit_initial`, `json`), `_bind( )`
is the right call and `path` stays on it.

```abap
)->a( n = `value` v = |\{ path: '{ client->_bind_path( amount ) }', type: 'sap.ui.model.type.Float' \}|
```

## When a Value Does Not Convert

A value travels back as text and is converted into the ABAP type behind the
binding. A string that will not convert — `1,250.00` typed into an `Input`
bound to a packed amount, letters into an integer — is **skipped, not raised
on**: the attribute keeps its old value, the roundtrip goes on, and the browser
still shows what the user typed, because the client model was updated before
the request went out. The skip is traced in `client->get( )-t_model_skipped`,
one row per refused value with the attribute name, the row and field for a
table cell, and the raw text as it arrived. Read it at the top of `main( )`
rather than inside a Save branch, since the delta travels with whatever
roundtrip follows the edit; the list is per roundtrip. Reading it pushes no
model, so the refused text stays on screen until the app writes something.

The errors that *do* stop a roundtrip are the ones about the binding itself:
`BINDING_ERROR` for an attribute `_bind( )` cannot reach, and
`BINDING_ERROR_TAB_CELL_LEVEL` for a cell binding whose row or component does
not exist. Both are listed with their causes in
[Common Failures](/cookbook/troubleshooting/common_failures).

## Data-Type Mapping

ABAP and UI5 do not share a type system. When ABAP values cross to the frontend
they are serialized to JSON and then read by UI5 controls. The table below is
the reference for how each ABAP type travels on the wire and which UI5 binding
it pairs with. For the controls that need an explicit `type:` or formatter, the
linked sections in [Formatter](/cookbook/model/formatter) show the full
binding-string pattern.

| ABAP type            | On the wire        | Typical UI5 use                                        | Notes                                                                |
| -------------------- | ------------------ | ------------------------------------------------------ | -------------------------------------------------------------------- |
| `string`, `c LENGTH n` | JSON string        | `Input`, `Text`                                        | Works without a formatter.                                           |
| `i`, `int8`, `b`, `s` | JSON number       | `Input type="Number"`, `Text`                          | Returned as string from inputs; cast back if you need an integer.    |
| `p LENGTH n DECIMALS m`, `decfloat16`, `decfloat34` | JSON number | `Input`, `Text` + `sap.ui.model.type.Float`/`Currency` | The decimals travel as written (`1234.56`), no rounding through a binary float. Locale formatting needs an explicit type — see [Currency](/cookbook/model/formatter#currency). |
| `f` (binary float)   | JSON number        | `Input`, `Text` + `sap.ui.model.type.Float`            | Binary float — prefer `p` or `decfloat34` for monetary values to avoid rounding drift. |
| `n LENGTH n`         | JSON string of digits | `Input` + `sap.ui.model.odata.type.String` with `isDigitSequence: true` | Without the constraint, leading zeros render literally — see [Digit Sequence](/cookbook/model/formatter#digit-sequence). |
| `d`                  | ISO date string `2024-01-15` (initial: `""`) | `DatePicker` + `sap.ui.model.type.Date`             | Give the type a `yyyy-MM-dd` source pattern for explicit locale or pattern control. See [Date](/cookbook/model/formatter#date). |
| `t`                  | time string `12:30:00` (initial: `""`) | `TimePicker` + `sap.ui.model.type.Time`                | Same pattern as `d`, source pattern `HH:mm:ss` — see [Time](/cookbook/model/formatter#time).    |
| `abap_bool` (`X`/` `) | JSON `true` / `false` | `CheckBox`, `Switch` — bind it directly | The framework maps the boolean ABAP types to a JSON boolean and back, so no expression and no formatter is needed. A flag typed `c LENGTH 1` instead is just a string — see [Boolean](/cookbook/model/formatter#boolean). |
| `timestamp`, `timestampl` | JSON string, ISO in UTC (`2024-01-15T12:30:00Z`) | `DateTimePicker` + `Formatter.DateCreateObject` or a type with a source pattern | The `Z` marks UTC — see [Timestamp](/cookbook/model/formatter#timestamp). |
| `utclong` | JSON string, ISO-like (`2026-08-21T14:00:00Z`) | `DateTimePicker` + `Formatter.DateCreateObject` | Like `timestamp`, it arrives in a shape `new Date( )` parses — see [The formatters abap2UI5 ships](/cookbook/model/formatter#the-formatters-abap2ui5-ships). |
| `xstring`            | binary — must be base64-encoded in ABAP before binding | `Image`, `FileUploader`, `pdf_viewer`                  | The framework does not auto-encode. Convert with `cl_web_http_utility=>encode_x_base64( )` (or `cl_http_utility=>if_http_utility~encode_x_base64( )` on older releases) — see [PDF](/cookbook/device_capabilities/pdf) and [Upload / Download](/cookbook/device_capabilities/upload_download). |
| structure            | JSON object         | Bind individual fields with `struct-field`             | One model path per field — see [Binding to Structures](#binding-to-structures). |
| internal table       | JSON array          | `Table`, `List`, `Tree`                                | One row context per item — see [Tables](/cookbook/model/tables) and [Trees](/cookbook/model/trees). |

When a value looks wrong, the fix is almost always a UI5-side `type` (e.g.
`sap.ui.model.type.Date`, `sap.ui.model.type.Float`) or an abap2UI5
[Formatter](/cookbook/model/formatter). The shape is always the same — build a
JSON binding string with `parts` and `type`, using `_bind_path( )` to inject the
raw model path:

```abap
)->tag( `Input`
    )->a( n = `value` v = |\{ parts: [ '{ client->_bind_path( amount ) }',|
                       && | '{ client->_bind_path( currency ) }' ],|
                       && | type: 'sap.ui.model.type.Currency' \}|
```

See [Formatter](/cookbook/model/formatter) for the full example with
`formatOptions`, `constraints`, and read-only display variants.

## Validation with `valueState`

A UI5 input shows a check result through two properties: `valueState`
(`None`, `Error`, `Warning`, `Success`, `Information`) and `valueStateText`,
the message under the field. Both are ordinary properties, so the check runs in
ABAP and the result is bound like any other value — no second view, no popup:

```abap
CLASS zcl_sample_validation DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_quantity   TYPE i.
    DATA mv_state      TYPE string VALUE `None`.
    DATA mv_state_text TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_validation IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Validation`

                  )->tag( `Label`
                      )->a( n = `text` v = `Quantity (1 to 100)`
                  )->tag( `Input`
                      )->a( n = `value`          v = client->_bind( mv_quantity )
                      )->a( n = `valueState`     v = client->_bind( mv_state )
                      )->a( n = `valueStateText` v = client->_bind( mv_state_text )
                  )->tag( `Button`
                      )->a( n = `text`  v = `Save`
                      )->a( n = `press` v = client->_event( `SAVE` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SAVE` ).

      IF mv_quantity < 1 OR mv_quantity > 100.
        mv_state      = `Error`.
        mv_state_text = `Enter a quantity between 1 and 100`.
      ELSE.
        mv_state      = `None`.
        mv_state_text = ``.
        client->message_toast_display( `saved` ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

The `SAVE` branch changes two attributes and returns; the framework pushes the
changed values into the view that is already on screen, and the field turns
red with its text underneath. Clearing the state on success is the app's job
too — a state that is never reset stays red after the user has fixed the
value. For the case where the value did not even reach the attribute, see
[When a Value Does Not Convert](#when-a-value-does-not-convert) above: a row
of `t_model_skipped` is the input to the same two attributes.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basics II — Data Binding: Input and Button | [`Z2UI5_CL_SMP_APP_494`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_494.clas.abap) |
| Types for Integer, Decimal, Date and Time | [`Z2UI5_CL_SMP_APP_047`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_047.clas.abap) |
| Structure Fields and INCLUDEs | [`Z2UI5_CL_SMP_APP_166`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_166.clas.abap) |
| Single Table Cell (tab_index) | [`Z2UI5_CL_SMP_APP_144`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_144.clas.abap) |
| Dynamic Table Typed at Runtime (RTTI) | [`Z2UI5_CL_SMP_APP_061`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_061.clas.abap) |

<!-- samples:end -->
