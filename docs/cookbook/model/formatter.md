---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_067
  - z2ui5_cl_smp_app_450
  - z2ui5_cl_smp_app_457
  - z2ui5_cl_smp_app_456
  - z2ui5_cl_smp_app_466
  - z2ui5_cl_smp_app_453
---
# Formatter

You can format values like currencies, numerics, dates, or booleans directly on the frontend with UI5 type formatters.

UI5 formatter types use a special JSON-based binding syntax with these key elements:
- **`parts: [...]`** — lists the model paths used as input (e.g., amount + currency)
- **`type: '...'`** — the UI5 formatter type (e.g., `sap.ui.model.type.Currency`)
- **`formatOptions: {...}`** — optional settings that control the output format
- **`constraints: {...}`** — optional constraints applied when user input is parsed back

Note on ABAP syntax: inside string templates (`|...|`), escape the curly braces as `\{` and `\}`, because `{ }` normally denotes an embedded ABAP expression.

The `path = abap_true` parameter on `_bind` returns only the raw model path rather than the full binding expression, so you can embed it inside the `parts` array or a single-path `path:` entry. The path is e.g. `/AMOUNT`, **unquoted** — so the quotes around it are yours to write, and they have to be `'` or `"`. A backtick is an ABAP string delimiter, not a UI5 one: written here it survives into the binding string, and UI5 answers with a syntax error and renders nothing at all.

For example, this ABAP code:
```abap
|\{ parts: ['{ client->_bind( val = amount path = abap_true ) }'], type: 'sap.ui.model.type.Currency' \}|
```
produces this UI5 binding string at runtime:
```text
{ parts: ['/AMOUNT'], type: 'sap.ui.model.type.Currency' }
```

The sections below show the binding-string pattern for each ABAP type that needs a formatter. Each pattern is the minimum that makes the value display and parse correctly — for runnable apps with full `formatOptions`, `constraints`, and read-only variants, see the [samples repository](https://github.com/abap2UI5/samples).

## Currency

ABAP `p LENGTH n DECIMALS m` + a `c LENGTH 3` currency code (a plain `string` also works, as in the worked example below) → UI5 `Currency` formatter. Two `parts` entries; the type combines them into a locale-aware amount string:

```abap
)->tag( `Input`
    )->a( n = `value` v = |\{ parts: [ '{ client->_bind( val = amount   path = abap_true ) }',
                                       '{ client->_bind( val = currency path = abap_true ) }' ],
                              type: 'sap.ui.model.type.Currency' \}|
```

Common `formatOptions`:
- `showMeasure: false` — hides the currency symbol
- `showNumber: false` — hides the amount, shows only the symbol
- `preserveDecimals: false` — trims trailing zeros
- `currencyCode: false` — hides the ISO code
- `style: 'short'` / `'long'` — compact (`123M`) or full-text (`123 million US dollars`) notation

The [Full Worked Example](#full-worked-example) below demonstrates each of these variants in a single app.

## Digit Sequence

ABAP `n LENGTH n` is sent as a digit string, leading zeros included. Without a type the zeros render literally. Use the OData `String` type with `isDigitSequence: true`:

```abap
)->tag( `Text`
    )->a( n = `text` v = |\{ path: '{ client->_bind( val = numeric path = abap_true ) }',
                             type: 'sap.ui.model.odata.type.String',
                             constraints: \{ isDigitSequence: true \} \}|
```

This strips the leading zeros for display and re-pads them on write-back.

## Date

ABAP `d` is an 8-character string `YYYYMMDD`. `DatePicker` accepts it directly via `client->_bind( mv_date )` for the default case. For explicit locale or pattern control, use `sap.ui.model.type.Date` with a `source` pattern that matches the wire format:

```abap
)->tag( `DatePicker`
    )->a( n = `value` v = |\{ path: '{ client->_bind( val = mv_date path = abap_true ) }',
                              type: 'sap.ui.model.type.Date',
                              formatOptions: \{ pattern: 'yyyy-MM-dd',
                                                source: \{ pattern: 'yyyyMMdd' \} \} \}|
```

`source.pattern` is the wire format (ABAP side); the outer `pattern` is what the user sees.

## Time

ABAP `t` is a 6-character string `HHMMSS`. Same pattern as Date, with `sap.ui.model.type.Time`:

```abap
)->tag( `TimePicker`
    )->a( n = `value` v = |\{ path: '{ client->_bind( val = mv_time path = abap_true ) }',
                              type: 'sap.ui.model.type.Time',
                              formatOptions: \{ pattern: 'HH:mm:ss',
                                                source: \{ pattern: 'HHmmss' \} \} \}|
```

## Boolean

ABAP `abap_bool` is `"X"` or `""`. UI5's `CheckBox` expects `true` / `false`. Two practical bridges:

**Expression binding** — compare the bound value to `'X'` inline. Read-only:
```abap
)->tag( `CheckBox`
    )->a( n = `selected` v = `{= $` && client->_bind( mv_flag ) && ` === 'X' }`
```
This resolves to `{= ${/MV_FLAG} === 'X' }`. Note that expression bindings cannot write back — checking the box will not flip the ABAP attribute.

**ABAP-side conversion** — keep a parallel `string`-typed attribute (`'true'` / `'false'`) to bind against, and translate before/after each event:
```abap
DATA flag_bool TYPE abap_bool.
DATA flag_str  TYPE string.   " 'true' / 'false' for the checkbox

" before view_display:
flag_str = COND #( WHEN flag_bool = abap_true THEN 'true' ELSE 'false' ).

" after the event:
flag_bool = COND #( WHEN flag_str = 'true' THEN abap_true ELSE abap_false ).
```
Then a `CheckBox` whose `selected` attribute is `client->_bind( flag_str )` works both directions. More boilerplate in the controller, simpler view.

A custom JS formatter wired through `sap.ui.model.SimpleType` is the third option — see the [samples repository](https://github.com/abap2UI5/samples).

## Timestamp

`timestamp` and `timestampl` are packed numbers with no built-in UI5 type that reads them directly. Two practical approaches:

**Split in ABAP** — break the timestamp into separate `d` and `t` fields before sending, bind each with the [Date](#date) / [Time](#time) formatter above, recombine after the event. Simplest when the UI shows date and time as separate fields anyway.

**Send as string with a source pattern** — convert to a string in `yyyyMMddHHmmss` format on the ABAP side, then bind with `sap.ui.model.type.DateTime`:
```abap
)->tag( `DateTimePicker`
    )->a( n = `value` v = |\{ path: '{ client->_bind( val = mv_ts_string path = abap_true ) }',
                              type: 'sap.ui.model.type.DateTime',
                              formatOptions: \{ pattern: 'yyyy-MM-dd HH:mm:ss',
                                                source: \{ pattern: 'yyyyMMddHHmmss' \} \} \}|
```
Conversion happens in ABAP (`WRITE timestamp TO ts_string …` or a helper); the framework moves the string verbatim.

A custom JS formatter is the third option when neither fits.

## Full Worked Example

The class below combines the Currency and Digit Sequence patterns in one app and demonstrates every `formatOptions` variant listed under [Currency](#currency):

```abap

CLASS z2ui5_cl_smp_app_067 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA amount            TYPE p LENGTH 14 DECIMALS 3.
    DATA currency          TYPE string.
    DATA numeric           TYPE n LENGTH 12.
    DATA check_initialized TYPE abap_bool.

ENDCLASS.

CLASS z2ui5_cl_smp_app_067 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      numeric = `000000000012`.
      amount = `123456789.123`.
      currency = `USD`.

    ENDIF.

    CASE client->get( )-event.
      WHEN |BACK|.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
      WHEN |BUTTON|.
        " the roundtrip is the point of this button: the edited values travel
        " to the server and come back, and every formatter above renders them
        " again from the model
        client->message_toast_display( |Amount { amount }, currency { currency }| ).
    ENDCASE.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `abap2UI5 - Currency Format`
                    )->a( n = `navButtonPress` v = client->_event( |BACK| )
                    )->a( n = `showNavButton`  b = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Currency`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Title`
                                )->a( n = `text` v = `Input`
                            )->tag( `Label`
                                )->a( n = `text` v = `Documentation`
                            )->tag( `Link`
                                )->a( n = `text` v = `https://sdk.openui5.org/api/sap.ui.model.type.Currency`
                                )->a( n = `href` v = `https://sdk.openui5.org/api/sap.ui.model.type.Currency`

                            )->tag( `Label`
                                )->a( n = `text` v = `One field`
                            )->tag( `Input`
                                )->a( n = `value` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency' \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `Two fields`
                            )->tag( `Input`
                                )->a( n = `value` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency', formatOptions: \{showMeasure: false\} \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `Two fields`
                            )->tag( `Input`
                                )->a( n = `value` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency', formatOptions: \{showNumber: false\} \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `Default`
                            )->tag( `Text`
                                )->a( n = `text` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency' \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `preserveDecimals:false`
                            )->tag( `Text`
                                )->a( n = `text` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency', formatOptions: \{ preserveDecimals : false \} \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `currencyCode:false`
                            )->tag( `Text`
                                )->a( n = `text` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency', formatOptions: \{ currencyCode : false \} \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `style:'short'`
                            )->tag( `Text`
                                )->a( n = `text` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency', formatOptions: \{ style : 'short' \} \}|
                            )->tag( `Label`
                                )->a( n = `text` v = `style:'long'`
                            )->tag( `Text`
                                )->a( n = `text` v = |\{ parts: [ '{ client->_bind( val  = amount
                                                                             path = abap_true ) }', '{ client->_bind( val  = currency
                                                                                                                      path = abap_true ) }'], type: 'sap.ui.model.type.Currency', formatOptions: \{   style : 'long' \} \}|

                            )->tag( `Label`
                                )->a( n = `text` v = `event`
                            )->tag( `Button`
                                )->a( n = `text`  v = `send`
                                )->a( n = `press` v = client->_event( `BUTTON` )

                    )->end(
                    )->end(

                    " Remove leading zeros from a numeric string with OData type formatting.
                    " isDigitSequence: true tells the formatter to treat the value as a digit
                    " sequence — resolves to: { path: "/NUMERIC",
                    " type: 'sap.ui.model.odata.type.String',
                    " constraints: { isDigitSequence: true } }
                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `No Zeros`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Title`
                                )->a( n = `text` v = `Input`
                            )->tag( `Label`
                                )->a( n = `text` v = `Documentation`
                            )->tag( `Link`
                                )->a( n = `text` v = `https://sdk.openui5.org/api/sap.ui.model.odata.type.String%23methods/formatValue`
                                )->a( n = `href` v = `https://sdk.openui5.org/api/sap.ui.model.odata.type.String%23methods/formatValue`
                            )->tag( `Label`
                                )->a( n = `text` v = `Numeric`
                            )->tag( `Input`
                                )->a( n = `value` v = client->_bind( val = numeric )
                            )->tag( `Label`
                                )->a( n = `text` v = `Without leading Zeros`
                            )->tag( `Text`
                                )->a( n = `text` v = |\{ path : '{ client->_bind( val  = numeric
                                                                                  path = abap_true ) }', type : 'sap.ui.model.odata.type.String', constraints : \{ isDigitSequence : true \} \}| ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.

```

For a full runnable copy, see the sample implementation in class `Z2UI5_CL_SMP_APP_067` in the [samples repository](https://github.com/abap2UI5/samples).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Currency Amounts (sap.ui.model.type.Currency) | [`Z2UI5_CL_SMP_APP_067`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_067.clas.abap) |
| ABAP Date and Time Strings (DATS/TIMS) | [`Z2UI5_CL_SMP_APP_450`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_450.clas.abap) |
| Date Object for the DatePicker | [`Z2UI5_CL_SMP_APP_457`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_457.clas.abap) |
| Date Objects for the PlanningCalendar | [`Z2UI5_CL_SMP_APP_456`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_456.clas.abap) |
| Inline Icons in a Text | [`Z2UI5_CL_SMP_APP_466`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_466.clas.abap) |
| When Not to Use One: Compute in ABAP | [`Z2UI5_CL_SMP_APP_453`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_453.clas.abap) |

<!-- samples:end -->
