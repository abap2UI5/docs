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

**A boolean needs no formatter.** `abap_bool` is `"X"` or `""` in ABAP, but it
does not travel that way: the framework serializes the boolean ABAP types to a
JSON `true` / `false` and converts back on the way in. So bind the attribute
straight to the control and both directions work:

```abap
)->tag( `CheckBox`
    )->a( n = `selected` v = client->_bind( mv_flag ) )
```

Inside a table row template, the same thing relative to the row:

```abap
)->tag( `CheckBox`
    )->a( n = `selected` v = `{CHECKBOX}` )
```

::: warning It is the TYPE that decides, not the value
The mapping is keyed on the ABAP type: `abap_bool`, `abap_boolean`,
`xsdboolean`, `flag` and `xfeld` become a JSON boolean. A flag you declared as
`c LENGTH 1` looks identical in the debugger and travels as the **string**
`"X"` — a `CheckBox` bound to it stays unchecked, because `"X"` is not `true`.
Type the attribute `abap_bool` and the problem disappears; that is the fix, not
an expression binding.
:::

An expression binding (`{= ${/MV_FLAG} === 'X' }`) is the wrong tool here twice
over: it compares against a value that is not on the wire, and an expression
binding cannot write back, so the box would not flip the attribute even if the
comparison held.

::: tip A boolean written into the view is a different question
All of the above is about a **bound** value. An ABAP boolean put straight into
the XML as an attribute value — `)->a( n = `visible` v = flag )` — is
stringified, and UI5 reads any non-empty string as true, so `abap_false`
renders the control **visible**. Use the builder's boolean parameter for that:
`)->a( n = `visible` b = flag )`. The linter rule
[`unconverted-abap-boolean`](/advanced/linter) catches it.
:::

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

## The formatters abap2UI5 ships

A UI5 `type` covers the `value` property, which is a string. It does not cover
a property that wants a **JavaScript `Date` object** — `DatePicker.dateValue`,
`PlanningCalendarAppointment.startDate` — and JSON has no date type, so the
model physically cannot carry one. For that, and for one text case, the
framework ships a small curated formatter module. It is a public contract:
`z2ui5/model/formatter`, also published as the `z2ui5.Formatter` global for
releases without `core:require`.

Load it once on the view root, then name a helper in the binding string:

```abap
view->a( n = `core:require` v = `{Formatter: 'z2ui5/model/formatter'}` ).

...

)->tag( `DatePicker`
    )->a( n = `dateValue` v = |\{ path: '{ client->_bind( val = mv_date path = abap_true ) }',
                                 formatter: 'Formatter.DateAbapDateToDateObject' \}|
```

| Helper | Takes | Returns |
|---|---|---|
| `DateAbapDateToDateObject` | an ABAP `d` on the wire (`YYYYMMDD`) | a `Date` at midnight local time |
| `DateAbapDateTimeToDateObject` | an ABAP `d` and `t` as two `parts` (the `t` may be omitted → midnight) | a `Date` with the time applied |
| `DateCreateObject` | anything the JS `Date` constructor parses (an ISO string, `utclong`) | a `Date` |
| `expandInlineIcons` | a formatted-text string carrying `%%icon:sap-icon://<name>%%` placeholders | the same text with the theme's icon glyphs inlined — for a `MessageStrip` text |

An **initial or empty** value yields `null`, never an `Invalid Date`. That
matters more than it sounds: an Invalid Date is truthy, so a control accepts it
and fails much later — a `sap.ui.unified` calendar throws for every rendered
day and takes the whole view down. `null` is what "no date" means to a UI5 date
property, so an optional date field in a bound row stays empty instead.

The module grows through framework pull requests only; a helper of your own
goes in your own module and is required the same way. See
`Z2UI5_CL_SMP_APP_457` (DatePicker) and `Z2UI5_CL_SMP_APP_456`
(PlanningCalendar) in the [samples repository](https://github.com/abap2UI5/samples).

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

  PROTECTED SECTION.
  PRIVATE SECTION.
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
