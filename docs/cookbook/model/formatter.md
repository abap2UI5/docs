---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_067
  - z2ui5_cl_smp_app_450
  - z2ui5_cl_smp_app_457
  - z2ui5_cl_smp_app_456
  - z2ui5_cl_smp_app_466
  - z2ui5_cl_smp_app_453
description: UI5 types and formatters in abap2UI5 - the binding-string pattern for currency, digit sequence, date, time, boolean and timestamp values, and the formatter module the framework ships.
---
# Formatter

You can format values like currencies, numerics, dates, or booleans directly on the frontend with UI5 type formatters.

UI5 formatter types use a special JSON-based binding syntax with these key elements:
- **`parts: [...]`** — lists the model paths used as input (e.g., amount + currency)
- **`type: '...'`** — the UI5 formatter type (e.g., `sap.ui.model.type.Currency`)
- **`formatOptions: {...}`** — optional settings that control the output format
- **`constraints: {...}`** — optional constraints applied when user input is parsed back

Note on ABAP syntax: inside string templates (`|...|`), escape the curly braces as `\{` and `\}`, because `{ }` normally denotes an embedded ABAP expression.

`client->_bind_path( )` returns only the raw model path rather than the full binding expression (it is `_bind( val = … path = abap_true )` under a readable name — see [Binding](/cookbook/model/binding#bind-path)), so you can embed it inside the `parts` array or a single-path `path:` entry. The path is e.g. `/AMOUNT`, **unquoted** — so the quotes around it are yours to write, and they have to be `'` or `"`. A backtick is an ABAP string delimiter, not a UI5 one: written here it survives into the binding string, and UI5 answers with a syntax error and renders nothing at all.

For example, this ABAP code:
```abap
|\{ parts: ['{ client->_bind_path( amount ) }'], type: 'sap.ui.model.type.Currency' \}|
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
    )->a( n = `value` v = |\{ parts: [ '{ client->_bind_path( amount ) }',|
                       && | '{ client->_bind_path( currency ) }' ],|
                       && | type: 'sap.ui.model.type.Currency' \}|
```

Common `formatOptions`:
- `showMeasure: false` — hides the currency, shows only the amount
- `showNumber: false` — hides the amount, shows only the currency (UI5 1.89 and later)
- `preserveDecimals: false` — rounds the amount to the currency's decimals; by default the type keeps any further decimals (UI5 1.89 and later — before, it always rounds)
- `currencyCode: false` — shows the currency symbol (`€`) instead of the ISO code (`EUR`), where the currency has one
- `style: 'short'` — compact notation (`123M`); `'long'` formats a currency the same way

The [Full Worked Example](#full-worked-example) below demonstrates each of these variants in a single app.

## Digit Sequence

ABAP `n LENGTH n` is sent as a digit string, leading zeros included. Without a type the zeros render literally. Use the OData `String` type with `isDigitSequence: true`:

```abap
)->tag( `Text`
    )->a( n = `text` v = |\{ path: '{ client->_bind_path( numeric ) }',|
                      && | type: 'sap.ui.model.odata.type.String',|
                      && | constraints: \{ isDigitSequence: true \} \}|
```

This strips the leading zeros for display and re-pads them on write-back.

## Date

ABAP `d` goes on the wire as an ISO date string, `2024-01-15` (an initial date as an empty string), and the framework reads the same shape back. For explicit locale or pattern control, use `sap.ui.model.type.Date` with a `source` pattern that matches the wire format:

```abap
)->tag( `DatePicker`
    )->a( n = `value` v = |\{ path: '{ client->_bind_path( mv_date ) }',|
                       && | type: 'sap.ui.model.type.Date',|
                       && | formatOptions: \{ pattern: 'dd.MM.yyyy',|
                       && | source: \{ pattern: 'yyyy-MM-dd' \} \} \}|
```

`source.pattern` is the wire format (ABAP side); the outer `pattern` is what the user sees.

## Time

ABAP `t` goes on the wire as `12:30:00` (an initial time as an empty string). Same pattern as Date, with `sap.ui.model.type.Time`:

```abap
)->tag( `TimePicker`
    )->a( n = `value` v = |\{ path: '{ client->_bind_path( mv_time ) }',|
                       && | type: 'sap.ui.model.type.Time',|
                       && | formatOptions: \{ pattern: 'HH:mm',|
                       && | source: \{ pattern: 'HH:mm:ss' \} \} \}|
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

An expression binding (`{= ${/MV_FLAG} === 'X' }`) is the wrong tool here for
two reasons. It compares against a value that is not sent to the browser. And
an expression binding cannot write back, so the box would not change the
attribute even if the comparison held.

::: tip A boolean written into the view is a different question
All of the above is about a **bound** value. It is different for an ABAP
boolean put straight into the XML as an attribute value:
`)->a( n = `visible` v = xsdbool( … ) )`. It is turned into the string `X` or
into an empty string. (A bare `v = flag` does not even compile, because `v` is
a string.) UI5 reads a boolean attribute as true only when it says `true`. On
1.71 `abap_true` therefore renders the control **hidden**. A newer release
logs `X` as invalid and ignores it, which leaves the property at its default.
Use the builder's boolean parameter for that:
`)->a( n = `visible` b = flag )`. The linter rule
[`unconverted-abap-boolean`](/advanced/linter) catches it.
:::

## Timestamp

`timestamp` and `timestampl` go on the wire as an ISO string in UTC, `2024-01-15T12:30:00Z` (`timestampl` with its fraction), and the framework reads that shape back. For a property that wants a `Date` object, `Formatter.DateCreateObject` reads the string as it is — see [the formatters abap2UI5 ships](#the-formatters-abap2ui5-ships). For an editable value, two practical approaches:

**Split in ABAP** — break the timestamp into separate `d` and `t` fields before sending, bind each with the [Date](#date) / [Time](#time) formatter above, recombine after the event. Simplest when the UI shows date and time as separate fields anyway.

**Send as string with a source pattern** — convert to a `string` attribute in `yyyyMMddHHmmss` format on the ABAP side, then bind with `sap.ui.model.type.DateTime`:
```abap
)->tag( `DateTimePicker`
    )->a( n = `value` v = |\{ path: '{ client->_bind_path( mv_ts_string ) }',|
                       && | type: 'sap.ui.model.type.DateTime',|
                       && | formatOptions: \{ pattern: 'yyyy-MM-dd HH:mm:ss',|
                       && | source: \{ pattern: 'yyyyMMddHHmmss' \} \} \}|
```
Conversion happens in ABAP (`ts_string = |{ timestamp }|` yields those 14 digits for a `timestamp`); the framework moves the string verbatim.

A custom JS formatter is the third option when neither fits.

## The formatters abap2UI5 ships

A UI5 `type` covers the `value` property, which is a string. It does not cover
a property that wants a **JavaScript `Date` object** — `DatePicker.dateValue`,
`PlanningCalendarAppointment.startDate` — and JSON has no date type, so the
model physically cannot carry one. For that, and for one text case, the
framework ships a small curated formatter module. It is a public contract:
`z2ui5/model/formatter`, reached through `core:require`, which every UI5
release abap2UI5 supports understands, 1.71 included. It is the only formatter
module the frontend has; what it replaced is on
[Deprecations](/resources/deprecations#the-z2ui5-frontend-global).

Load it once on the view root, then name a helper in the binding string:

```abap
view->a( n = `core:require` v = `{Formatter: 'z2ui5/model/formatter'}` ).

...

)->tag( `DatePicker`
    )->a( n = `dateValue` v = |\{ path: '{ client->_bind_path( mv_date ) }',|
                           && | formatter: 'Formatter.DateCreateObject' \}|
```

`mv_date` is a `d`, so it arrives as `2024-01-15`, which `DateCreateObject`
reads. The `DateAbapDate…` helpers are for a date held as an eight-digit
string instead — a `c LENGTH 8` key or a legacy structure field. The
JavaScript `Date` constructor reads a bare ISO date as midnight UTC, so west
of Greenwich the picker shows the day before; where that matters, bind the
`value` with the [Date](#date) type instead.

| Helper | Takes | Returns |
|---|---|---|
| `DateAbapDateToDateObject` | an ABAP date held as an eight-digit string (`20240115`) — not a `d` attribute, which arrives as `2024-01-15` | a `Date` at midnight local time |
| `DateAbapDateTimeToDateObject` | the same date string and a six-digit time string (`123000`) as two `parts` (the time may be omitted → midnight) | a `Date` with the time applied |
| `DateCreateObject` | anything the JS `Date` constructor parses (an ISO string: a `d`, `timestamp` or `utclong` attribute) | a `Date` |
| `expandInlineIcons` | a formatted-text string carrying `%%icon:sap-icon://<name>%%` placeholders | the same text with the theme's icon glyphs inlined — for a `MessageStrip` text |

An **initial or empty** value yields `null`, never an `Invalid Date`. This
is important. An Invalid Date counts as a value, so a control accepts it and
fails much later: a `sap.ui.unified` calendar throws for every rendered day
and takes the whole view down. To a UI5 date property, `null` means "no
date", so an optional date field in a bound row simply stays empty.

The module grows through framework pull requests only; a helper of your own
goes in your own module and is required the same way. See
`Z2UI5_CL_SMP_APP_457` (DatePicker) and `Z2UI5_CL_SMP_APP_456`
(PlanningCalendar) in the [samples repository](https://github.com/abap2UI5/samples).

## Full Worked Example

The class below combines the Currency and Digit Sequence patterns in one app.
It shows an amount as an editable currency field, the same amount without its
currency code, and a number without its leading zeros:

```abap
CLASS zcl_sample_currency DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA amount   TYPE p LENGTH 14 DECIMALS 3.
    DATA currency TYPE string.
    DATA numeric  TYPE n LENGTH 12.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_currency IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      " the branch runs again on every return, so set the values only once
      IF currency IS INITIAL.
        numeric  = `000000000012`.
        amount   = `123456789.123`.
        currency = `USD`.
      ENDIF.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `abap2UI5 - Currency Format`

                  )->tag( `Input`
                      )->a( n = `value` v = |\{ parts: [ '{ client->_bind_path( amount ) }',|
                                         && | '{ client->_bind_path( currency ) }' ],|
                                         && | type: 'sap.ui.model.type.Currency' \}|
                  )->tag( `Text`
                      )->a( n = `text` v = |\{ parts: [ '{ client->_bind_path( amount ) }',|
                                        && | '{ client->_bind_path( currency ) }' ],|
                                        && | type: 'sap.ui.model.type.Currency',|
                                        && | formatOptions: \{ showMeasure: false \} \}|
                  )->tag( `Text`
                      )->a( n = `text` v = |\{ path: '{ client->_bind_path( numeric ) }',|
                                        && | type: 'sap.ui.model.odata.type.String',|
                                        && | constraints: \{ isDigitSequence: true \} \}|
                  )->tag( `Button`
                      )->a( n = `text`  v = `send`
                      )->a( n = `press` v = client->_event( `BUTTON` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `BUTTON` ).
      " the edited values travel to the server and come back, and every
      " formatter renders them again from the model
      client->message_toast_display( |Amount { amount }, currency { currency }| ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

Sample `Z2UI5_CL_SMP_APP_067` in the
[samples repository](https://github.com/abap2UI5/samples) is the long version:
it shows every `formatOptions` variant listed under [Currency](#currency) side
by side.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Currency Amounts (sap.ui.model.type.Currency) | [`Z2UI5_CL_SMP_APP_067`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_067.clas.abap) |
| ABAP Date and Time Strings (DATS/TIMS) | [`Z2UI5_CL_SMP_APP_450`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_450.clas.abap) |
| Date Object for the DatePicker | [`Z2UI5_CL_SMP_APP_457`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_457.clas.abap) |
| Date Objects for the PlanningCalendar | [`Z2UI5_CL_SMP_APP_456`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_456.clas.abap) |
| Inline Icons in a Text | [`Z2UI5_CL_SMP_APP_466`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_466.clas.abap) |
| When Not to Use One: Compute in ABAP | [`Z2UI5_CL_SMP_APP_453`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_453.clas.abap) |

<!-- samples:end -->
