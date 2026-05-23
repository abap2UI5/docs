---
outline: [2, 4]
---
# Formatter

You can format values like currencies, numerics, dates, or booleans directly on the frontend with UI5 type formatters.

UI5 formatter types use a special JSON-based binding syntax with these key elements:
- **`parts: [...]`** — lists the model paths used as input (e.g., amount + currency)
- **`type: '...'`** — the UI5 formatter type (e.g., `sap.ui.model.type.Currency`)
- **`formatOptions: {...}`** — optional settings that control the output format
- **`constraints: {...}`** — optional input constraints for two-way binding
- **`\{ ... \}`** — in ABAP string templates (`|...|`), escape curly braces with `\` because `{ }` normally denotes an ABAP expression

The `path = abap_true` parameter on `_bind` / `_bind_edit` returns only the raw model path (e.g., `/XX/AMOUNT`) rather than the full binding expression (`{/XX/AMOUNT}`), so you can embed it inside the `parts` array or a single-path `path:` entry.

For example, this ABAP code:
```abap
|\{ parts: [`{ client->_bind_edit( val = amount path = abap_true ) }`], type: 'sap.ui.model.type.Currency' \}|
```
produces this UI5 binding string at runtime:
```text
{ parts: ["/XX/AMOUNT"], type: 'sap.ui.model.type.Currency' }
```

The sections below show the binding-string pattern for each ABAP type that needs a formatter. Each pattern is the minimum that makes the value display and parse correctly — for runnable apps with full `formatOptions`, `constraints`, and read-only variants, see the [samples repository](https://github.com/abap2UI5/samples).

## Currency

ABAP `p LENGTH n DECIMALS m` + a `c LENGTH 3` currency code → UI5 `Currency` formatter. Two `parts` entries; the type combines them into a locale-aware amount string:

```abap
)->input(
    |\{ parts: [ `{ client->_bind_edit( val = amount   path = abap_true ) }`,
                 `{ client->_bind_edit( val = currency path = abap_true ) }` ],
        type: 'sap.ui.model.type.Currency' \}| )
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
)->text(
    |\{ path: `{ client->_bind_edit( val = numeric path = abap_true ) }`,
        type: 'sap.ui.model.odata.type.String',
        constraints: \{ isDigitSequence: true \} \}| )
```

This strips the leading zeros for display and re-pads them on write-back.

## Date

ABAP `d` is an 8-character string `YYYYMMDD`. `date_picker` accepts it directly via `client->_bind_edit( mv_date )` for the default case. For explicit locale or pattern control, use `sap.ui.model.type.Date` with a `source` pattern that matches the wire format:

```abap
)->date_picker(
    value = |\{ path: `{ client->_bind_edit( val = mv_date path = abap_true ) }`,
                type: 'sap.ui.model.type.Date',
                formatOptions: \{ pattern: 'yyyy-MM-dd',
                                  source: \{ pattern: 'yyyyMMdd' \} \} \}| )
```

`source.pattern` is the wire format (ABAP side); the outer `pattern` is what the user sees.

## Time

ABAP `t` is a 6-character string `HHMMSS`. Same pattern as Date, with `sap.ui.model.type.Time`:

```abap
)->time_picker(
    value = |\{ path: `{ client->_bind_edit( val = mv_time path = abap_true ) }`,
                type: 'sap.ui.model.type.Time',
                formatOptions: \{ pattern: 'HH:mm:ss',
                                  source: \{ pattern: 'HHmmss' \} \} \}| )
```

## Boolean

ABAP `abap_bool` is `"X"` or `""`. UI5's `CheckBox` expects `true` / `false`. Two practical bridges:

**Expression binding** — compare the bound value to `'X'` inline. Read-only:
```abap
)->checkbox( selected = `{= $` && client->_bind( mv_flag ) && ` === 'X' }` )
```
This resolves to `{= ${/XX/MV_FLAG} === 'X' }`. Note that expression bindings cannot write back — checking the box will not flip the ABAP attribute.

**ABAP-side conversion** — keep a parallel `string`-typed attribute (`'true'` / `'false'`) for two-way binding, and translate before/after each event:
```abap
DATA flag_bool TYPE abap_bool.
DATA flag_str  TYPE string.   " 'true' / 'false' for the checkbox

" before view_display:
flag_str = COND #( WHEN flag_bool = abap_true THEN 'true' ELSE 'false' ).

" after the event:
flag_bool = COND #( WHEN flag_str = 'true' THEN abap_true ELSE abap_false ).
```
Then `)->checkbox( selected = client->_bind_edit( flag_str ) )` works both directions. More boilerplate in the controller, simpler view.

A custom JS formatter wired through `sap.ui.model.SimpleType` is the third option — see the [samples repository](https://github.com/abap2UI5/samples).

## Timestamp

`timestamp` and `timestampl` are packed numbers with no built-in UI5 type that reads them directly. Two practical approaches:

**Split in ABAP** — break the timestamp into separate `d` and `t` fields before sending, bind each with the [Date](#date) / [Time](#time) formatter above, recombine after the event. Simplest when the UI shows date and time as separate fields anyway.

**Send as string with a source pattern** — convert to a string in `yyyyMMddHHmmss` format on the ABAP side, then bind with `sap.ui.model.type.DateTime`:
```abap
)->date_time_picker(
    value = |\{ path: `{ client->_bind_edit( val = mv_ts_string path = abap_true ) }`,
                type: 'sap.ui.model.type.DateTime',
                formatOptions: \{ pattern: 'yyyy-MM-dd HH:mm:ss',
                                  source: \{ pattern: 'yyyyMMddHHmmss' \} \} \}| )
```
Conversion happens in ABAP (`WRITE timestamp TO ts_string …` or a helper); the framework moves the string verbatim.

A custom JS formatter is the third option when neither fits.

## Full Worked Example

The class below combines the Currency and Digit Sequence patterns in one app and demonstrates every `formatOptions` variant listed under [Currency](#currency):

```abap

CLASS z2ui5_cl_demo_app_067 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA amount            TYPE p LENGTH 14 DECIMALS 3.
    DATA currency          TYPE string.
    DATA numeric           TYPE n LENGTH 12.
    DATA check_initialized TYPE abap_bool.

ENDCLASS.

CLASS z2ui5_cl_demo_app_067 IMPLEMENTATION.

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
    ENDCASE.

    DATA(page) = z2ui5_cl_xml_view=>factory( )->shell(
         )->page( title          = `abap2UI5 - Currency Format`
                  navbuttonpress = client->_event( |BACK| )
                  shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL  ) ).

    page->simple_form( title    = `Currency`
                       editable = abap_true
     )->content( `form`
         )->title( `Input`
         )->label( `Documentation`
         )->link( text = `https://sapui5.hana.ondemand.com/#/entity/sap.ui.model.type.Currency`
                  href = `https://sapui5.hana.ondemand.com/#/entity/sap.ui.model.type.Currency`

         "Currency in one field — shows amount and currency symbol together
         "resolves to: { parts: ["/XX/AMOUNT", "/XX/CURRENCY"], type: 'sap.ui.model.type.Currency' }
         )->label( `One field`
         )->input(
             |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                 path = abap_true ) }`, `{ client->_bind_edit(
                                                                               val  = currency
                                                                               path = abap_true ) }`],  type: 'sap.ui.model.type.Currency' \}|

         "Split into two fields — first shows only the number, second only the currency
         "showMeasure: false → hides the currency symbol
         "showNumber: false  → hides the numeric value
         )->label( `Two field`
         )->input(
             |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                 path = abap_true ) }`, `{ client->_bind_edit(
                                                                               val  = currency
                                                                               path = abap_true ) }`],  type: 'sap.ui.model.type.Currency' , formatOptions: \{showMeasure: false\}  \}|
         )->input(
             |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                 path = abap_true ) }`, `{ client->_bind_edit(
                                                                               val  = currency
                                                                               path = abap_true ) }`],  type: 'sap.ui.model.type.Currency' , formatOptions: \{showNumber: false\} \}|

         "Read-only display variants with different formatOptions
         )->label( `Default`
         )->text(
             |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                 path = abap_true ) }`, `{ client->_bind_edit(
                                                                               val  = currency
                                                                               path = abap_true ) }`],  type: 'sap.ui.model.type.Currency' \}|

         "preserveDecimals: false → trims trailing zeros (e.g. 123,456,789.12 USD)
         )->label( `preserveDecimals:false`
         )->text( |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                      path = abap_true ) }`, `| && client->_bind_edit(
                                                                                       val  = currency
                                                                                       path = abap_true ) &&
                     |`],  type: 'sap.ui.model.type.Currency' , formatOptions: \{ preserveDecimals : false \} \}|

         "currencyCode: false → hides the ISO code, shows only the number
         )->label( `currencyCode:false`
         )->text( |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                      path = abap_true ) }`, `| && client->_bind_edit(
                                                                                       val  = currency
                                                                                       path = abap_true ) &&
                         |`],  type: 'sap.ui.model.type.Currency' , formatOptions: \{ currencyCode : false \} \}|

         "style: 'short' → compact notation (e.g. 123M USD)
         )->label( `style:'short'`
         )->text(
             |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                 path = abap_true ) }`, `{ client->_bind_edit(
                                                                               val  = currency
                                                                               path = abap_true ) }`],  type: 'sap.ui.model.type.Currency' , formatOptions: \{ style : 'short' \} \}|

         "style: 'long' → full text notation (e.g. 123 million US dollars)
         )->label( `style:'long'`
         )->text(
             |\{ parts: [ `{ client->_bind_edit( val  = amount
                                                 path = abap_true ) }`, `{ client->_bind_edit(
                                                                               val  = currency
                                                                               path = abap_true ) }`],  type: 'sap.ui.model.type.Currency' , formatOptions: \{   style : 'long' \} \}|
         )->label( `event`
         )->button( text  = `send`
                    press = client->_event( `BUTTON` ) ).

    "Remove leading zeros from a numeric string with OData type formatting
    "isDigitSequence: true tells the formatter to treat the value as a digit sequence
    "resolves to: { path: "/XX/NUMERIC", type: 'sap.ui.model.odata.type.String', constraints: { isDigitSequence: true } }
    page->simple_form( title    = `No Zeros`
                       editable = abap_true
        )->content( `form`
        )->title( `Input`
        )->label( `Documentation`
        )->link( text = `https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.model.odata.type.String%23methods/formatValue`
                 href = `https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.model.odata.type.String%23methods/formatValue`
        )->label( `Numeric`
        )->input( value = client->_bind_edit( val = numeric )
        )->label( `Without leading Zeros`
        )->text(
    text = |\{path : `{ client->_bind_edit(
                            val  = numeric
                            path = abap_true ) }`, type : 'sap.ui.model.odata.type.String', constraints : \{  isDigitSequence : true \} \}| ).

    client->view_display( page->stringify( ) ).

  ENDMETHOD.

ENDCLASS.

```

For a full runnable copy, see the sample implementation in class `Z2UI5_CL_DEMO_APP_067` in the [samples repository](https://github.com/abap2UI5/samples).
