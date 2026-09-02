---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_027
---
# Expression Binding

Expression Binding lets you compute values directly in XML views with JavaScript-like expressions. This is especially handy in abap2UI5, since it cuts server roundtrips by moving calculations, logical conditions, and string operations to the frontend.

The syntax `{= ... }` marks a UI5 expression binding. Inside the expression, you can use JavaScript operators (like `===` for strict equality or `Math.max`) and reference model properties with `$` followed by a binding path. Note: `===` is the JavaScript strict equality operator (not an ABAP operator) — UI5 needs it because these expressions evaluate in the browser.

### Calculate the Maximum Value on the Frontend

The inputs use a UI5 type binding (`{ type: ..., path: "..." }`) for integer validation. The third input uses an expression binding (`{= ... }`) to compute the maximum of both values directly in the browser. What the ABAP string concatenation produces at runtime:

| ABAP code | UI5 binding result |
|---|---|
| `client->_bind( val = input31 path = abap_true )` | `/INPUT31` (raw path for type binding) |
| `client->_bind( input31 )` | `{/INPUT31}` (full binding for expression) |
| `` `{= Math.max($` && client->_bind( input31 ) && `, $` && client->_bind( input32 ) && `) }` `` | `{= Math.max(${/INPUT31}, ${/INPUT32}) }` |

```abap
CLASS z2ui5_cl_demo_app_max_val DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA input31 TYPE i.
    DATA input32 TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_app_max_val IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->tag( `Label`
                        )->a( n = `text` v = `max value of the first two inputs`

                    " UI5 type binding — validates integer input
                    " resolves to: { type: "sap.ui.model.type.Integer", path: "/INPUT31" }
                    )->tag( `Input`
                        )->a( n = `value` v = `{ type : "sap.ui.model.type.Integer",` &&
                                              `  path:"` && client->_bind( val  = input31
                                                                           path = abap_true ) && `" }`
                    )->tag( `Input`
                        )->a( n = `value` v = `{ type : "sap.ui.model.type.Integer",` && |\n| &&
                                              `  path:"` && client->_bind( val  = input32
                                                                           path = abap_true ) && `" }`

                    " Expression binding — computed in the browser
                    " resolves to: {= Math.max(${/INPUT31}, ${/INPUT32}) }
                    )->tag( `Input`
                        )->a( n = `value`   v = `{= Math.max($` && client->_bind( input31 ) && `, $` && client->_bind( input32 ) && `) }`
                        )->a( n = `enabled` b = abap_false ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Conditionally Set Input Field Editability

The `enabled` property uses an expression binding that resolves to `{= 500===${/QUANTITY} }` — the product field becomes editable only when the quantity equals 500 exactly. Note that `===` is the JavaScript strict equality operator.

```abap
CLASS z2ui5_cl_demo_editable DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA quantity TYPE i.
    DATA product TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_editable IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->tag( `Label`
                        )->a( n = `text` v = `only enabled when the quantity equals 500`

                    )->tag( `Input`
                        )->a( n = `value` v = `{ type : "sap.ui.model.type.Integer",` &&
                                              `  path:"` && client->_bind( val  = quantity
                                                                           path = abap_true ) && `"  }`

                    " enabled resolves to: {= 500===${/QUANTITY} }
                    )->tag( `Input`
                        )->a( n = `value`   v = client->_bind( product )
                        )->a( n = `enabled` v = `{= 500===$` && client->_bind( quantity ) && ` }` ).

    client->view_display( view->stringify( ) ).


  ENDMETHOD.
ENDCLASS.
```

For all options, see the sample class `Z2UI5_CL_SMP_APP_027` or the [UI5 docs on expression binding](https://sapui5.hana.ondemand.com/sdk/#/topic/daf6852a04b44d118963968a1239d2c0).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Expression Binding, Types and Composite Parts | [`Z2UI5_CL_SMP_APP_027`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_027.clas.abap) |

<!-- samples:end -->
