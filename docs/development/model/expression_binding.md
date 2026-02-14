# Expression Binding

Expression Binding lets you compute values directly in XML views using JavaScript-like expressions. This is especially useful in abap2UI5, as it can reduce server roundtrips by moving calculations, logical conditions, and string operations to the frontend.

The syntax `{= ... }` marks a UI5 expression binding. Inside the expression, you can use JavaScript operators (like `===` for strict equality or `Math.max`) and reference model properties with `$` followed by a binding path. Note: `===` is the JavaScript strict equality operator (not an ABAP operator) — it is required because UI5 evaluates these expressions in the browser.

#### Calculate the Maximum Value at the Frontend

The inputs use a UI5 type binding (`{ type: ..., path: "..." }`) to ensure integer validation. The third input uses an expression binding (`{= ... }`) to compute the maximum of both values directly in the browser. Here's what the ABAP string concatenation produces at runtime:

| ABAP code | UI5 binding result |
|---|---|
| `client->_bind( val = input31 path = abap_true )` | `/XX/INPUT31` (raw path for type binding) |
| `client->_bind( input31 )` | `{/XX/INPUT31}` (full binding for expression) |
| `` `{= Math.max($` && client->_bind( input31 ) && `, $` && client->_bind( input32 ) && `) }` `` | `{= Math.max(${/XX/INPUT31}, ${/XX/INPUT32}) }` |

```abap
CLASS z2ui5_cl_demo_app_max_val DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA input31 TYPE i.
    DATA input32 TYPE i.

ENDCLASS.

CLASS z2ui5_cl_demo_app_max_val IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell( )->page(
      )->label( `max value of the first two inputs`
                "UI5 type binding — validates integer input
                "resolves to: { type: "sap.ui.model.type.Integer", path: "/XX/INPUT31" }
                )->input( `{ type : "sap.ui.model.type.Integer",` &&
                          `  path:"` && client->_bind( val  = input31
                                                       path = abap_true ) && `" }`
                )->input( `{ type : "sap.ui.model.type.Integer",` && |\n| &&
                          `  path:"` && client->_bind( val  = input32
                                                       path = abap_true ) && `" }`
                "Expression binding — computed in the browser
                "resolves to: {= Math.max(${/XX/INPUT31}, ${/XX/INPUT32}) }
                )->input(
                    value   = `{= Math.max($` && client->_bind( input31 ) &&`, $` && client->_bind( input32 ) && `) }`
                    enabled = abap_false ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

#### Conditionally Set Input Field Editability

The `enabled` property uses an expression binding that resolves to `{= 500===${/XX/QUANTITY} }` — the product field is only editable when the quantity is exactly 500. Note that `===` is the JavaScript strict equality operator.

```abap
CLASS z2ui5_cl_demo_editable DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA quantity TYPE i.
    DATA product TYPE string.

ENDCLASS.

CLASS z2ui5_cl_demo_editable IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell( )->page(
      )->label( `only enabled when the quantity equals 500`
                )->input( `{ type : "sap.ui.model.type.Integer",` &&
                          `  path:"` && client->_bind( val  = quantity
                                                       path = abap_true ) && `"  }`
                "enabled resolves to: {= 500===${/XX/QUANTITY} }
                )->input(
                    value   = client->_bind_edit( product )
                    enabled = `{= 500===$` && client->_bind( quantity ) && ` }` ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

Explore all the possibilities in the sample class `Z2UI5_CL_DEMO_APP_027` or refer to the UI5 documentation [here.](https://sapui5.hana.ondemand.com/sdk/#/topic/daf6852a04b44d118963968a1239d2c0)
