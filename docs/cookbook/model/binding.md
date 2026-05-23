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
ABAP and UI5 do not share a type system. When ABAP values cross to the frontend they are serialized to JSON and then read by UI5 controls — and the exact coercion rules for `i`, `n`, `d`, `p`, `t`, packed numbers with decimals, etc. into the inputs UI5 expects (`string`, `number`, `Date`, …) are **not formally documented**.

In practice this means:
- A `p` field with `DECIMALS 2` may arrive at an `Input` as a plain string, with locale formatting handled (or not) depending on the control.
- A `d` field is sent as an 8-character string, not as an ISO date — a `DatePicker` typically needs a [Formatter](/cookbook/model/formatter) to display and parse it correctly.
- Numeric (`i`, `n`) values round-trip as strings; whether an `Input` returns them as text or as a number depends on the control's `type` attribute.

When the displayed or returned value looks wrong, the fix is almost always a UI5-side `type` (e.g. `sap.ui.model.type.Date`, `sap.ui.model.type.Float`) or an abap2UI5 [Formatter](/cookbook/model/formatter). Verify behavior in the browser with the actual control rather than assuming the default coercion is correct.
:::
