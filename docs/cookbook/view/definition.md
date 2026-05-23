---
outline: [2, 4]
---
# Definition

In abap2UI5, UI5 renders the UI from an XML view that you build in ABAP. A simple example with raw XML:

```abap
  METHOD z2ui5_if_app~main.

    client->view_display(
        |<mvc:View xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:mvc="sap.ui.core.mvc" | &
        |          displayBlock="true" height="100%">| &
        |  <Shell>| &
        |     <Page title="My title">| &
        |         <Text text="My text"/>| &
        |     </Page>| &
        |  </Shell>| &
        |</mvc:View>| ).

ENDMETHOD.
```
You can use any UI5 control from the [UI5 SDK](https://sapui5.hana.ondemand.com). But writing raw XML quickly turns cumbersome. A handier approach is the `Z2UI5_CL_XML_VIEW` helper class, with a fluent API for building views. The `stringify( )` method at the end converts the view tree into an XML string that the framework sends to the frontend:

```abap
  METHOD z2ui5_if_app~main.

    client->view_display(
        z2ui5_cl_xml_view=>factory(
            )->shell(
                )->page( `My title`
                    )->text( `My text`
        )->stringify( ) ).

  ENDMETHOD.
```

Tips for working with views:
- Use code completion on `Z2UI5_CL_XML_VIEW` to find controls and properties
- See the [samples repository](/get_started/samples) for ready-made XML examples to copy and adapt

::: warning Respect the UI5 Control Aggregation Rules
`Z2UI5_CL_XML_VIEW` is intentionally permissive — its fluent API lets you nest **any** control inside **any** other control. UI5 itself is not. Every UI5 control defines specific aggregations (e.g. `sap.m.Page` has `content`, `headerContent`, `footer`) and each aggregation accepts only certain child control types (often a particular interface or base class).

Combining controls in a way that violates these rules can lead to broken rendering, missing controls, layout glitches, runtime errors in the browser console, or subtle bugs that only show up on certain devices or themes.

**Always check the [UI5 SDK](https://sapui5.hana.ondemand.com) for each control** to confirm:
- which aggregations it exposes,
- which child types those aggregations accept, and
- which parent controls are valid for the control you want to use.

The ABAP compiler cannot catch these mistakes — they are pure UI5 concerns and must be verified against the SDK documentation.
:::

## Mapping UI5 XML ↔ ABAP Fluent API

`Z2UI5_CL_XML_VIEW` is a hand-curated wrapper around UI5 controls. It does not cover every UI5 control, and the naming is not always a strict 1:1 mapping of the UI5 SDK:

- Control names follow snake_case of the UI5 control class — `sap.m.MultiComboBox` becomes `->multi_combo_box( )`, `sap.m.Text` becomes `->text( )`.
- Properties are passed as named parameters in snake_case (`enabled`, `placeholder`, `selected_key`).
- Aggregations are exposed as methods named after the aggregation itself (`->items( )`, `->content( )`, `->custom_data( )`) — not as `add_item( )` / `add_content( )`. Calling the aggregation method returns the parent builder so you can chain children inside it.
- Coverage is incomplete and occasionally inconsistent: some controls or properties are missing, some method signatures don't match the SDK exactly. When in doubt, check the source of `Z2UI5_CL_XML_VIEW` or fall back to the generic builder below.

### The Fully Generic Builder

Every fluent helper ultimately produces an XML element. The lowest-level method `_generic( name = ... ns = ... )` lets you emit **any** XML element with **any** attributes, regardless of whether `Z2UI5_CL_XML_VIEW` knows the control:

```abap
  DATA(view) = z2ui5_cl_xml_view=>factory( ).

  view->_generic( name = `Page` ns = `sap.m`
    )->_generic( name = `MultiComboBox` ns = `sap.m`
        t_prop = VALUE #(
          ( n = `selectedKeys` v = `{/keys}` )
          ( n = `placeholder`  v = `Pick one` )
        )
      )->_generic( name = `items` ns = `sap.m`
        )->_generic( name = `core:Item` ns = `sap.ui.core`
            t_prop = VALUE #(
              ( n = `key`  v = `{key}` )
              ( n = `text` v = `{text}` )
            ) ).

  client->view_display( view->stringify( ) ).
```

This maps **1:1** to the UI5 XML/SDK API — control names, property names, and aggregation names are written exactly as they appear in the [UI5 SDK](https://sapui5.hana.ondemand.com), in their original camelCase. There is no abstraction layer guessing what to call things.

::: tip Recommended for AI-assisted Development
For AI-assisted coding, prefer the generic builder. It removes the need for the model to know the abap2UI5 naming conventions or guess which controls and properties are wrapped — the SDK documentation can be used directly. The fluent API in `Z2UI5_CL_XML_VIEW` is convenient for humans but its inconsistencies and incomplete coverage make it a poor target for code generation.
:::

The rest of this documentation uses the higher-level fluent API (`->page( )`, `->button( )`, `->multi_combo_box( )`) because it reads better in examples. Both styles can be mixed freely in the same view.

#### Next Steps
This produces a static view. The next section walks through binding and sharing data between the view and the app logic.
