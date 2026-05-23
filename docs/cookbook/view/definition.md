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

#### Next Steps
This produces a static view. The next section walks through binding and sharing data between the view and the app logic.
